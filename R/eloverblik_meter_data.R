#' Download meter data
#'
#' The API is described on a Swagger page: <https://api.eloverblik.dk/CustomerApi/index.html>.
#'
#' @param dateFrom `[Date]` The start date.
#' @param dateTo `[Date]` The end date.
#' @param aggregation `[character(1)]` Time aggregation. Allowed values are "Actual", "Quarter", "Hour", "Day", "Month", "Year".
#' @param metering_points `[character]` A vector of metering points to get data for.
#' @param dat `[character(1)]` Data access token from [get_data_access_token()].
#'
#' @return A `{httr}` response.
#'
#' @export
download_meter_data <- function(dateFrom, dateTo, aggregation, metering_points, dat)
{
    assertthat::assert_that(
        assertthat::is.date(dateFrom),
        assertthat::is.date(dateTo),
        assertthat::is.string(aggregation),
        aggregation %in% c("Actual", "Quarter", "Hour", "Day", "Month", "Year"),
        is.character(metering_points),
        assertthat::is.string(dat)
    )

    httr::POST(
        glue::glue("https://api.eloverblik.dk/customerapi/api/meterdata/gettimeseries/{dateFrom}/{dateTo}/{aggregation}"),
        httr::content_type_json(),
        httr::accept_json(),
        httr::add_headers(Authorization = paste('Bearer', dat)),
        body = list(
            "meteringPoints" = list(
                "meteringPoint" = I(metering_points)
            )
        ),
        encode = "json"
    )
}


#' Parse meter data response
#'
#' Parse meter data into a `tibble`.
#'
#' @param json_file JSON content from a Response from [download_spot_prices].
#'
#' @return A `tibble` with columns `Date`, `HourOfDay`, `StartTimeUTC`, `EndTimeUTC`, `Consumption`, `Quality`, `Resolution`.
#'
#' @details Because the data source is brittle and I only include a subset of the data in the output of this function, I save the raw JSON content first instead of parsing the response.
#'
#' @export
parse_meter_data <- function(json_file)
{
    raw_meter_data <- extract_meter_data(json_file)
    munge_meter_data(raw_meter_data)
}


extract_meter_data <- function(json_file)
{
    assertthat::assert_that(
        assertthat::is.readable(json_file),
        assertthat::has_extension(json_file, "json")
    )

    parsed_content <- RcppSimdJson::fload(json_file)

    timeseries_data <- purrr::chuck(parsed_content, "result", "MyEnergyData_MarketDocument")

    is_timeseries_data_missing <- purrr::map_lgl(timeseries_data, ~ is.null(.x$TimeSeries))
    if (any(is_timeseries_data_missing))
    {
        requested_ids <- purrr::chuck(parsed_content, "result", "id")
        ids_with_missing_data <- requested_ids[is_timeseries_data_missing]
        message("These meters return no data: ", paste(ids_with_missing_data, collapse = ", "))
    }

    timeseries_data |>
        purrr::discard(~ is.null(.x$TimeSeries)) |>
        purrr::map_dfr(extract_meter_data_single_id)
}


extract_meter_data_single_id <- function(single_meter_data_entry)
{
    # Sometimes (e.g. from 2020-03-01 to 2020-04-01) there are *two* elements in 'Period'. One is
    # the requested (hourly consumption). The other is the profiled consumption for Q1.
    # Source: CUSTOMER AND THIRD PARTY API FOR DATAHUB (ELOVERBLIK) - DATA DESCRIPTION
    present_business_types <- purrr::chuck(single_meter_data_entry, "TimeSeries", "businessType")
    measurement_idx <- which(present_business_types %in% c("A01", "A04"))

    if (length(measurement_idx) != 1)
        stop(
            "Business type must be production ('A01') or consumtion ('A04').",
            "Here we have: ", present_business_types
        )

    timeseries_data <- purrr::chuck(single_meter_data_entry, "TimeSeries", "Period", measurement_idx)

    raw_meter_data_list <- purrr::chuck(timeseries_data, "Point")

    start_stop_times_list <- purrr::chuck(timeseries_data, "timeInterval")

    meassurement_resolution <- purrr::chuck(timeseries_data, "resolution")

    raw_meter_data <- purrr::pmap_dfr(
        list(raw_meter_data_list, start_stop_times_list, meassurement_resolution),
        function(md, times, res)
        {
            md$DayStartUTC <- times$start
            md$DayEndUTC <- times$end
            md$Resolution <- res

            return(md)
        }
    ) |>
        tibble::as_tibble()

    id <- purrr::chuck(single_meter_data_entry, "TimeSeries", "mRID", measurement_idx)
    raw_meter_data$MeterId <- id[measurement_idx]

    business_type <- present_business_types[measurement_idx]
    raw_meter_data$BusinessType <- business_type

    measurement_units <- purrr::chuck(single_meter_data_entry, "TimeSeries", "measurement_Unit.name")
    raw_meter_data$Unit <- measurement_units[measurement_idx]

    return(raw_meter_data)
}


munge_meter_data <- function(raw_meter_data)
{
    # From the document "CUSTOMER AND THIRD PARTY API FOR DATAHUB (ELOVERBLIK) - DATA DESCRIPTION"
    resolution <- c(
        "PT15M" = "15 min",
        "PT1H" = "Hour",
        "P1D" = "Day",
        "P1M" = "Month",
        "P1Y" = "Year"
    )

    quality <- c(
        "A01" = "Adjusted",
        "A02" = "Not available",
        "A03" = "Estimated",
        "A04" = "As provided",
        "A05" = "Incomplete"
    )

    business_types <- c(
        "A01" = "Production",
        "A04" = "Consumption",
        "A64" = "Consumption profile"
    )

    if (any(c("P1D", "P1M", "P1Y") %in% raw_meter_data$Resolution))
        warning("Data with resolution coarser than days has not been parsed before.")

    meter_data <- raw_meter_data |>
        dplyr::mutate(
            dplyr::across(
                c("DayStartUTC", "DayEndUTC"),
                ~ as.POSIXct(.x, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
            ),
            Consumption = as.numeric(out_Quantity.quantity),
            Quality = quality[out_Quantity.quality],
            Resolution = resolution[Resolution],
            BusinessType = business_types[BusinessType],
            #
            HourOfDay = dplyr::case_when(
                Resolution == "15 min" ~ as.integer(ceiling(as.double(position) / 4)),
                Resolution == "Hour" ~ as.integer(position)
            ),
            EndTimeUTC = dplyr::case_when(
                Resolution == "15 min" ~ clock::add_minutes(DayStartUTC, as.integer(position) * 15),
                Resolution == "Hour" ~ clock::add_hours(DayStartUTC, HourOfDay)
            ),
            StartTimeUTC = dplyr::case_when(
                Resolution == "15 min" ~ clock::add_minutes(EndTimeUTC, -15),
                Resolution == "Hour" ~ clock::add_hours(EndTimeUTC, -1)
            ),
            StartTimeCET = lubridate::with_tz(StartTimeUTC, "CET"),
            Date = lubridate::date(StartTimeCET),
        )

    meter_data |>
        dplyr::select(
            MeterId,
            Date,
            HourOfDay,
            StartTimeUTC,
            EndTimeUTC,
            Consumption,
            Unit,
            Quality,
            BusinessType,
            Resolution
        )
}


#' Save parsed meter data
#'
#' Save parsed meter data to CSV files -- one file per metering id.
#'
#' @param meter_data Dataframe from [parse_meter_data].
#' @param parsed_folder Folder to store CSV files.
#'
#' @export
save_parsed_meter_data <- function(meter_data, parsed_folder)
{
    meter_data_per_id <- split(meter_data, meter_data$MeterId)

    date_range <- purrr::map(meter_data_per_id, ~ range(as.character(.x[["Date"]]))) |>
        purrr::map_chr(~ paste(.x, collapse = "_"))

    save_names <- fs::path(parsed_folder, names(meter_data_per_id), date_range, ext = "csv")

    fs::dir_create(dirname(save_names))

    purrr::walk2(
        meter_data_per_id, save_names,
        readr::write_delim, delim = ";", progress = FALSE
    )
}


#' Load saved meter data
#'
#' Raw meter data saved in JSON is loaded and parsed.
#'
#' @param parsed_folder Folder with CSV files.
#'
#' @inherit parse_meter_data return
#'
#' @export
load_meter_data <- function(parsed_folder)
{
    csv_files <- fs::dir_ls(parsed_folder, glob = "*.csv")

    meter_data <- readr::read_delim(csv_files, delim = ";")
}
