download_meter_data <- function(dateFrom, dateTo, aggregation, meteringpoint, dat)
{
    httr::POST(
        glue::glue("https://api.eloverblik.dk/customerapi/api/meterdata/gettimeseries/{dateFrom}/{dateTo}/{aggregation}"),
        httr::content_type_json(),
        httr::accept_json(),
        httr::add_headers(Authorization = paste('Bearer', dat)),
        body = list(
            "meteringPoints" = list(
                "meteringPoint" = I(meteringpoint)
            )
        ),
        encode = "json"
    )
}

# parse_meter_data <- function(response)
# {
#     raw_content <- httr::content(response, as = 'raw')
#
#     parsed_content <- RcppSimdJson::fparse(raw_content)


parse_meter_data <- function(json_file)
{
    raw_meter_data <- extract_meter_data(json_file)
    munge_meter_data(raw_meter_data)
}


extract_meter_data <- function(json_file)
{
    parsed_content <- RcppSimdJson::fload(json_file)

    timeseries_data <- purrr::chuck(
        parsed_content, "result", "MyEnergyData_MarketDocument", 1, "TimeSeries", "Period", 1
    )

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

    # id <- purrr::chuck(parsed_content, "result", "id")
    # raw_meter_data$MeterId <- id

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

    meter_data <- raw_meter_data |>
        dplyr::mutate(
            dplyr::across(
                c("DayStartUTC", "DayEndUTC"),
                ~ as.POSIXct(.x, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
            ),
            HourOfDay = as.integer(position),
            EndTimeUTC = clock::add_hours(DayStartUTC, HourOfDay),
            StartTimeUTC = clock::add_hours(EndTimeUTC, -1),
            StartTimeCET = lubridate::with_tz(StartTimeUTC, "CET"),
            Date = lubridate::date(StartTimeCET),
            Consumption = as.numeric(out_Quantity.quantity),
            Quality = quality[out_Quantity.quality],
            Resolution = resolution[Resolution]
        )

    meter_data |>
        dplyr::select(
            Date,
            HourOfDay,
            StartTimeUTC,
            EndTimeUTC,
            Consumption,
            Quality,
            Resolution
        )
}


load_meter_data <- function(raw_folder)
{
    json_files <- fs::dir_ls(raw_folder, glob = "*.json")

    raw_meter_data <- purrr::map_dfr(json_files, extract_meter_data)

    munge_meter_data(raw_meter_data)
}
