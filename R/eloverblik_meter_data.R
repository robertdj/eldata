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

parse_meter_data <- function(response)
{
    raw_content <- httr::content(response, as = 'raw')

    parsed_content <- RcppSimdJson::fparse(raw_content)

    timeseries_data <- purrr::chuck(parsed_content, "result", "MyEnergyData_MarketDocument", 1, "TimeSeries", "Period", 1)

    raw_meter_data_list <- purrr::chuck(timeseries_data, "Point")

    start_stop_times_list <- purrr::chuck(timeseries_data, "timeInterval")

    meassurement_resolution <- purrr::chuck(timeseries_data, "resolution")

    raw_meter_data <- purrr::pmap_dfr(
        list(raw_meter_data_list, start_stop_times_list, meassurement_resolution),
        function(md, times, res)
        {
            md$StartDay <- times$start
            md$EndDay <- times$end
            md$Resolution <- res

            return(md)
        }
    ) %>%
        tibble::as_tibble()

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

    meter_data <- raw_meter_data %>%
        dplyr::mutate(
            dplyr::across(
                c("StartDay", "EndDay"),
                ~ as.POSIXct(.x, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
            ),
            StartDayCET = lubridate::with_tz(StartDay, "CET"),
            EndTime = clock::add_hours(StartDayCET, as.integer(position)),
            StartTime = clock::add_hours(EndTime, -1),
            Consumption = as.numeric(out_Quantity.quantity),
            Quality = quality[out_Quantity.quality],
            Resolution = resolution[Resolution]
        )

    meter_data %>%
        dplyr::select(
            StartTime,
            EndTime,
            Consumption,
            Quality,
            Resolution
        )
}
