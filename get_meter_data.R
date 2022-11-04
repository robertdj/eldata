dotenv::load_dot_env()


# Download data -------------------------------------------------------------------------------

raw_save_dir <- fs::path("data", "raw", "meter_data")
fs::dir_create(raw_save_dir)

this_month <- lubridate::floor_date(lubridate::today("CET"), unit = "month")
last_month <- clock::add_months(this_month, -1)

meter_data_input <- tibble::tibble(
    dateFrom = seq.Date(from = as.Date("2020-01-01"), to = last_month, by = "month"),
    dateTo = clock::add_months(dateFrom, 1),
    filename = fs::path(raw_save_dir, as.character(dateFrom, format = "%Y-%m"), ext = "json")
)

missing_meter_data <- meter_data_input %>%
    dplyr::filter(!fs::file_exists(filename))

if (nrow(missing_meter_data) > 0)
{
    dat <- eldata::get_data_access_token()
}

meter_data_responses <- missing_meter_data %>%
    dplyr::select(dateFrom, dateTo) %>%
    purrr::pmap(
        eldata::download_meter_data,
        aggregation = "Actual", meteringpoint = Sys.getenv("ELOVERBLIK_METERING_POINT"), dat = dat
    )

is_response_successful <- purrr::map_lgl(
    meter_data_responses,
    ~ httr::status_code(.x) == 200
)

purrr::walk2(
    meter_data_responses[is_response_successful],
    missing_meter_data$filename[is_response_successful],
    function(md_response, filename)
    {
        md_json <- httr::content(md_response, as = "text")
        cat(jsonlite::prettify(md_json), file = filename)
    }
)
