library(magrittr)

raw_save_dir <- fs::path("data", "raw", "spot_prices")
fs::dir_create(raw_save_dir)

this_month <- lubridate::floor_date(lubridate::today("CET"), unit = "month")
last_month <- clock::add_months(this_month, -1)

spot_prices_input <- tibble::tibble(
    start = seq.Date(from = as.Date("2020-01-01"), to = last_month, by = "month"),
    end = clock::add_months(start, 1),
    filename = fs::path(raw_save_dir, as.character(start, format = "%Y-%m"), ext = "csv")
)

missing_spot_prices <- spot_prices_input %>%
    dplyr::filter(!fs::file_exists(filename))

spot_price_responses <- missing_spot_prices %>%
    dplyr::select(start, end) %>%
    purrr::pmap(
        eldata::download_spot_prices,
        area = "DK1"
    )

is_response_successful <- purrr::map_lgl(
    spot_price_responses,
    ~ httr::status_code(.x) == 200
)

purrr::walk2(
    spot_price_responses[is_response_successful],
    missing_spot_prices$filename[is_response_successful],
    function(response, filename)
    {
        spot_price_data <- parse_spot_prices(response)
        readr::write_delim(spot_price_data, file = filename, delim = ";", progress = FALSE)
    }
)
