raw_save_dir <- fs::path("data", "raw", "spot_prices")
fs::dir_create(raw_save_dir)

this_month <- lubridate::floor_date(lubridate::today("CET"), unit = "month")
last_month <- clock::add_months(this_month, -1)

# Get area from metering info file
spot_prices_input <- tibble::tibble(
    start = seq.Date(from = as.Date("2020-01-01"), to = last_month, by = "month"),
    end = clock::add_months(start, 1),
    area = "DK1",
    # filename = fs::path(raw_save_dir, glue::glue("{start}_{end}_{area}"), ext = "csv")
    filename = fs::path(raw_save_dir, glue::glue("{area}_{start}_{end}"), ext = "csv")
)

missing_spot_prices <- spot_prices_input |>
    dplyr::filter(!fs::file_exists(filename))

spot_price_responses <- missing_spot_prices |>
    dplyr::select(start, end, area) |>
    purrr::pmap(eldata::download_spot_prices)

is_response_successful <- purrr::map_lgl(
    spot_price_responses,
    ~ httr::status_code(.x) == 200
)

purrr::walk2(
    spot_price_responses[is_response_successful],
    missing_spot_prices$filename[is_response_successful],
    function(response, filename)
    {
        spot_price_data <- eldata::parse_spot_prices(response)
        readr::write_delim(spot_price_data, file = filename, delim = ";", progress = FALSE)
    }
)


raw_spot_price_files <- fs::dir_ls(raw_save_dir, glob = "*.csv")
spot_price_params <- strsplit(basename(raw_spot_price_files), "_")
spot_price_area <- purrr::map_chr(spot_price_params, 1)

parsed_save_dirs <- fs::path("data", "parsed", "spot_prices", spot_price_area)
fs::dir_create(unique(parsed_save_dirs))

parsed_spot_price_files <- fs::path(parsed_save_dirs, basename(raw_spot_price_files))

fs::file_copy(raw_spot_price_files, parsed_spot_price_files, overwrite = TRUE)
