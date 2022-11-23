dotenv::load_dot_env()


# Download data -------------------------------------------------------------------------------

raw_save_dir <- fs::path("data", "raw", "meter_data")
fs::dir_create(raw_save_dir)

this_month <- lubridate::floor_date(lubridate::today("CET"), unit = "month")
last_month <- clock::add_months(this_month, -1)

metering_points_info <- readr::read_delim(
    fs::path("data", "metering_points.csv"),
    delim = ";",
    trim_ws = TRUE,
    col_types = readr::cols(
        MeterId = readr::col_character(),
        TokenId = readr::col_character(),
        Name = readr::col_character()
    )
)

metering_points_params <- metering_points_info |>
    dplyr::group_by(TokenId) |>
    dplyr::summarise(
        metering_points = list(MeterId)
    ) |>
    dplyr::mutate(
        RefreshToken = purrr::map_chr(TokenId, Sys.getenv)
    )

metering_points_dat <- metering_points_params |>
    tidyr::drop_na(RefreshToken) |>
    dplyr::mutate(
        .keep = 'unused',
        dat = purrr::map_chr(RefreshToken, eldata::get_data_access_token)
    )

meter_data_dates <- tibble::tibble(
    dateFrom = seq.Date(from = as.Date("2020-01-01"), to = last_month, by = "month"),
    dateTo = clock::add_months(dateFrom, 1),
)

meter_data_input <- tidyr::crossing(meter_data_dates, metering_points_dat) |>
    dplyr::mutate(
        filename = fs::path(raw_save_dir, glue::glue("{dateFrom}_{dateTo}_{TokenId}"), ext = "json")
    )

missing_meter_data <- meter_data_input |>
    dplyr::filter(!fs::file_exists(filename))

meter_data_responses <- missing_meter_data |>
    dplyr::select(dateFrom, dateTo, metering_points, dat) |>
    purrr::pmap(eldata::download_meter_data, aggregation = "Actual")

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

raw_meter_data_files <- fs::dir_ls(raw_save_dir, glob = "*.json")
parsed_meter_data <- purrr::map(raw_meter_data_files, eldata::parse_meter_data)

purrr::walk(parsed_meter_data, eldata::save_parsed_meter_data, parsed_folder = fs::path("data", "parsed", "meter_data"))
