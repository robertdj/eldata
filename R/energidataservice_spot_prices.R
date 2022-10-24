# https://www.energidataservice.dk/tso-electricity/elspotprices
# https://www.energidataservice.dk/guides/api-guides

# https://api.energidataservice.dk/dataset/DeclarationProduction?start=2022-05-01&end=2022-06-01&filter={"PriceArea":"DK1"}

# From
# https://www.energidataservice.dk/tso-electricity/elspotprices
# The first hour of a day is 00:00. The second hour is 01:00 and so on.
download_spot_prices <- function(start, end, area)
{
    httr::GET(
        glue::glue('https://api.energidataservice.dk/dataset/Elspotprices?start={start}&end={end}&filter={{"PriceArea":"{area}"}}')
    )
}


parse_spot_prices <- function(response)
{
    parsed_response <- RcppSimdJson::fparse(httr::content(response, as = "raw"))

    format_string <- "%Y-%m-%dT%H:%M:%S"

    tibble::tibble(parsed_response$records) |>
        dplyr::mutate(
            HourUTC = as.POSIXct(HourUTC, format = format_string, tz = "UTC"),
            HourDK = as.POSIXct(HourDK, format = format_string, tz = "CET"),
            dplyr::across(c("SpotPriceDKK", "SpotPriceEUR"), ~ .x / 1000)
        ) |>
        dplyr::arrange(HourDK)
}


#' @export
get_spot_prices <- function(start, end, area)
{
    response <- download_spot_prices(start, end, area)
    parse_spot_prices(response)
}


load_spot_prices <- function(raw_folder)
{
    csv_files <- fs::dir_ls(raw_folder, glob = "*.csv")

    readr::read_delim(
        csv_files,
        col_types = readr::cols(
            HourUTC = readr::col_datetime(format = "%Y-%m-%dT%H:%M:%SZ"),
            HourDK = readr::col_datetime(format = "%Y-%m-%dT%H:%M:%SZ"),
            PriceArea = "character",
            SpotPriceDKK = "numeric",
            SpotPriceEUR = "numeric"
        )
    ) |>
        dplyr::mutate(
            HourDK = lubridate::with_tz(HourDK, tzone = "CET")
        )
}
