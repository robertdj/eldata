#' Download spot price data
#'
#' The API is described here: <https://www.energidataservice.dk/guides/api-guides>.
#'
#' @param start `[Date]` The start date.
#' @param end `[Date]` The end date.
#' @param area `[character(1)]` The area in Denmark. Allowed values are "DK1" and "DK2".
#'
#' @return A `{httr}` response.
#'
#' @export
download_spot_prices <- function(start, end, area)
{
    assertthat::assert_that(
        assertthat::is.date(start),
        assertthat::is.date(end),
        assertthat::is.string(area),
        area %in% c("DK1", "DK2")
    )

    httr::GET(
        glue::glue('https://api.energidataservice.dk/dataset/Elspotprices?start={start}&end={end}&filter={{"PriceArea":"{area}"}}')
    )
}


#' Parse spot price response
#'
#' Parse spot prices into a `tibble`.
#' The columns are described on <https://www.energidataservice.dk/tso-electricity/elspotprices>.
#' In particular, the first hour of a day is 00:00. The second hour is 01:00 and so on.
#'
#' @param response Response from [download_spot_prices].
#'
#' @return A `tibble` with columns `HourUTC`, `HourDK`, `PriceArea`, `SpotPriceDKK`, `SpotPriceEU`.
#'
#' @export
parse_spot_prices <- function(response)
{
    httr::stop_for_status(response)

    parsed_response <- RcppSimdJson::fparse(httr::content(response, as = "raw"))

    format_string <- "%Y-%m-%dT%H:%M:%S"

    purrr::chuck(parsed_response, "records") |>
        tibble::tibble() |>
        dplyr::mutate(
            HourUTC = as.POSIXct(HourUTC, format = format_string, tz = "UTC"),
            HourDK = as.POSIXct(HourDK, format = format_string, tz = "CET"),
            dplyr::across(c("SpotPriceDKK", "SpotPriceEUR"), ~ .x / 1000)
        ) |>
        dplyr::arrange(HourUTC)
}


#' Load saved spot price data
#'
#' Spot prices saved in CSV files are loaded.
#'
#' @param raw_folder Folder with CSV files.
#'
#' @inherit parse_spot_prices return
#'
#' @export
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
