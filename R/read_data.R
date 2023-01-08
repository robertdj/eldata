#' Lazy load of meter data
#'
#' There are a number of function to **lazily** load all data used in {eldata}. Check the details
#' for more info about lazy data.
#'
#' @details Lazy load means that we return a query from the `{arrow}` package, but no **data** is
#' available in R. This defers computations and (more importantly) loading of data into R so we only
#' read the necessary data. Use [dplyr::collect()] to read the data into R.
#'
#' Check these resources for more info about the `{arrow}` package:
#'  - The "dataset" vignette in the arrow package: `vignette('dataset', package = 'arrow')`.
#'  - <https://arrow-user2022.netlify.app/data-wrangling.html>
#'  - <https://arrow.apache.org/docs/r//reference/acero.html>
#'
#' @param meter_data_folder Path to the root of the meter data.
#'
#' @return An [arrow::FileSystemDataset()].
#'
#' @export
lazy_read_meter_data <- function(meter_data_folder = here::here("data", "parsed", "meter_data"))
{
    arrow::open_dataset(
        meter_data_folder,
        schema = arrow::schema(
            MeterId = arrow::utf8(),
            Date = arrow::date32(),
            HourOfDay = arrow::int32(),
            StartTimeUTC = arrow::timestamp(unit = "s", timezone = "UTC"),
            EndTimeUTC = arrow::timestamp(unit = "s", timezone = "UTC"),
            Consumption = arrow::float64(),
            Unit = arrow::utf8(),
            Quality = arrow::utf8(),
            BusinessType = arrow::utf8(),
            Resolution = arrow::utf8()
        ),
        partitioning = "MeterId",
        skip = 1,
        format = "text",
        delimiter = ";"
    )
}


#' Lazy load of spot price data
#'
#' @inherit lazy_read_meter_data description
#' @inherit lazy_read_meter_data details
#' @inherit lazy_read_meter_data return
#'
#' @param spot_price_folder Path to the root of the spot price data.
#'
#' @export
lazy_read_spot_prices <- function(spot_price_folder = here::here("data", "parsed", "spot_prices"))
{
    arrow::open_dataset(
        spot_price_folder,
        schema = arrow::schema(
            HourUTC = arrow::timestamp(unit = "s", timezone = "UTC"),
            HourDK = arrow::timestamp(unit = "s", timezone = "CET"),
            PriceArea = arrow::utf8(),
            SpotPriceDKK = arrow::float64(),
            SpotPriceEUR = arrow::float64()
        ),
        skip = 1,
        format = "text",
        delimiter = ";"
    )
}


#' Lazy load of fee data
#'
#' @inherit lazy_read_meter_data description
#' @inherit lazy_read_meter_data details
#'
#' @param fees_folder Path to file with fees data.
#' @param margin The margin of the electricity provider.
#'
#' @return An [arrow::Table()].
#'
#' @export
lazy_read_fees <- function(fees_folder = here::here("data", "fees"), margin = 0.1)
{
    all_fees_files <- fs::dir_ls(fees_folder, glob = "*.csv")

    fees <- read_fees(all_fees_files) |>
        tidyr::drop_na() |>
        dplyr::mutate(
            Margin = margin,
            AllFees = Transport + NetTarif + Elafgift
        )

    arrow::as_arrow_table(
        fees,
        schema = arrow::schema(
            Date = arrow::date32(),
            HourOfDay = arrow::int32(),
            Transport = arrow::float64(),
            FixedPrice = arrow::float64(),
            NetTarif = arrow::float64(),
            Elafgift = arrow::float64(),
            Margin = arrow::float64(),
            AllFees = arrow::float64()
        )
    )
}


#' Join consumption data and price data
#'
#' Lazy join of consumption data and price data.
#'
#' @param meter_data Output from [lazy_read_meter_data()].
#' @param spot_prices Output from [lazy_read_spot_prices()].
#' @param fees Output from [lazy_read_fees()].
#'
#' @inherit lazy_read_meter_data return
#'
#' @export
lazy_join_all <- function(meter_data, spot_prices, fees)
{
    meter_data |>
        dplyr::inner_join(spot_prices, by = c("StartTimeUTC" = "HourUTC")) |>
        dplyr::inner_join(fees, by = c("Date", "HourOfDay"))
}


#' Compute hourly consumption prices
#'
#' @param consumption_and_prices Output from [lazy_join_all()].
#'
#' @inherit lazy_read_meter_data return
#'
#' @param consumption_and_prices Output from [lazy_join_all()]
#'
#' @export
lazy_read_prices <- function(consumption_and_prices)
{
    # The Danish MOMS tax
    moms_factor = 1.25

    consumption_and_prices |>
        dplyr::transmute(
            MeterId,
            Date,
            HourOfDay,
            StartTimeUTC,
            Consumption,
            SpotPriceDKK,
            EnergyPriceFlex = moms_factor * (SpotPriceDKK + AllFees) + Margin,
            EnergyPriceFixed = moms_factor * (FixedPrice + AllFees),
            TotalPriceFlex = Consumption * EnergyPriceFlex,
            TotalPriceFixed = Consumption * EnergyPriceFixed
        )
}


#' Compute daily consumption prices
#'
#' @param price_tbl Output from [lazy_read_prices()].
#'
#' @inherit lazy_read_meter_data return
#'
#' @param price_tbl Output from [lazy_read_prices()].
#'
#' @export
lazy_daily_prices <- function(price_tbl)
{
    price_tbl |>
        dplyr::group_by(MeterId, Date) |>
        dplyr::summarise(
            .groups = "drop",
            DailyPriceFlex = sum(TotalPriceFlex),
            DailyPriceFixed = sum(TotalPriceFixed)
        ) |>
        dplyr::arrange(MeterId, Date) |>
        dplyr::mutate(
            Month = strftime(Date, format = "%Y-%m"),
            # Need Arrow v 10
            DayOfMonth = lubridate::mday(Date)
        )
}


#' Read meter points info
#'
#' Metering points info in CSV format is expected to have columns:
#'
#' - `MeterId`
#' - `TokenId`
#' - `Name`
#'
#' @param metering_points_file The location of the CSV file.
#' @param delim The delimiter passed on to [readr::read_delim()].
#'
#' @return A tibble.
#'
#' @export
read_metering_points_info <- function(metering_points_file = here::here("data", "metering_points.csv"), delim = ";")
{
    readr::read_delim(
        metering_points_file,
        delim = delim,
        trim_ws = TRUE,
        progress = FALSE,
        col_types = readr::cols(
            MeterId = readr::col_character(),
            TokenId = readr::col_character(),
            Name = readr::col_character()
        )
    )
}
