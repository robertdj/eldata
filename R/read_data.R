# https://arrow-user2022.netlify.app/data-wrangling.html

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


lazy_read_fees <- function(fees_folder = here::here("data", "fees"), margin = 0.1)
{
    all_fees_files <- fs::dir_ls(fees_folder, glob = "*.csv")

    fees <- eldata::read_fees(all_fees_files) |>
        tidyr::drop_na() |>
        dplyr::mutate(
            Margin = margin,
            AllFees = Transport + NetTarif + Fees
        )

    arrow::as_arrow_table(
        fees,
        schema = arrow::schema(
            Date = arrow::date32(),
            HourOfDay = arrow::int32(),
            NetTarif = arrow::float64(),
            FixedPrice = arrow::float64(),
            Transport = arrow::float64(),
            Fees = arrow::float64(),
            Margin = arrow::float64(),
            AllFees = arrow::float64()
        )
    )
}


lazy_read_all <- function(meter_data_folder, spot_price_folder, fees_folder)
{
    meter_data <- lazy_read_meter_data(meter_data_folder)
    spot_prices <- lazy_read_spot_prices(spot_price_folder)
    fees <- lazy_read_fees(fees_folder)
}


lazy_join_all <- function(meter_data, spot_prices, fees)
{
    meter_data |>
        dplyr::inner_join(spot_prices, by = c("StartTimeUTC" = "HourUTC")) |>
        dplyr::inner_join(fees, by = c("Date", "HourOfDay"))
}


lazy_read_prices <- function(consumption_and_prices, moms_factor = 1.25)
{
    price_tbl <- consumption_and_prices |>
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


read_metering_points_info <- function(metering_points_file = here::here("data", "metering_points.csv"))
{
    metering_points_info <- readr::read_delim(
        metering_points_file,
        delim = ";",
        trim_ws = TRUE,
        col_types = readr::cols(
            MeterId = readr::col_character(),
            TokenId = readr::col_character(),
            Name = readr::col_character()
        )
    )
}
