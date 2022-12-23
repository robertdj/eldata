library(ggplot2)

metering_point = Sys.getenv("ELOVERBLIK_METERING_POINT")
metering_point = metering_points[2]

meter_data <- eldata::load_meter_data(fs::path("data/parsed/meter_data", metering_point))
spot_prices <- eldata::load_spot_prices("data/raw/spot_prices")

all_fees_files <- fs::dir_ls(here::here("data", "fees"), glob = "*.csv")
fees <- eldata::read_fees(all_fees_files) |>
    tidyr::drop_na()
# Upper bound on margin
fees$Margin <- 0.1

consumption_and_spot <- dplyr::inner_join(meter_data, spot_prices, by = c("StartTimeUTC" = "HourUTC"))
consumption_and_prices <- dplyr::inner_join(consumption_and_spot, fees, by = c("Date", "HourOfDay"))

moms_factor <- 1.25

price_tbl <- consumption_and_prices |>
    dplyr::mutate(
        AllFees = Transport + NetTarif + Fees
    ) |>
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
        TotalPriceFixed = Consumption * EnergyPriceFixed,
        WeekDay = lubridate::wday(Date, week_start = 1),
        IsWorkday = ifelse(WeekDay <= 5, TRUE, FALSE),
    )

daily_tbl <- price_tbl |>
    dplyr::group_by(MeterId, Date) |>
    dplyr::summarise(
        DailyPriceFlex = sum(TotalPriceFlex),
        DailyPriceFixed = sum(TotalPriceFixed)
    ) |>
    dplyr::mutate(
        Month = as.character(Date, format = "%Y-%m"),
        DayOfMonth = lubridate::day(Date)
    )

long_daily_tbl <- daily_tbl |>
    tidyr::pivot_longer(
        c("DailyPriceFlex", "DailyPriceFixed"),
        names_prefix = "DailyPrice",
        names_to = "Type",
        values_to = "Price"
    ) |>
    dplyr::group_by(MeterId, Type, Month) |>
    dplyr::mutate(
        AccumulatedMonthlyPrice = cumsum(Price)
    ) |>
    dplyr::ungroup()


# Price plot ----------------------------------------------------------------------------------

long_daily_tbl |>
    dplyr::filter(Date >= as.Date("2022-01-01")) |>
    ggplot(aes(DayOfMonth, AccumulatedMonthlyPrice, group = Type, color = Type)) +
    facet_wrap(~ Month, scales = "free_y") +
    xlab("Day of month") +
    ylab("Accumulated monthly bill") +
    geom_step()

long_daily_tbl |>
    dplyr::filter(Date >= as.Date("2022-01-01")) |>
    dplyr::group_by(Type) |>
    dplyr::mutate(
        AccumulatedPrice = cumsum(Price)
    ) |>
    ggplot(aes(Date, AccumulatedPrice, group = Type, color = Type)) +
    xlab("Date") +
    ylab("Accumulated bill") +
    geom_step()

ggsave("monthly_bills.png", width = 20, height = 12, units = "cm")


# Spot prices ---------------------------------------------------------------------------------

long_price_tbl <- price_tbl |>
    dplyr::select(StartTimeUTC, EnergyPriceFlex, EnergyPriceFixed) |>
    tidyr::pivot_longer(
        c("EnergyPriceFlex", "EnergyPriceFixed"),
        names_prefix = "EnergyPrice",
        names_to = "Type",
        values_to = "Price"
    )

long_price_tbl |>
    plotly::plot_ly(
        x = ~ StartTimeUTC,
        y = ~ Price,
        color = ~ Type,
        type = "scatter",
        mode = "lines"
    )

long_price_tbl |>
    dplyr::filter(
        StartTimeUTC >= as.Date("2022-08-01"),
        StartTimeUTC <= as.Date("2022-08-30")
    ) |>
    ggplot(aes(StartTimeUTC, Price, group = Type, color = Type)) +
    xlab("") +
    ylab("Total price per kWh") +
    geom_line()

ggsave("final_price_august_2022.png", width = 18, height = 10, units = "cm")
