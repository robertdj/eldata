library(ggplot2)

meter_data <- load_meter_data("data/raw/meter_data")
spot_prices <- load_spot_prices("data/raw/spot_prices")
fees <- read_fees("fees.csv")

consumption_and_spot <- dplyr::inner_join(meter_data, spot_prices, by = c("StartTimeUTC" = "HourUTC"))
consumption_and_prices <- dplyr::inner_join(consumption_and_spot, fees, by = c("Date", "HourOfDay"))

moms_factor <- 1.25

price_tbl <- consumption_and_prices |>
    dplyr::mutate(
        AllFees = Transport + NetTarif + Fees
    ) |>
    dplyr::transmute(
        Date,
        HourOfDay,
        StartTimeUTC,
        Consumption,
        SpotPriceDKK,
        EnergyPriceFlex = moms_factor * (SpotPriceDKK + Transport + NetTarif + Fees) + Margin,
        EnergyPriceFixed = moms_factor * (FixedPrice + Transport + NetTarif + Fees),
        TotalPriceFlex = Consumption * EnergyPriceFlex,
        TotalPriceFixed = Consumption * EnergyPriceFixed,
        WeekDay = lubridate::wday(Date, week_start = 1),
        IsWorkday = ifelse(WeekDay <= 5, TRUE, FALSE),
    )

daily_tbl <- price_tbl |>
    dplyr::group_by(Date) |>
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
    dplyr::group_by(Type, Month) |>
    dplyr::mutate(
        AccumulatedPrice = cumsum(Price)
    ) |>
    dplyr::ungroup()


# Price plot ----------------------------------------------------------------------------------

long_daily_tbl |>
    ggplot(aes(DayOfMonth, AccumulatedPrice, group = Type, color = Type)) +
    facet_wrap(~ Month, scales = "free_y") +
    xlab("Day of month") +
    ylab("Accumulated daily bill") +
    geom_step()


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
