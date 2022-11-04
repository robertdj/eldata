library(magrittr)
library(ggplot2)

meter_data <- eldata:::load_meter_data("data/raw/meter_data")
spot_prices <- load_spot_prices("data/raw/spot_prices")

# From https://wen.dk
tarif <- 0.143
afgift <- 0.3462
moms <- 1.25

tbl <- dplyr::left_join(meter_data, spot_prices, by = c("StartTime" = "HourDK")) |>
    dplyr::mutate(
        Date = lubridate::date(StartTime),
        TotalPrice = moms * (SpotPriceDKK + tarif + afgift)
    )

tbl |>
    dplyr::filter(is.na(Consumption))

tbl |>
    dplyr::group_by(Year = lubridate::year(Date)) |>
    dplyr::summarise(
        Consumption = sum(Consumption)
    )

avg_price <- tbl |>
    dplyr::group_by(Date) |>
    dplyr::summarise(
        DailyAvgPrice = sum(Consumption * SpotPriceDKK) / sum(Consumption),
        Consumption = sum(Consumption)
    )

plotly::plot_ly(
    data = spot_prices,
    x = ~ HourDK,
    y = ~ SpotPriceDKK,
    type = "scatter",
    mode = "lines"
)


avg_price |>
    ggplot(aes(Consumption)) +
    xlab("Consumption [kWh]") +
    xlim(c(0, NA_real_)) +
    geom_histogram()

tbl |>
    dplyr::filter(Date >= as.Date("2022-06-01")) |>
    ggplot(aes(SpotPriceDKK)) +
    # facet_wrap(~ HourOfDay) +
    geom_histogram()

avg_price |>
    ggplot(aes(DailyAvgPrice)) +
    geom_histogram()

avg_price |>
    ggplot(aes(Date, DailyAvgPrice)) +
    geom_line()

plotly::plot_ly(
    data = avg_price,
    x = ~ Date,
    y = ~ DailyAvgPrice,
    type = "scatter",
    # mode = "lines+markers"
    mode = "lines"
)

plotly::plot_ly(
    data = avg_price,
    x = ~ Date,
    y = ~ Consumption,
    type = "scatter",
    mode = "lines"
)


tbl |>
    dplyr::filter(Date >= as.Date("2022-09-01")) |>
    ggplot(aes(SpotPriceDKK, log(Consumption))) +
    geom_point()

tbl |>
    dplyr::filter(Date >= as.Date("2022-09-01")) |>
    ggplot(aes(HourOfDay, SpotPriceDKK, group = Date)) +
    geom_line() +
    geom_point()

tbl |>
    dplyr::filter(Date >= as.Date("2022-09-01")) |>
    ggplot(aes(HourOfDay, Consumption, group = Date)) +
    geom_line() +
    geom_point()

plotly::plot_ly(
    data = tbl,
    x = ~ StartTime,
    y = ~ Consumption,
    name = "Consumption",
    type = "scatter",
    mode = "lines"
) |>
    plotly::add_trace(
        y = ~ SpotPriceDKK,
        name = "Spot price"
    )
