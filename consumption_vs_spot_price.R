library(magrittr)
library(ggplot2)

meter_data <- eldata:::load_meter_data("data/raw/meter_data")
spot_prices <- load_spot_prices("data/raw/spot_prices")

tbl <- dplyr::left_join(meter_data, spot_prices, by = c("StartTime" = "HourDK")) %>%
    dplyr::mutate(
        Date = lubridate::date(StartTime)
    )

tbl %>%
    dplyr::filter(is.na(Consumption))

avg_price <- tbl %>%
    dplyr::group_by(Date) %>%
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


avg_price %>%
    ggplot(aes(Consumption)) +
    xlab("Consumption [kWh]") +
    xlim(c(0, NA_real_)) +
    geom_histogram()

avg_price %>%
    ggplot(aes(DailyAvgPrice)) +
    geom_histogram()

avg_price %>%
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
