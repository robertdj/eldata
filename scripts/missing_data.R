meter_data <- load_meter_data("data/raw/meter_data")
spot_prices <- load_spot_prices("data/raw/spot_prices")

all_md_and_sp <- dplyr::full_join(meter_data, spot_prices, by = c("StartTimeUTC" = "HourUTC"))

missing_consumption <- all_md_and_sp |>
    dplyr::filter(is.na(Consumption))

missing_spot <- all_md_and_sp |>
    dplyr::filter(is.na(SpotPriceDKK))

missing_spot_days <- all_md_and_sp |>
    dplyr::filter(Date %in% unique(missing_spot$Date))
