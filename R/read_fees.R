read_fees <- function(fees_file)
{
    wide_fees <- readr::read_delim(fees_file, delim = ";", trim_ws = TRUE)

    nested_fees <- wide_fees |>
        tidyr::fill(Transport, NetTarif, Fees, Margin, FixedPrice, .direction = "downup") |>
        dplyr::mutate(
            Date = purrr::map2(StartDate, EndDate, seq, by = "1 day"),
            HourOfDay = purrr::map2(HourStart, HourEnd, seq, by = 1),
            .keep = "unused"
        )

    nested_fees |>
        tidyr::unnest(Date) |>
        tidyr::unnest(HourOfDay) |>
        dplyr::arrange(Date, HourOfDay)
}
