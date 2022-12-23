#' Read all fees files
#'
#' @param all_fees_files A vector of filenames with fees. Currently all CSV files in `data/fees`.
#'
#' @return A `tibble` with all fees for every hour of every day.
#'
#' @export
read_fees <- function(all_fees_files)
{
    all_wide_fees <- purrr::map(
        all_fees_files,
        readr::read_delim,
        progress = FALSE, delim = ";", trim_ws = TRUE, col_types = readr::cols()
    )

    all_long_fees <- purrr::map(all_wide_fees, wide_to_long_fees)

    purrr::reduce(all_long_fees, dplyr::inner_join, by = c("Date", "HourOfDay")) |>
        dplyr::arrange(Date, HourOfDay)
}


wide_to_long_fees <- function(wide_fees)
{
    if (!(all(c("StartHour", "EndHour") %in% names(wide_fees))))
    {
        wide_fees$StartHour <- 1
        wide_fees$EndHour <- 24
    }

    nested_fees <- wide_fees |>
        dplyr::mutate(
            Date = purrr::map2(StartDate, EndDate, seq, by = "1 day"),
            HourOfDay = purrr::map2(StartHour, EndHour, seq, by = 1),
            .keep = "unused",
            .before = 1
        )

    long_fees <- nested_fees |>
        tidyr::unnest(Date) |>
        tidyr::unnest(HourOfDay)

    return(long_fees)
}
