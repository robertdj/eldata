#' Download data access token for Eloverblik
#'
#' @inheritParams get_data_access_token
#'
#' @return A `{httr}` response.
download_data_access_token <- function(refresh_token = get_refresh_token())
{
    httr::GET(
        "https://api.eloverblik.dk/customerapi/api/token",
        httr::accept_json(),
        httr::add_headers(Authorization = paste("Bearer", refresh_token))
    )
}


#' Parse data access toekn response
#'
#' @param response Response from [download_data_access_token].
parse_data_access_token <- function(response)
{
    httr::stop_for_status(response)

    parsed_content <- httr::content(response, as = 'parsed')

    purrr::chuck(parsed_content, "result")
}


#' Get data access token for Eloverblik
#'
#' @param refresh_token A valid refresh token
#'
#' @return The data access token as a string.
#'
#' @export
get_data_access_token <- function(refresh_token = get_refresh_token())
{
    response <- download_data_access_token(refresh_token)
    parse_data_access_token(response)
}
