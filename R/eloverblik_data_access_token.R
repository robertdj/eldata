download_data_access_token <- function(refresh_token = get_refresh_token())
{
    httr::GET(
        "https://api.eloverblik.dk/customerapi/api/token",
        httr::accept_json(),
        httr::add_headers(Authorization = paste("Bearer", refresh_token))
    )
}


parse_data_access_token <- function(response)
{
    httr::stop_for_status(response)

    parsed_content <- httr::content(response, as = 'parsed')

    purrr::chuck(parsed_content, "result")
}


get_data_access_token <- function(refresh_token = get_refresh_token())
{
    response <- download_data_access_token(refresh_token)
    parse_data_access_token(response)
}
