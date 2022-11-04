#' Retrieve refresh token
#'
#' Read refresh token from the environment variable `ELOVERBLIK_TOKEN`, if available.
#'
#'@export
get_refresh_token <- function()
{
    token <- Sys.getenv("ELOVERBLIK_TOKEN", unset = NA_character_)

    if (is.na(token))
        stop("Refresh token is not available")

    return(token)
}
