% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/energidataservice_spot_prices.R
\name{parse_spot_prices}
\alias{parse_spot_prices}
\title{Parse spot price response}
\usage{
parse_spot_prices(response)
}
\arguments{
\item{response}{Response from \link{download_spot_prices}.}
}
\value{
A \code{tibble} with columns \code{HourUTC}, \code{HourDK}, \code{PriceArea}, \code{SpotPriceDKK}, \code{SpotPriceEU}.
}
\description{
Parse spot prices into a \code{tibble}.
The columns are described on \url{https://www.energidataservice.dk/tso-electricity/elspotprices}.
In particular, the first hour of a day is 00:00. The second hour is 01:00 and so on.
}
