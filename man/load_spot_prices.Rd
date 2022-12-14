% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/energidataservice_spot_prices.R
\name{load_spot_prices}
\alias{load_spot_prices}
\title{Load saved spot price data}
\usage{
load_spot_prices(raw_folder)
}
\arguments{
\item{raw_folder}{Folder with CSV files.}
}
\value{
A \code{tibble} with columns \code{HourUTC}, \code{HourDK}, \code{PriceArea}, \code{SpotPriceDKK}, \code{SpotPriceEU}.
}
\description{
Spot prices saved in CSV files are loaded.
}
