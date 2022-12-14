% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_fees.R
\name{read_fees}
\alias{read_fees}
\title{Read all fees files}
\usage{
read_fees(all_fees_files)
}
\arguments{
\item{all_fees_files}{A vector of filenames with fees. Currently all CSV files in \code{data/fees}.}
}
\value{
A \code{tibble} with all fees for every hour of every day.
}
\description{
Read all fees files
}
