eldata
======

Package and scripts to interact with electricity ("el") consumption in Denmark.

The package contains functionality:

- To get consumption data from [Eloverblik](https://eloverblik.dk).
- To get spot prices from [Energi data service](https://www.energidataservice.dk).
- A Shiny app to visualize prices and consumption.


# Installation

{eldata} is only on GitHub and can be installed using the [remotes package](https://remotes.r-lib.org) with the command:

``` r
remotes::install_github("robertdj/eldata")
```

If you want to run the scripts and clone this repo you can also install the package from the local copy in (at least) two ways:

- With RStudio: Open `eldata.Rproj` and click "Install" in the "Build" pane.
- Navigate to the project's root folder and run `devtools::Install()`.


# Data access

Spot price data is publicly available.
Household consumption data requires authentication.
First a refresh token is required by logging in to Eloverblik -- check their docs.

I save refesh tokens in environment variables in the local file `.env` (that is not included in this repo) and make them available in R with the [{dotenv} package](https://cran.r-project.org/package=dotenv).

In the folder `scripts` there are scripts to download and analyse the data.

To download the data run the scripts `get_meter_data.R` and `get_spot_prices.R`.
Perhaps they need to run several times -- the APIs cannot always keep up with the demand.

When downloading consumption data with `get_meter_data.R` I rely on metering points data in a "CSV" file with the following format:

```
MeterId    ; TokenId      ; Name
<meter id> ; <token name> ; <name>
```

By default, this should be located in `data/meterings_points.csv`.
I have not included this in the repo (for obvious reasonss).
The columns are used as follows: 

- The column `MeterId` is the id from Energi data service (a long number). 
- The column `TokenId` is the *environment variable* that holds the token used for downloading.
Data from meters with the same `TokenId` (that is, with the same owner) are requested in a single call.
- The column `Name` is a human recognizable name -- this is also used in the Shiny app.

Note the requests from the data providers about being nice.
In particular:
Don't make too many calls in short time spans; take a break if an internal server error is received.

The script `plot_prices.R` plots final energy price per hour and the monthly bill for fixed price and flex price.
For a more interactive experience, run the Shiny app in `shiny/app.R`.


## Shiny app

The Shiny app requires a few packages that are not installed by default with {eldata}. To include those install the package with the command

``` r
remotes::install_github("robertdj/eldata", dependencies = TRUE)
```

If you are using Linux, note that one of the packages used is [{arrow}](https://cran.r-project.org/package=arrow) that (at the time of writing) needs a little help to get installed.
To compile the {arrow} from scratch, use e.g.

```r
withr::with_envvar(new = c("NOT_CRAN" = TRUE), install.packages("arrow"))
```

Note that this will take a loong time.


# Final energy prices

To me as an end customer, the energy price consists of the spot price plus various fees and taxes.
It has proven difficult to get the relevant fees for my household in an automatic manner from official sources.
Instead, I have used the fees from my electricity bill. 
This probably doesn't generalize to households in other regions of Denmark.

All fees data is included in the folder `data/fees`.

