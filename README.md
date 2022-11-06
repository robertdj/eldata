eldata
======

Package and scripts to interact with electricity ("el") consumption in Denmark.

The package contains functionality to get

- Consumption data from [Eloverblik](https://eloverblik.dk).
- Spot prices from [Energi data service](https://www.energidataservice.dk).


# Installation

{eldata} is only on GitHub and can be installed using the [remotes package](https://remotes.r-lib.org) with the command:

``` r
remotes::install_github("robertdj/pkg.peek")
```

If you want to run the scripts and clone this repo you can also install the package from the local copy in (at least) two ways:

- With RStudio: Open `eldata.Rproj` and click "Install" in the "Build" pane.
- Navigate to the project's root folder and run `devtools::Install()`.


# Data access

Spot price data is publicly available.
Household consumption data requires authentication.
First a refresh token is required by logging in to Eloverblik -- check their docs.

If you don't want to explicitly pass a refresh token, {eldata} looks in the environment variable `ELOVERBLIK_TOKEN`.
To this end I save them in the local file `.env` (that is not included in this repo) and make them available in R with the [{dotenv} package](https://cran.r-project.org/package=dotenv).


# Investigations

In the folder `scripts` there are scripts to download and analyse the data.

To download the data run the scripts `get_meter_data.R` and `get_spot_prices.R`.
Perhaps they need to run several times -- the APIs cannot always keep up with the demand.

Note the requests from the data providers about being nice.
In particular:
Don't make too many calls in short time spans; take a break if an internal server error is received.

The script `plot_prices.R` plots final energy price per hour and the monthly bill for fixed price and flex price.


# Final energy prices

To me as an end customer, the energy price consists of the spot price plus various fees and taxes.
It has proven difficult to get the relevant fees for my household in an automatic manner from official sources.
Instead, I have used the fees from my electricity bill. 
This probably doesn't generalize to households in other regions of Denmark.

