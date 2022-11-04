eldata
======

Package and scripts to interact with electricity ("el") consumption in Denmark.

The package contains functionality to get

- Consumption data from [Eloverblik](https://eloverblik.dk).
- Spot prices from [Energi data service](https://www.energidataservice.dk).


# Data access

Spot price data is publicly available.
Household consumption data requires authentication.
First a refresh token is required by logging in to Eloverblik -- check their docs.

{eldata} expects to find the refresh token for Eloverblik in the environment variable `ELOVERBLIK_TOKEN`.
To this end I save them in the local file `.env` (that is not included in this repo) and make them available in R with the [{dotenv} package](https://cran.r-project.org/package=dotenv).


# Investigations

In the folder `scripts` there are scripts to download and analyse the data.

To download the data run the scripts `get_meter_data.R` and `get_spot_prices.R`.
Note the requests from the data providers about being nice.
In particular:
Don't make too many calls in short time spans; take a break if an internal server error is received.


# Final energy prices

To me as an end customer, the energy price consists of the spot price plus various fees and taxes.
It has proven difficult to get the relevant fees for my household in an automatic manner from official sources.
Instead, I have "reverse engineered" my electricity bill. This probably doesn't generalize to households in other regions of Denmark.

