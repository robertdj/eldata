eldata
======

Package and scripts to interact with electricity ("el") consumption in Denmark.

The package contains functionality to get

- Consumption data from [Eloverblik](https://eloverblik.dk).
- Spot prices from [Energi data service](https://www.energidataservice.dk).

In the folder `scripts` there are scripts to analyse the data.

The refresh token for eloverblik must be available as the environment variable `ELOVERBLIK_TOKEN`.
To this end I save them in the local file `.env` (that is not included in this repo) and make them available in R with the [{direnv} package](https://direnv.net).


# Energy prices

To me as an end customer, the energy price consists of the spot price plus various fees and taxes.
It has proven difficult to get the relevant fees for my household in an automatic manner from official sources.
Instead, I have "reverse engineered" my electricity bill. This probably doesn't generalize to households in other regions of Denmark.

