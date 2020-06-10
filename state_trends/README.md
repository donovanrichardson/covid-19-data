# State Trends
The [covid_normalized.csv](covid_normalized.csv) file in this directory was generated using the [percapita.R](percapita.R) script in this repo.

## Definitions:

**state:** The state, or `USA` for the whole United States of America.

**fips:** The FIPS code for the state, or `0` for the `USA`. Leading zeros got cut off, I'll fix that later.

**date:** The date of the timeseries.

**cases:** Total number of confirmed or suspected COVID-19 cases on given date

**deaths:** Total number of confirmed or suspected COVID-19 deaths on given date. See the [New York Times repo](https://github.com/nytimes/covid-19-data) for more info on confirmed/suspected cases/deaths

**daily_cases:** Increase in number of cases in the state from the day before

**daily_cases_avg:** Seven day running average of `daily_cases`

**daily_deaths:** Increase in number of deaths in the state from the day before

**daily_deaths_avg:** Seven day running average of `daily_deaths`

**daily_cases_avg_per:** `daily_cases_avg` normalized per capita

**daily_deaths_avg_per:** `daily_deaths_avg` normalized per capita

**weekly_cases_per100k:** number of reported COVID-19 cases in the past week normalized per 100k population

**weekly_deaths_per100k:** number of reported COVID-19 deaths in the past week normalized per 100k population
