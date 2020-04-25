library(dplyr)

nony = filter(us_states, state!= "New York")

us_byday <- aggregate(cbind(cases_usa=us_states$cases, deaths_usa=us_states$deaths), by=list(date=us_states$date), FUN=sum)%>%
  mutate(daily_cases_usa = cases_usa - lag(cases_usa, default = cases_usa[1])) %>%
  mutate(daily_deaths_usa = deaths_usa - lag(deaths_usa, default = deaths_usa[1]))

nony_byday = aggregate(cbind(cases=nony$cases, deaths=nony$deaths), by=list(date=nony$date), FUN=sum)%>%
  mutate(daily_cases = cases - lag(cases, default = cases[1])) %>%
  mutate(daily_deaths = deaths - lag(deaths, default = deaths[1]))

onlyny = filter(us_states, state == "New York")[c(1,4,5)]%>%
  mutate(daily_cases = cases - lag(cases, default = cases[1])) %>%
  mutate(daily_deaths = deaths - lag(deaths, default = deaths[1]))

sidebyside = left_join(us_byday, left_join(nony_byday,onlyny, by="date", suffix = c(".nony", ".ny")), by="date")
