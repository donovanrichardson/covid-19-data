library(dplyr)
library(readr)
us_states <- read_csv("us-states.csv")

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

#
#

# Libraries
library(ggplot2)

cbp <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
#abridged version ^^^

# Log plot of cases in US, NY, and US minus NY
ggplot(sidebyside, aes(x=date)) +
  geom_line(aes(y=cases_usa), color=cbp[1], size = .85) +
  geom_line(aes(y=cases.nony),color=cbp[8], size = .85) +
  geom_line(aes(y=cases.ny),color=cbp[3], size = .85) +
  scale_y_log10() +
  theme_minimal()

# Daily COVID-19 Cases in the US with ok coloring
ggplot(sidebyside[40:95,], aes(x=date)) +
  geom_line(aes(y=daily_cases_usa), color=cbp[1], size = .85) +
  geom_line(aes(y=daily_cases.nony),color=cbp[8], size = .85) +
  geom_line(aes(y=daily_cases.ny),color=cbp[3], size = .85) +
  geom_line(aes(y=daily_deaths_usa), color=cbp[9], size = .85) +
  geom_line(aes(y=daily_deaths.nony),color=cbp[7], size = .85) +
  geom_line(aes(y=daily_deaths.ny),color=cbp[6], size = .85) +
  scale_y_log10() +
  theme_minimal()

#Daily COVID-19 Cases in the US with legend and ugly coloring
ggplot(sidebyside[40:95,], aes(x=date)) +
  geom_line(aes(y=daily_cases_usa, color="Nationwide Cases"), size = .85) +
  geom_line(aes(y=daily_cases.nony,color="Cases Outside NYS"), size = .85) +
  geom_line(aes(y=daily_cases.ny,color="NYS Cases"), size = .85) +
  geom_line(aes(y=daily_deaths_usa, color="Nationwide Deaths"), size = .85) +
  geom_line(aes(y=daily_deaths.nony,color="Deaths Outside NYS"), size = .85) +
  geom_line(aes(y=daily_deaths.ny,color="NYS Deaths"), size = .85) +
  scale_colour_manual("", 
                      breaks = c("Nationwide Cases", "Cases Outside NYS", "NYS Cases","Nationwide Deaths","Deaths Outside NYS","NYS Deaths"),
                      values = 1:6) +
  xlab("Date") +
  ylab("Daily Cases") +
  labs(title = "Daily COVID-19 Cases in the US") +
  scale_y_log10(breaks=c(1,10,100,1000,10000), minor_breaks=c(5,50,500,5000,50000)) +
  annotation_logticks() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
  

