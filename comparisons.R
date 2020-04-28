library(dplyr)
library(readr)
library(zoo)
library(ggplot2)
us_states <- read_csv("us-states.csv")

nony = filter(us_states, state!= "New York")

us_byday <- aggregate(cbind(cases_usa=us_states$cases, deaths_usa=us_states$deaths), by=list(date=us_states$date), FUN=sum)%>%
  mutate(daily_cases_usa = cases_usa - lag(cases_usa, default = cases_usa[1])) %>%
  mutate(daily_cases_avg_usa = rollmean(daily_cases_usa, 7, fill=NA, align="right")) %>%
  mutate(daily_deaths_usa = deaths_usa - lag(deaths_usa, default = deaths_usa[1]))%>%
  mutate(daily_deaths_avg_usa = rollmean(daily_deaths_usa, 7, fill=NA, align="right"))

nony_byday = aggregate(cbind(cases=nony$cases, deaths=nony$deaths), by=list(date=nony$date), FUN=sum)%>%
  mutate(daily_cases = cases - lag(cases, default = cases[1])) %>%
  mutate(daily_cases_avg = rollmean(daily_cases, 7, fill=NA, align="right")) %>%
  mutate(daily_deaths = deaths - lag(deaths, default = deaths[1]))%>%
  mutate(daily_deaths_avg = rollmean(daily_deaths, 7, fill=NA, align="right"))

onlyny = filter(us_states, state == "New York")[c(1,4,5)]%>%
  mutate(daily_cases = cases - lag(cases, default = cases[1])) %>%
  mutate(daily_cases_avg = rollmean(daily_cases, 7, fill=NA, align="right")) %>%
  mutate(daily_deaths = deaths - lag(deaths, default = deaths[1]))%>%
  mutate(daily_deaths_avg = rollmean(daily_deaths, 7, fill=NA, align="right"))

sidebyside = left_join(us_byday, left_join(nony_byday,onlyny, by="date", suffix = c(".nony", ".ny")), by="date")

cbp <- c("#999999", "#E69F00", "#6BC9FF", "#009E73", "#F0E442", "#372091", "#F52E00", "#C746A4", "#000000")
pal2= c("#88ccee","#0077bb","#666666","#000000", "#ee7733", "#cc3311")
#abridged version of colorblind palette ^^^

# Log plot of cases in US, NY, and US minus NY
cases <- ggplot(sidebyside, aes(x=date)) +
  geom_line(aes(y=cases_usa, color = "Nationwide Cases"), size = .85) +
  geom_line(aes(y=cases.nony, color = "Cases Outside NYS"), size = .85) +
  geom_line(aes(y=cases.ny, color = "NYS Cases"), size = .85) +
  scale_y_log10(breaks=c(1,10,100,1000,10000,100000,1000000), minor_breaks=c(5,50,500,5000,50000,500000)) +
  annotation_logticks() +
  scale_colour_manual("", 
                      breaks = c("Nationwide Cases", "Cases Outside NYS", "NYS Cases"),
                      values = c(cbp[8],cbp[1],cbp[3])) +
  theme_minimal()

# Daily COVID-19 Cases in the US with nice coloring
dailyLine <- ggplot(sidebyside[-(1:39),], aes(x=date)) +
  geom_line(aes(y=daily_cases_usa), color=cbp[1], size = .85) +
  geom_line(aes(y=daily_cases.nony),color=cbp[8], size = .85) +
  geom_line(aes(y=daily_cases.ny),color=cbp[3], size = .85) +
  geom_line(aes(y=daily_deaths_usa), color=cbp[9], size = .85) +
  geom_line(aes(y=daily_deaths.nony),color=cbp[7], size = .85) +
  geom_line(aes(y=daily_deaths.ny),color=cbp[6], size = .85) +
  scale_y_log10() +
  theme_minimal()

#Daily COVID-19 Cases in the US with legend, running 7-day average and deuteranopia colors
dailyDotLine<-ggplot(sidebyside[-(1:39),], aes(x=date)) +
  geom_point(aes(y=daily_cases_usa, color="Nationwide Cases"), size=.6) +
  geom_line(aes(y=daily_cases_avg_usa, color="Nationwide Cases"), size=.75) +
  geom_point(aes(y=daily_cases.nony,color="Cases Outside NYS"), size=.6) +
  geom_line(aes(y=daily_cases_avg.nony,color="Cases Outside NYS"), size=.75) +
  geom_point(aes(y=daily_cases.ny,color="NYS Cases"), size=.6) +
  geom_line(aes(y=daily_cases_avg.ny,color="NYS Cases"), size=.75) +
  geom_point(aes(y=daily_deaths_usa, color="Nationwide Deaths"), size=.6) +
  geom_line(aes(y=daily_deaths_avg_usa, color="Nationwide Deaths"), size=.75) +
  geom_point(aes(y=daily_deaths.nony,color="Deaths Outside NYS"), size=.6) +
  geom_line(aes(y=daily_deaths_avg.nony,color="Deaths Outside NYS"), size=.75) +
  geom_point(aes(y=daily_deaths.ny,color="NYS Deaths"), size=.6) +
  geom_line(aes(y=daily_deaths_avg.ny,color="NYS Deaths"), size=.75) +
  scale_colour_manual("", 
                      breaks = c("Nationwide Cases", "Cases Outside NYS", "NYS Cases","Nationwide Deaths","Deaths Outside NYS","NYS Deaths"),
                      values = pal2) +
  xlab("Date") +
  ylab("Daily Cases") +
  labs(title = "Daily COVID-19 Cases in the US (to April 27)",
       caption = "Points are recorded cases and deaths for each day.\nLines are 7-day running averages.\nGraph by Donovan Richardson\nData from The New York Times, based on reports from state and local health agencies.") +
  scale_y_log10(breaks=c(1,10,100,1000,10000), minor_breaks=c(5,50,500,5000,50000)) +
  annotation_logticks() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0))

dailyDotLine

# creates the DailyCases.png file shown in the README.md
ggsave("DailyCases.png", dailyDotLine)
