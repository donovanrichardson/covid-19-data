# install.packages("zoo")

library(dplyr)
library(readr)
library(zoo)
library(ggplot2)

library(sqldf)

# View(sqldf('select distinct state, fips from us_states'))
us_states <- read_csv("us-states.csv")
us_states$fips <- as.numeric(us_states$fips) #technically this is incorrect, but I did not want to put quotes and zeroes in front of all the single digit numbers

refdate <- Sys.Date() -1
toDate <- sprintf("(to %s)", sub('  ', ' ', format(refdate, "%B %e")))

nony = filter(us_states, !(state %in% c("New York")))



nonynj = filter(us_states, !(state %in% c("New York", "New Jersey")))

us_byday <- aggregate(cbind(cases_usa=us_states$cases, deaths_usa=us_states$deaths), by=list(date=us_states$date), FUN=sum)%>%
  mutate(daily_cases_usa = cases_usa - lag(cases_usa, default = cases_usa[1])) %>%
  mutate(daily_cases_avg_usa = rollmean(daily_cases_usa, 7, fill=NA, align="right")) %>%
  mutate(daily_deaths_usa = deaths_usa - lag(deaths_usa, default = deaths_usa[1]))%>%
  mutate(daily_deaths_avg_usa = rollmean(daily_deaths_usa, 7, fill=NA, align="right"))



nony_byday = aggregate(cbind(cases=nony$cases, deaths=nony$deaths), by=list(date=nony$date), FUN=sum)%>%
  mutate(daily_cases.nony = cases - lag(cases, default = cases[1])) %>%
  mutate(daily_cases_avg.nony = rollmean(daily_cases.nony, 7, fill=NA, align="right")) %>%
  mutate(daily_deaths.nony = deaths - lag(deaths, default = deaths[1]))%>%
  mutate(daily_deaths_avg.nony = rollmean(daily_deaths.nony, 7, fill=NA, align="right"))

nonynj_byday = aggregate(cbind(cases=nonynj$cases, deaths=nonynj$deaths), by=list(date=nonynj$date), FUN=sum)%>%
  mutate(daily_cases.nonynj = cases - lag(cases, default = cases[1])) %>%
  mutate(daily_cases_avg.nonynj = rollmean(daily_cases.nonynj, 7, fill=NA, align="right")) %>%
  mutate(daily_deaths.nonynj = deaths - lag(deaths, default = deaths[1]))%>%
  mutate(daily_deaths_avg.nonynj = rollmean(daily_deaths.nonynj, 7, fill=NA, align="right"))

onlyny = filter(us_states, state == "New York")[c(1,4,5)]%>%
  mutate(daily_cases.ny = cases - lag(cases, default = cases[1])) %>%
  mutate(daily_cases_avg.ny = rollmean(daily_cases.ny, 7, fill=NA, align="right")) %>%
  mutate(daily_deaths.ny = deaths - lag(deaths, default = deaths[1]))%>%
  mutate(daily_deaths_avg.ny = rollmean(daily_deaths.ny, 7, fill=NA, align="right"))

onlynj = filter(us_states, state == "New Jersey")[c(1,4,5)]%>%
  mutate(daily_cases.nj = cases - lag(cases, default = cases[1])) %>%
  mutate(daily_cases_avg.nj = rollmean(daily_cases.nj, 7, fill=NA, align="right")) %>%
  mutate(daily_deaths.nj = deaths - lag(deaths, default = deaths[1]))%>%
  mutate(daily_deaths_avg.nj = rollmean(daily_deaths.nj, 7, fill=NA, align="right"))

sidebyside = left_join(us_byday, left_join(nony_byday,left_join(onlyny, onlynj, by='date'), by="date"), by="date")

sidebyside2 = left_join(us_byday, left_join(nonynj_byday,left_join(onlyny, onlynj, by='date'), by="date"), by="date")

cbp <- c("#999999", "#E69F00", "#6BC9FF", "#009E73", "#F0E442", "#372091", "#F52E00", "#C746A4", "#000000")

cbp4 <- c("#999999", "#E69F00", "#6BC9FF", "#009E73", "#F0E442", "#372091", "#F52E00", "#C746A4", "#000000", "#E69F00", "#6BC9FF", "#009E73", "#F0E442", "#372091", "#F52E00", "#C746A4", "#000000")

pal2= c("#88ccee","#0077bb","#666666","#000000", "#ee7733", "#cc3311")
#abridged version of colorblind palette ^^^#c nony, d nony, us cas, us dea, nycas, nydea

pal3= c("#0077bb","#000000", "#ee7733", "#cc3311")
#d nony, us dea, njdea, nydea

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
  labs(title = paste("Daily COVID-19 Cases and Deaths in the US", toDate),
       caption = "Points are cases and deaths reported each day.\nLines are 7-day running averages.\nGraph by Donovan Richardson\nData from The New York Times, based on reports from state and local health agencies.") +
  scale_y_log10(breaks=c(1,10,100,1000,10000), minor_breaks=c(5,50,500,5000,50000)) +
  annotation_logticks() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0))

dailyDotLine

dailyNyNj<-ggplot(sidebyside2[-(1:55),], aes(x=date)) +
  geom_point(aes(y=daily_deaths_usa, color="Nationwide Deaths"), size=.6) +
  geom_line(aes(y=daily_deaths_avg_usa, color="Nationwide Deaths"), size=.75) +
  geom_point(aes(y=daily_deaths.nonynj,color="Deaths Outside NY&NJ"), size=.6) +
  geom_line(aes(y=daily_deaths_avg.nonynj,color="Deaths Outside NY&NJ"), size=.75) +
  geom_point(aes(y=daily_deaths.ny,color="NYS Deaths"), size=.6) +
  geom_line(aes(y=daily_deaths_avg.ny,color="NYS Deaths"), size=.75) +
  
  geom_point(aes(y=daily_deaths.nj,color="New Jersey Deaths"), size=.6) +
  geom_line(aes(y=daily_deaths_avg.nj,color="New Jersey Deaths"), size=.75) +
  scale_colour_manual("", 
                      breaks = c("Nationwide Deaths","Deaths Outside NY&NJ","NYS Deaths", "New Jersey Deaths"),
                      values = pal3) +
  xlab("Date") +
  ylab("Daily Cases") +
  labs(title = paste("Daily COVID-19 Cases and Deaths in the US", toDate),
       caption = "Points are deaths reported each day.\nLines are 7-day running averages.\nGraph by Donovan Richardson\nData from The New York Times, based on reports from state and local health agencies.") +
  # scale_y_log10(breaks=c(1,10,100,1000), minor_breaks=c(5,50,500,5000)) +
  # annotation_logticks() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0))

# creates the DailyCases.png file shown in the README.md

dailyNyNj

ggsave("DailyCases.png", dailyDotLine)
ggsave("DailyNY-NJ.png", dailyNyNj)
ggsave("deaths_regional.pdf",by_region)

# ggsave("DailyCases.pdf", dailyDotLine)
# ggsave("DailyNY-NJ.pdf", dailyNyNj)

# View(sidebyside$daily_deaths_avg.ny / sidebyside$daily_deaths_avg_usa)
# View((sidebyside$daily_deaths_avg.ny + sidebyside$daily_deaths_avg.nj) / sidebyside$daily_deaths_avg_usa)
