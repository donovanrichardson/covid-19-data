# install.packages("zoo")

library(dplyr)
library(readr)
library(zoo)
library(ggplot2)

# library(sqldf)

# View(sqldf('select distinct state, fips from us_states'))
us_states <- read_csv("us-states.csv")
us_states$fips <- as.numeric(us_states$fips) #technically this is incorrect, but I did not want to put quotes and zeroes in front of all the single digit numbers

nony = filter(us_states, !(state %in% c("New York")))

# ggplot() +
#   geom_line(data=south, aes(x=date, y=daily_deaths_avg, color="South")) +
#   # geom_line(data=mwe, aes(x=date, y=daily_deaths_avg, color="Midwest(East)")) +
#   # geom_line(data=mww, aes(x=date, y=daily_deaths_avg, color="Midwest(Plains)")) +
#   geom_line(data=ny, aes(x=date, y=daily_deaths_avg, color="New York")) +
#   geom_line(data=nj, aes(x=date, y=daily_deaths_avg, color="New Jersey")) +
#   geom_line(data=mtn, aes(x=date, y=daily_deaths_avg, color="Mounatin")) +
#   geom_line(data=aznm, aes(x=date, y=daily_deaths_avg, color="AZ+NM")) +
#   geom_line(data=cali, aes(x=date, y=daily_deaths_avg, color="California")) +
#   geom_line(data=pnw, aes(x=date, y=daily_deaths_avg, color="Pacific NW")) +
#   geom_line(data=akhi, aes(x=date, y=daily_deaths_avg, color="AK+HI")) +
#   geom_line(data=penn, aes(x=date, y=daily_deaths_avg, color="Pennsylvania")) +
#   geom_line(data=new, aes(x=date, y=daily_deaths_avg, color="New England")) + scale_y_log10() 
# + scale_colour_manual("",breaks=c("South","Midwest(East)","Midwest(Plains),New York,New Jersey, Mountain, AZ+NM, California, Pacific NW, AK+HI, Pennsylvania, New England"), values= cbp4)
  


ny<- sqldf('select date, sum(cases) as cases, sum(deaths) as deaths from us_states where fips in (36) group by date') %>%
  mutate(daily_cases = cases - lag(cases, default = cases[1])) %>%
  mutate(daily_cases_avg = rollmean(daily_cases, 7, fill=NA, align="right")) %>%
  mutate(daily_deaths = deaths - lag(deaths, default = deaths[1]))%>%
  mutate(daily_deaths_avg = rollmean(daily_deaths, 7, fill=NA, align="right"))
nj<- sqldf('select date, sum(cases) as cases, sum(deaths) as deaths from us_states where fips in (34) group by date') %>%
  mutate(daily_cases = cases - lag(cases, default = cases[1])) %>%
  mutate(daily_cases_avg = rollmean(daily_cases, 7, fill=NA, align="right")) %>%
  mutate(daily_deaths = deaths - lag(deaths, default = deaths[1]))%>%
  mutate(daily_deaths_avg = rollmean(daily_deaths, 7, fill=NA, align="right"))

nynj<- sqldf('select date, sum(cases) as cases, sum(deaths) as deaths from us_states where fips in (34,36) group by date') %>%
  mutate(daily_cases = cases - lag(cases, default = cases[1])) %>%
  mutate(daily_cases_avg = rollmean(daily_cases, 7, fill=NA, align="right")) %>%
  mutate(daily_deaths = deaths - lag(deaths, default = deaths[1]))%>%
  mutate(daily_deaths_avg = rollmean(daily_deaths, 7, fill=NA, align="right"))

northeast <- sqldf('select date, sum(cases) as cases, sum(deaths) as deaths from us_states where fips in (34,36,42,09,23,25,33,44,50) group by date') %>%
  mutate(daily_cases = cases - lag(cases, default = cases[1])) %>%
  mutate(daily_cases_avg = rollmean(daily_cases, 7, fill=NA, align="right")) %>%
  mutate(daily_deaths = deaths - lag(deaths, default = deaths[1]))%>%
  mutate(daily_deaths_avg = rollmean(daily_deaths, 7, fill=NA, align="right"))

south <- sqldf('select date, sum(cases) as cases, sum(deaths) as deaths from us_states where fips in  (1,5,10,11,12,13,21,21,24,28,37,40,45,47,48,51,54) group by date') %>%
  mutate(daily_cases = cases - lag(cases, default = cases[1])) %>%
  mutate(daily_cases_avg = rollmean(daily_cases, 7, fill=NA, align="right")) %>%
  mutate(daily_deaths = deaths - lag(deaths, default = deaths[1]))%>%
  mutate(daily_deaths_avg = rollmean(daily_deaths, 7, fill=NA, align="right"))

mw <- sqldf('select date, sum(cases) as cases, sum(deaths) as deaths from us_states where fips in  (17,18,26,39,55,19,20,27,29,31,38,46) group by date') %>%
  mutate(daily_cases = cases - lag(cases, default = cases[1])) %>%
  mutate(daily_cases_avg = rollmean(daily_cases, 7, fill=NA, align="right")) %>%
  mutate(daily_deaths = deaths - lag(deaths, default = deaths[1]))%>%
  mutate(daily_deaths_avg = rollmean(daily_deaths, 7, fill=NA, align="right"))

mwe <- sqldf('select date, sum(cases) as cases, sum(deaths) as deaths from us_states where fips in  (17,18,26,39,55) group by date') %>%
  mutate(daily_cases = cases - lag(cases, default = cases[1])) %>%
  mutate(daily_cases_avg = rollmean(daily_cases, 7, fill=NA, align="right")) %>%
  mutate(daily_deaths = deaths - lag(deaths, default = deaths[1]))%>%
  mutate(daily_deaths_avg = rollmean(daily_deaths, 7, fill=NA, align="right"))

mww <- sqldf('select date, sum(cases) as cases, sum(deaths) as deaths from us_states where fips in  (19,20,27,29,31,38,46) group by date') %>%
  mutate(daily_cases = cases - lag(cases, default = cases[1])) %>%
  mutate(daily_cases_avg = rollmean(daily_cases, 7, fill=NA, align="right")) %>%
  mutate(daily_deaths = deaths - lag(deaths, default = deaths[1]))%>%
  mutate(daily_deaths_avg = rollmean(daily_deaths, 7, fill=NA, align="right"))

mtn <- sqldf('select date, sum(cases) as cases, sum(deaths) as deaths from us_states where fips in  (08,16,30,32,49,56) group by date')%>%
  mutate(daily_cases = cases - lag(cases, default = cases[1])) %>%
  mutate(daily_cases_avg = rollmean(daily_cases, 7, fill=NA, align="right")) %>%
  mutate(daily_deaths = deaths - lag(deaths, default = deaths[1]))%>%
  mutate(daily_deaths_avg = rollmean(daily_deaths, 7, fill=NA, align="right"))

mtnsw <- sqldf('select date, sum(cases) as cases, sum(deaths) as deaths from us_states where fips in  (08,16,30,32,49,56,4,35) group by date')%>%
  mutate(daily_cases = cases - lag(cases, default = cases[1])) %>%
  mutate(daily_cases_avg = rollmean(daily_cases, 7, fill=NA, align="right")) %>%
  mutate(daily_deaths = deaths - lag(deaths, default = deaths[1]))%>%
  mutate(daily_deaths_avg = rollmean(daily_deaths, 7, fill=NA, align="right"))

# aznm <- sqldf('select date, sum(cases) as cases, sum(deaths) as deaths from us_states where fips in  (4,35) group by date')%>%
#   mutate(daily_cases = cases - lag(cases, default = cases[1])) %>%
#   mutate(daily_cases_avg = rollmean(daily_cases, 7, fill=NA, align="right")) %>%
#   mutate(daily_deaths = deaths - lag(deaths, default = deaths[1]))%>%
#   mutate(daily_deaths_avg = rollmean(daily_deaths, 7, fill=NA, align="right"))

cali <- sqldf('select date, sum(cases) as cases, sum(deaths) as deaths from us_states where fips in  (6) group by date')%>%
  mutate(daily_cases = cases - lag(cases, default = cases[1])) %>%
  mutate(daily_cases_avg = rollmean(daily_cases, 7, fill=NA, align="right")) %>%
  mutate(daily_deaths = deaths - lag(deaths, default = deaths[1]))%>%
  mutate(daily_deaths_avg = rollmean(daily_deaths, 7, fill=NA, align="right"))

pnw<- sqldf('select date, sum(cases) as cases, sum(deaths) as deaths from us_states where fips in  (41,53) group by date') %>%
  mutate(daily_cases = cases - lag(cases, default = cases[1])) %>%
  mutate(daily_cases_avg = rollmean(daily_cases, 7, fill=NA, align="right")) %>%
  mutate(daily_deaths = deaths - lag(deaths, default = deaths[1]))%>%
  mutate(daily_deaths_avg = rollmean(daily_deaths, 7, fill=NA, align="right"))

akhi <- sqldf('select date, sum(cases) as cases, sum(deaths) as deaths from us_states where fips in  (2,15) group by date')%>%
  mutate(daily_cases = cases - lag(cases, default = cases[1])) %>%
  mutate(daily_cases_avg = rollmean(daily_cases, 7, fill=NA, align="right")) %>%
  mutate(daily_deaths = deaths - lag(deaths, default = deaths[1]))%>%
  mutate(daily_deaths_avg = rollmean(daily_deaths, 7, fill=NA, align="right"))
  
# penn<-sqldf('select date, sum(cases) as cases, sum(deaths) as deaths from us_states where fips in  (42) group by date')%>%
#   mutate(daily_cases = cases - lag(cases, default = cases[1])) %>%
#   mutate(daily_cases_avg = rollmean(daily_cases, 7, fill=NA, align="right")) %>%
#   mutate(daily_deaths = deaths - lag(deaths, default = deaths[1]))%>%
#   mutate(daily_deaths_avg = rollmean(daily_deaths, 7, fill=NA, align="right"))
# 
# new<-sqldf('select date, sum(cases) as cases, sum(deaths) as deaths from us_states where fips in  (09,23,25,33,44,50) group by date')%>%
#   mutate(daily_cases = cases - lag(cases, default = cases[1])) %>%
#   mutate(daily_cases_avg = rollmean(daily_cases, 7, fill=NA, align="right")) %>%
#   mutate(daily_deaths = deaths - lag(deaths, default = deaths[1]))%>%
#   mutate(daily_deaths_avg = rollmean(daily_deaths, 7, fill=NA, align="right"))

by_region<-ggplot() +
  geom_line(data=south, aes(x=date, y=daily_deaths_avg, color="South")) +
  geom_line(data=mw, aes(x=date, y=daily_deaths_avg, color="Midwest")) +
  # geom_line(data=mwe, aes(x=date, y=daily_deaths_avg, color="Midwest(East)")) +
  # geom_line(data=mww, aes(x=date, y=daily_deaths_avg, color="Midwest(Plains)")) +
  # geom_line(data=ny, aes(x=date, y=daily_deaths_avg, color="New York")) +
  # geom_line(data=nj, aes(x=date, y=daily_deaths_avg, color="New Jersey")) +
  geom_line(data=northeast, aes(x=date, y=daily_deaths_avg, color="Northeast")) +
  geom_line(data=mtnsw, aes(x=date, y=daily_deaths_avg, color="Mountain+SW")) +
  # geom_line(data=aznm, aes(x=date, y=daily_deaths_avg, color="AZ+NM")) +
  geom_line(data=cali, linetype='solid', aes(x=date, y=daily_deaths_avg, color="California")) +
  geom_line(data=pnw, aes(x=date, y=daily_deaths_avg, color="Pacific NW")) +
  geom_line(data=akhi, aes(x=date, y=daily_deaths_avg, color="AK+HI")) +
  # geom_line(data=penn, linetype='dashed', aes(x=date, y=daily_deaths_avg, color="Pennsylvania")) +
  # geom_line(data=new, aes(x=date, y=daily_deaths_avg, color="New England")) + 
  # scale_colour_manual("",breaks=c("South","Midwest","Midwest(East)","Midwest(Plains)","New York","New Jersey","NY+NJ","Northeast","Mountain","Mountain+SW", "AZ+NM", "California", "Pacific NW", "AK+HI", "Pennsylvania", "New England"), values= cbp4)+
  annotation_logticks() +
  theme_minimal() +
  # scale_linetype_manual(breaks= c('dashed'),values = 'dashed') +
  scale_y_log10(breaks=c(1,10,100,1000), minor_breaks=c(5,50,500))
  

nonynj = filter(us_states, !(state %in% c("New York", "New Jersey")))

us_byday <- aggregate(cbind(cases_usa=us_states$cases, deaths_usa=us_states$deaths), by=list(date=us_states$date), FUN=sum)%>%
  mutate(daily_cases_usa = cases_usa - lag(cases_usa, default = cases_usa[1])) %>%
  mutate(daily_cases_avg_usa = rollmean(daily_cases_usa, 7, fill=NA, align="right")) %>%
  mutate(daily_deaths_usa = deaths_usa - lag(deaths_usa, default = deaths_usa[1]))%>%
  mutate(daily_deaths_avg_usa = rollmean(daily_deaths_usa, 7, fill=NA, align="right"))

south_byday = south %>%
  mutate(daily_cases.south = cases - lag(cases, default = cases[1])) %>%
  mutate(daily_cases_avg.south = rollmean(daily_cases.south, 7, fill=NA, align="right")) %>%
  mutate(daily_deaths.south = deaths - lag(deaths, default = deaths[1]))%>%
  mutate(daily_deaths_avg.south = rollmean(daily_deaths.south, 7, fill=NA, align="right"))

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
  labs(title = "Daily COVID-19 Cases and Deaths in the US (to June 6)",
       caption = "Points are cases and deaths reported each day.\nLines are 7-day running averages.\nGraph by Donovan Richardson\nData from The New York Times, based on reports from state and local health agencies.") +
  scale_y_log10(breaks=c(1,10,100,1000,10000), minor_breaks=c(5,50,500,5000,50000)) +
  annotation_logticks() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0))
by_region
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
  labs(title = "Daily COVID-19 Deaths in Selected States (to June 6)",
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
