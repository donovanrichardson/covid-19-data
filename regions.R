library(sqldf)
library(dplyr)
library(readr)
library(zoo) #for rollmean
library(ggplot2)

# View(sqldf('select distinct state, fips from us_states'))
us_states <- read_csv("us-states.csv")
us_states$fips <- as.numeric(us_states$fips) #technically this is incorrect, but I did not want to put quotes and zeroes in front of all the single digit numbers

# I should fix the legend

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

cap_form <- paste0(c(
  strwrap("Lines are 7-day running averages.", 85),
  strwrap("Graph by Donovan Richardson",85),
  strwrap("Data from The New York Times, based on reports from state and local health agencies.",85),
  strwrap("Midwest: Ohio, Indiana, Michigan, Illinois, Wisconsin, Minnesota, Iowa, Missouri, North Dakota, South Dakota, Nebraska, Kansas",85),
  strwrap("Mountain+SW: Idaho, Montana, Nevada, Utah, Wyoming, Colorado, Arizona, New Mexico",85),
  strwrap("Northeast: Maine, New Hampshire, Vermont, Massachusetts, Rhode Island, Connecticut, New York, New Jersey, Pennsylvania",85),
  strwrap("Pacific NW: Washington, Oregon",85),
  strwrap("South: Maryland, Delaware, DC, Virginia, West Virginia, North Carolina, South Carolina, Georgia, Florida, Kentucky, Tennessee, Alabama, Mississippi, Louisiana, Arkansas, Oklahoma, Texas", 85)), sep="", collapse="\n")

by_region<-ggplot() +
  geom_line(data=south[!south$date<as.Date('2020-02-29'),], aes(x=date, y=daily_deaths_avg, color="South")) +
  geom_line(data=mw[!mw$date<as.Date('2020-02-29'),], aes(x=date, y=daily_deaths_avg, color="Midwest")) +
  # geom_line(data=mwe, aes(x=date, y=daily_deaths_avg, color="Midwest(East)")) +
  # geom_line(data=mww, aes(x=date, y=daily_deaths_avg, color="Midwest(Plains)")) +
  # geom_line(data=ny, aes(x=date, y=daily_deaths_avg, color="New York")) +
  # geom_line(data=nj, aes(x=date, y=daily_deaths_avg, color="New Jersey")) +
  geom_line(data=northeast[!northeast$date<as.Date('2020-02-29'),], aes(x=date, y=daily_deaths_avg, color="Northeast")) +
  geom_line(data=mtnsw[!mtnsw$date<as.Date('2020-02-29'),], aes(x=date, y=daily_deaths_avg, color="Mountain+SW")) +
  # geom_line(data=aznm, aes(x=date, y=daily_deaths_avg, color="AZ+NM")) +
  geom_line(data=cali[!cali$date<as.Date('2020-02-29'),], linetype='solid', aes(x=date, y=daily_deaths_avg, color="California")) +
  geom_line(data=pnw[!pnw$date<as.Date('2020-02-29'),], aes(x=date, y=daily_deaths_avg, color="Pacific NW")) +
  geom_line(data=akhi[!akhi$date<as.Date('2020-02-29'),], aes(x=date, y=daily_deaths_avg, color="AK+HI")) +
  # geom_line(data=penn, linetype='dashed', aes(x=date, y=daily_deaths_avg, color="Pennsylvania")) +
  # geom_line(data=new, aes(x=date, y=daily_deaths_avg, color="New England")) + 
  # scale_colour_manual("",breaks=c("South","Midwest","Midwest(East)","Midwest(Plains)","New York","New Jersey","NY+NJ","Northeast","Mountain","Mountain+SW", "AZ+NM", "California", "Pacific NW", "AK+HI", "Pennsylvania", "New England"), values= cbp4)+
  annotation_logticks() +
  theme_minimal() +
  # scale_linetype_manual(breaks= c('dashed'),values = 'dashed') +
  scale_y_log10(breaks=c(1,10,100,1000), minor_breaks=c(5,50,500))+
  xlab("Date") +
  ylab("Daily Deaths (logarithmic scale)") +
  labs(title = paste("Daily COVID-19 Deaths by Region", toDate),
       caption = cap_form)+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, size=rel(.7)))

by_region

ggsave("deaths_regional.pdf",by_region)
