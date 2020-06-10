counties <- read_csv("us-counties.csv")
cpop <- read_csv("county_pop.csv")

View(sqldf("select counties.*, cpop.popestimate2019 from counties left join cpop on counties.fips = cpop.fips"))

idx = 0;
countydatalist <- list()

for(i in cpop$Fips){
  idx = idx+1
  dat <- sqldf(paste0('select * from counties where fips=\'',i,"'")) %>% mutate(daily_cases = cases - lag(cases, default = cases[1])) %>%
    mutate(daily_cases_avg = rollmean(daily_cases, 7, fill=NA, align="right")) %>%
    mutate(daily_deaths = deaths - lag(deaths, default = deaths[1]))%>%
    mutate(daily_deaths_avg = rollmean(daily_deaths, 7, fill=NA, align="right"))
  countydatalist[[idx]] <- dat
}

big_county <- do.call(rbind, countydatalist)