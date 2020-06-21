library(sqldf)
library(dplyr)
library(readr)
library(zoo) #for rollmean
library(ggplot2)

refdate <- Sys.Date() -1

# Cases and deaths by state
us_states <- read_csv("us-states.csv")

# State populations, 2019 estimate
population <- read_csv("state_pop.csv")

# allows equality comparison
us_states$date <- as.character(us_states$date)

# gets state names
states <- sqldf('select distinct state, fips from us_states')

# https://stackoverflow.com/questions/29402528/append-data-frames-together-in-a-for-loop/29419402
datalist <- list()

# combines daily cases/deaths and rolling averages by state into one dataframe
for(i in states$state){
  
  dat <- sqldf(paste0('select * from us_states where state=\'',i,"'")) %>% mutate(daily_cases = cases - lag(cases, default = cases[1])) %>%
    mutate(daily_cases_avg = rollmean(daily_cases, 7, fill=NA, align="right")) %>%
    mutate(daily_deaths = deaths - lag(deaths, default = deaths[1]))%>%
    mutate(daily_deaths_avg = rollmean(daily_deaths, 7, fill=NA, align="right"))
  datalist[[i]] <- dat
}

covid_daily_states <- do.call(rbind, datalist)
row.names(covid_daily_states) <- c()

# ^^^ combines daily cases/deaths and rolling averages by state into one dataframe

# chooses the most severe states
# severe <- sqldf("select * from covid_daily_states where state in (select state from covid_daily_states where date = '2020-06-07' and daily_deaths_avg > 25)")

# severe$date <- as.Date(severe$date)

# normalization of covid_daily_states by population
covid_daily_norm <- sqldf("select covid_daily_states.*,daily_cases_avg/population.pop2019 as daily_cases_avg_per, daily_deaths_avg / population.pop2019 as daily_deaths_avg_per from covid_daily_states left join population on covid_daily_states.state = population.state")

# chooses the most severe states (normalized by pop)
# norm_severe <-sqldf("select * from covid_daily_norm where state in (select state from covid_daily_norm where date = '2020-06-07' and daily_deaths_avg_per > .000004)")

covid_daily_norm$date <- as.Date(covid_daily_norm$date)
# norm_severe$date <- as.Date(norm_severe$date)



# ggplot(severe[severe$date > '2020-02-29',], aes(x=date, y = daily_deaths_avg, group=state, color=state)) +geom_line() + ylim(0, 200) 
# ggplot(norm_severe[norm_severe$date > '2020-02-29',], aes(x=date, y = daily_deaths_avg_per, group=state, color=state)) +geom_line()+scale_y_log10()

# char compare
covid_daily_norm$date <- as.character(covid_daily_norm$date)

# population of US on Jul 1 2019
us2019pop <- 328239523

# Cases and deaths from COVID in USA
us <- read_csv("us.csv")


us_byday <- us%>%
  mutate(daily_cases = cases - lag(cases, default = cases[1])) %>%
  mutate(daily_cases_avg = rollmean(daily_cases, 7, fill=NA, align="right")) %>%
  mutate(daily_deaths = deaths - lag(deaths, default = deaths[1]))%>%
  mutate(daily_deaths_avg = rollmean(daily_deaths, 7, fill=NA, align="right"))

us_byday <- sqldf(sprintf("select us_byday.*,daily_cases_avg / %d as daily_cases_avg_per, daily_deaths_avg / %d as daily_deaths_avg_per from us_byday", us2019pop, us2019pop))

usa_norm <- data.frame(state = 'USA', fips=0, us_byday)

covid_daily_norm <- rbind(usa_norm, covid_daily_norm)
covid_daily_norm$weekly_cases_per100k <- covid_daily_norm$daily_cases_avg_per*7 * 100000
covid_daily_norm$weekly_deaths_per100k <- covid_daily_norm$daily_deaths_avg_per*7 * 100000

write.csv(covid_daily_norm, "state_trends/covid_normalized.csv")

covid_daily_norm$date <- as.character(covid_daily_norm$date)

ranking <- sqldf(sprintf("select date, state, daily_cases_avg, daily_cases_avg_per, daily_deaths_avg, daily_deaths_avg_per, weekly_cases_per100k, weekly_deaths_per100k from covid_daily_norm where date = '%s'", refdate))

# View(ranking)
# View(sqldf("select state, weekly_cases_per100k, weekly_deaths_per100k from ranking"))

# View(sqldf('pragma table_info(states)'))

covid_daily_norm$date <- as.Date(covid_daily_norm$date)
covid_daily_norm$fips <- as.numeric(covid_daily_norm$fips)

grap1 <- ggplot(covid_daily_norm[covid_daily_norm$date > '2020-02-29'&covid_daily_norm$fips %in%1:12,], aes(x=date, y = daily_deaths_avg, group=state, color=state)) +geom_line() + scale_y_log10()

grap2<- ggplot(covid_daily_norm[covid_daily_norm$date > '2020-02-29'&covid_daily_norm$fips %in%13:24,], aes(x=date, y = daily_deaths_avg, group=state, color=state)) +geom_line() + scale_y_log10()

grap3 <- ggplot(covid_daily_norm[covid_daily_norm$date > '2020-02-29'&covid_daily_norm$fips %in%24:36,], aes(x=date, y = daily_deaths_avg, group=state, color=state)) +geom_line() + scale_y_log10()

grap4 <- ggplot(covid_daily_norm[covid_daily_norm$date > '2020-02-29'&covid_daily_norm$fips %in%37:48,], aes(x=date, y = daily_deaths_avg, group=state, color=state)) +geom_line() + scale_y_log10()

grap5 <- ggplot(covid_daily_norm[covid_daily_norm$date > '2020-02-29'&covid_daily_norm$fips %in%49:80,], aes(x=date, y = daily_deaths_avg, group=state, color=state)) +geom_line() + scale_y_log10()

# grap1
# grap2
# grap3
# grap4
# grap5

cas1 <- ggplot(covid_daily_norm[covid_daily_norm$date > '2020-02-29'&covid_daily_norm$fips %in%1:12,], aes(x=date, y = daily_cases_avg, group=state, color=state)) +geom_line() + scale_y_log10()

cas2<- ggplot(covid_daily_norm[covid_daily_norm$date > '2020-02-29'&covid_daily_norm$fips %in%13:24,], aes(x=date, y = daily_cases_avg, group=state, color=state)) +geom_line() + scale_y_log10()

cas3 <- ggplot(covid_daily_norm[covid_daily_norm$date > '2020-02-29'&covid_daily_norm$fips %in%24:36,], aes(x=date, y = daily_cases_avg, group=state, color=state)) +geom_line() + scale_y_log10()

cas4 <- ggplot(covid_daily_norm[covid_daily_norm$date > '2020-02-29'&covid_daily_norm$fips %in%37:48,], aes(x=date, y = daily_cases_avg, group=state, color=state)) +geom_line() + scale_y_log10()

cas5 <- ggplot(covid_daily_norm[covid_daily_norm$date > '2020-02-29'&covid_daily_norm$fips %in%49:80,], aes(x=date, y = daily_cases_avg, group=state, color=state)) +geom_line() + scale_y_log10()

# cas1
# cas2
# cas3
# cas4
# cas5

# sqldf('select * from covid_daily_norm ')

ranking2 <-sqldf("select state, weekly_cases_per100k, weekly_deaths_per100k from ranking order by weekly_deaths_per100k desc")
View(ranking2)

write.csv(ranking2, "state_trends/summarized_normalized.csv")
