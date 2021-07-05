# Installing required libraries
library('dplyr')
library('sjmisc')
library('tidyverse')

# Import COIVD-19 daily deaths/cases data

# Importing county level cases/deaths
# https://www.kaggle.com/headsortails/covid19-us-county-jhu-data-demographics?select=us_county.csv
input2 = read.csv('C:/Users/info/Downloads/covid_us_county.csv')
str(input2)
input2$date = as.Date(input2$date, '%Y-%m-%d')

str(input2)
min(input2$date)
max(input2$date)

# Converting cumulative numbers to daily aggregates
input3 = input2
str(input3)
input2$date = input2$date + 1
min(input3$date)
max(input3$date)

colnames(input2) = c("fips", "county", "state", "lat", "long", "date", "cases_pyc", "state_code", "deaths_pyc")
input4 = left_join(input3, input2, by = c("fips", "county", "state", "lat", "long", "date", "state_code"))

input4$dailycases = input4$cases - input4$cases_pyc
input4$dailydeaths = input4$deaths - input4$deaths_pyc
input5 = input4[,c("fips", "county", "state", "lat", "long", "date", "state_code", "cases", "deaths", "dailycases", "dailydeaths")]

write.csv(input5, 'C:/Users/info/Downloads/covid_us_county_rolled.csv', row.names = F)

# Importing country level SVI, CVAC & Hesitancy along with % Popuation vaccinated
# Source - https://data.cdc.gov/Vaccinations/Vaccine-Hesitancy-for-COVID-19-County-and-local-es/q9mh-h2tw
hesitancy = read.csv('C:/Users/info/Downloads/Vaccine_Hesitancy_for_COVID-19__County_and_local_estimates.csv')

str(hesitancy)
# Aggregating hesitancy by County
hesi = hesitancy%>%
  group_by(FIPS.Code, County.Name, State)%>%
  summarise(hesitant = mean(Estimated.hesitant, na.rm = T),
            strong_hesitant = mean(Estimated.strongly.hesitant, na.rm = T))
colnames(hesi) = c("fips", "county", "state", "hesitant", "strong_hesitant")

# Identifying correlation, if any, between cases, deaths & hesitancy
death_by_hesi = full_join(death_cases_county_state, hesi, by = 'fips')
death_by_hesi2 = na.omit(death_by_hesi)
cor(death_by_hesi2[,c("dailycases","dailydeaths","hesitant","strong_hesitant")])


