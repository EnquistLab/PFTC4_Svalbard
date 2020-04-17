####################################
#### DOWNLOAD RAW DATA FROM OSF ####
####################################

# Install packages and load libraries
#install.packages("remotes")
#remotes::install_github("Plant-Functional-Trait-Course/PFTCFunctions")
library("PFTCFunctions")
library("tidyverse")
library("lubridate")
library("readxl")
library("data.table")

pn <- . %>% print(n = Inf)

## Source files to download raw data form OSF ##
source("climate/R/Download_Raw_Climate_Data.R")

## Load and clean raw climate data
source("climate/R/load_TinyTags.R")
source("climate/R/load_weather_endalen.R")
source("climate/R/load_iButtons.R")


#### JOIN IBUTTON AND TINY TAGS DATA ####
ItexSvalbard_Temp_2005_2015 <- TinyTag %>% 
  bind_rows(ibutton) %>% 
  mutate(Unit = if_else(is.na(Unit), "C", Unit))
  
# Check
# ItexSvalbard_Temp_2005_2015 %>% 
#   filter(Type == "soil") %>% 
#   ggplot(aes(x = DateTime, y = Value, colour = Logger)) +
#   geom_line() +
#   facet_grid(Treatment ~ Site)



#### CALCULATE DAILY AND MONTHLY TEMPERATURE MEANS ####
# Daily temperature
threshold <- 1 # Remove days with very little data
#dailyTemperature <- 
dailyTemp <- ItexSvalbard_Temp_2005_2015 %>%
  mutate(DateTime = ymd_hms(DateTime)) %>% 
  mutate(Date = ymd(format(DateTime, "%Y-%m-%d"))) %>% 
  group_by(Date, Site, PlotID, Treatment, Type, Logger) %>%
  summarise(n = n(), Value = mean(Value)) %>% 
  filter(n > threshold) %>%
  select(-n)


threshold2 <- 14 # remove month with less than 15 data points
monthlyTemp <- dailyTemp %>%
  ungroup() %>% 
  mutate(YearMonth = dmy(paste0("15-",format(Date, "%b.%Y")))) %>%
  group_by(YearMonth, Type, Site, PlotID, Treatment, Logger) %>%
  summarise(n = n(), Value = mean(Value)) %>%
  filter(n > threshold2) %>%
  select(-n)


#### CALCULATE DAILY AND MONTHLY MEANS FROM WEATHER STATION ####
# Daily
dailyClimate <- WeatherStation %>% 
  ungroup() %>% 
  select(-file) %>% 
  pivot_longer(col = c(PAR, WaterContent, Temperature, RelHumidity, SolarRadiation), names_to = "Variable", values_to = "Value") %>% 
  filter(!is.na(Value)) %>%
  separate(col = "DateTime", into= c("Date", "Time"), sep = " ") %>% 
  mutate(Date = ymd(Date)) %>% 
  group_by(Date, Variable) %>% 
  summarise(n = n(), Value = mean(Value))


# Monthly
monthlyClimate <- dailyClimate %>% 
  filter(!is.na(Value)) %>% 
  ungroup() %>% 
  mutate(YearMonth = dmy(paste0("15-", format(Date , "%b.%Y")))) %>% # sets all the dates of the month to the 15th.
  group_by(YearMonth, Variable) %>% 
  summarise(n = n(), Value = mean(Value))


# Checks
# summerTemperature17 <- monthlyTemperature %>% 
#   filter(Date > "2017-05-15", Date < "2017-09-15") %>% 
#   group_by(Site, Treatment) %>% 
#   summarise(n = n(), temp = mean(value), sd = sd(value), se = sd / sqrt(n))
# 
# yearTemperature <- monthlyTemperature %>% 
#   filter(Date > "2017-05-15", Date < "2018-07-15") %>% 
#   group_by(Site, Treatment) %>% 
#   summarise(n = n(), temp = mean(value), sd = sd(value), se = sd / sqrt(n))
# 
# 
# ggplot(monthlyTemperature, aes(x = Date, y = value, colour = Treatment)) +
#   geom_point() +
#   facet_wrap( ~ Site)

