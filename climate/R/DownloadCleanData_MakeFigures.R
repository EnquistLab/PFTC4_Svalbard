#### MAKING FIGURES AND TEST OTC VS CTL ####

#install.packages("remotes")
#remotes::install_github("Plant-Functional-Trait-Course/PFTCFunctions")
library("PFTCFunctions")
library("tidyverse")
library("lubridate")
library("broom")
library("lme4")
library("patchwork")

# Download clean data
download_PFTC_data(country = "Svalbard", 
                   datatype = "climate", 
                   path = "climate/data_clean")

WeatherStation <- read_csv(file = "climate/data_clean/ItexSvalbard_Climate_2015_2018.csv")
ItexSvalbard_Temp_2005_2015 <- read_csv(file = "climate/data_clean/ItexSvalbard_Temp_2005_2015.csv")


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



# Make Figures
meta <- dailyTemp %>% ungroup() %>% distinct(PlotID, Site, Treatment, Type)
DailyTemp <- tibble(Date = seq(from = as.Date("2004-09-03"), 
                               to = as.Date("2018-07-10"), 
                               by = "days")) %>% 
  crossing(meta) %>% 
  left_join(dailyTemp, by = c("Date", "PlotID", "Site", "Treatment", "Type")) %>%
  mutate(Year = year(Date),
         Date2 = ymd(paste(2020, month(Date), day(Date)))) %>% 
  filter(Year %in% c(2004, 2005, 2015:2018)) %>% 
  ggplot(aes(x = Date2, y = Value, colour = as.factor(Year), linetype = Treatment)) +
  geom_line() +
  labs(x = "", y = "Daily temperature in °C") +
  facet_grid(Type ~ Site) +
  theme_bw()
#ggsave(DailyTemp, filename = "DailyTemp.png")


meta <- monthlyTemp %>% ungroup() %>% distinct(PlotID, Site, Treatment, Type)
MonthlyTemp <- tibble(YearMonth = seq(from = as.Date("2004-09-15"), 
                                      to = as.Date("2018-06-15"), 
                                      by = "month")) %>% 
  crossing(meta) %>% 
  left_join(monthlyTemp, by = c("YearMonth", "PlotID", "Site", "Treatment", "Type")) %>% 
  mutate(Year = year(YearMonth),
         Date2 = ymd(paste(2020, month(YearMonth), day(YearMonth)))) %>% 
  filter(Year %in% c(2004, 2005, 2015:2018)) %>% 
  ggplot(aes(x = Date2, y = Value, group = interaction(Year, PlotID), colour = as.factor(Year), linetype = Treatment)) +
  geom_line() +
  labs(x = "", y = "Monthly temperature in °C") +
  scale_x_date(date_labels = "%b") +
  facet_grid(Type ~ Site) +
  theme_bw()
#ggsave(MonthlyTemp, filename = "MonthlyTemp.png")

# Test OTC vs CTL
monthlyTemp %>% 
  mutate(Year = year(YearMonth)) %>% 
  group_by(Site, Type) %>% 
  nest() %>% 
  mutate(mod = map(data, ~ lmer(Value ~ Treatment + (1|PlotID), data = .x)), 
         result = map(mod, tidy)) %>% 
  unnest(result)


# Weather station plot
DailyClimatePlot <- dailyClimate %>% 
  filter(Variable %in% c("Temperature", "WaterContent", "PAR")) %>% 
  ggplot() + 
  geom_line(aes(x = Date, y = Value)) +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(x = "") +
  theme_bw()
#ggsave(DailyClimatePlot, filename = "DailyClimate.png")

FinalItexPlot <- DailyClimatePlot / MonthlyTemp
ggsave(FinalItexPlot, filename = "FinalClimatePlot.png", width = 10, height = 8)
