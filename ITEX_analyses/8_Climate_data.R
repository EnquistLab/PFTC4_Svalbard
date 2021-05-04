#### MAKING FIGURES AND TEST OTC VS CTL ####

## ----ClimateTempPlot
library("tidyverse")
library("lubridate")
library("broom")
library("lme4")
library("patchwork")

# import data
# weather station data
WeatherStation <- read_csv(file = "climate/data_clean/ItexSvalbard_Climate_2015_2018.csv")

# plot level temperature loggers
ItexSvalbard_Temp_2005_2015 <- read_csv(file = "climate/data_clean/ItexSvalbard_Temp_2005_2015.csv") %>% 
  mutate(Site = case_when(Site == "BIS" ~ "SB",
                          Site == "CAS" ~ "CH",
                          Site == "DRY" ~ "DH"))


#### CALCULATE DAILY AND MONTHLY VALUES ####

# Temperature data
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


# Weather station data
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



# Make ITEX Figure 

meta <- monthlyTemp %>% ungroup() %>% distinct(PlotID, Site, Treatment, Type)
MonthlyTemp <- tibble(YearMonth = seq(from = as.Date("2004-09-15"), 
                                      to = as.Date("2018-06-15"), 
                                      by = "month")) %>% 
  crossing(meta) %>% 
  left_join(monthlyTemp, by = c("YearMonth", "PlotID", "Site", "Treatment", "Type")) %>% 
  mutate(Year = year(YearMonth),
         Date2 = ymd(paste(2020, month(YearMonth), day(YearMonth))),
         Type = factor(Type, levels = c("surface", "soil"))) %>% 
  filter(Year %in% c(2004, 2005, 2015:2018)) %>% 
  ggplot(aes(x = Date2, y = Value, group = interaction(Year, PlotID), colour = as.factor(Year), linetype = Treatment)) +
  geom_line() +
  labs(x = "", y = "Monthly temperature in °C") +
  scale_x_date(date_labels = "%b") +
  ggtitle("B") +
  facet_grid(Type ~ Site) +
  theme_bw()

# Test OTC vs CTL in the summer
monthlyTemp %>% 
  mutate(Year = year(YearMonth),
         Month = lubridate::month(YearMonth)) %>% 
  filter(Month %in% c(6, 7, 8)) %>% 
  group_by(Site, Type, Month) %>% 
  nest() %>% 
  mutate(mod1 = map(data, ~ lmer(Value ~ Treatment + (1|PlotID), data = .x)),
         result1 = map(mod1, glance),
         mod2 = map(data, ~ lmer(Value ~ 1 + (1|PlotID), data = .x)),
         result2 = map(mod2, glance)) %>% 
  unnest(result1, result2) %>% 
  mutate(Diff = AIC - AIC1) %>% select(Diff) %>% filter(abs(Diff) > 2)
# Only for surface DRY is model including Treatment better.

# Weather station plot
DailyClimatePlot <- dailyClimate %>% 
  filter(Variable %in% c("Temperature", "WaterContent", "PAR")) %>% 
  ggplot() + 
  geom_line(aes(x = Date, y = Value)) +
  facet_wrap(~ Variable, scales = "free_y",
             #strip.position = "left",
             labeller = as_labeller(c(PAR = "PAR uE", Temperature = "Temperature °C", WaterContent = "Water Content m³/m³"))) +
  labs(x = "", y = NULL) +
  ggtitle("A") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.placement = "outside")

FinalItexPlot <- cowplot::plot_grid(DailyClimatePlot, MonthlyTemp, nrow = 2, scale = c(0.9, 1))
ggsave(FinalItexPlot, filename = "ITEX_analyses/output/Fig_S1_FinalClimatePlot.png", width = 10, height = 8, dpi = 150)
## ----

# Max temp
monthlyClimate %>% 
  filter(Variable == "Temperature") %>% 
  group_by(year(YearMonth)) %>% 
  summarise(max(Value))


# Temperature range
monthlyTemp %>% 
  filter(month(YearMonth) %in% c(1, 2, 3, 7),
         Treatment == "CTL") %>% 
  mutate(time = case_when(month(YearMonth) %in% c(1, 2, 3) ~ "winter",
                          TRUE ~ "summer")) %>% 
  group_by(Type, time) %>% 
  summarise(mean = mean(Value), 
            se = sd(Value)/sqrt(n()),
            min = min(Value),
            max = max(Value),
            diff = max - min)

monthlyTemp %>% 
  filter(month(YearMonth) %in% c(1, 2, 3, 7)) %>% 
  mutate(time = case_when(month(YearMonth) %in% c(1, 2, 3) ~ "winter",
                          TRUE ~ "summer")) %>% 
  group_by(Type, Treatment, time, Site) %>% 
  summarise(mean = mean(Value)) %>% 
  pivot_wider(names_from = Treatment, values_from = mean) %>% 
  mutate(diff = OTC - CTL)

res <- monthlyTemp %>% 
  filter(month(YearMonth) %in% c(1, 2, 3),
         Type == "surface")
fit <- lm(Value ~ Treatment, data = res)
summary(fit)
tidy(fit)

monthlyTemp %>% 
  filter(month(YearMonth) == 1) %>% 
  group_by(Type, Site) %>% 
  summarise(mean = mean(Value), min = min(Value), max = max(Value)) %>% 
  mutate(diff = max - min)