#### MAKING FIGURES AND TEST OTC VS CTL ####

source("climate/R/Merge_ClimateData.R")

library("broom")
library("lme4")

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
DailyClimatePlot <- ggplot(dailyClimate)  + 
  geom_line(aes(x = Date, y = Value)) +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(x = "") +
  theme_bw()
ggsave(DailyClimatePlot, filename = "DailyClimate.png")