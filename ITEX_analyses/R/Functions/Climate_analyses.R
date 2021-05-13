### Climate data 

calculate_temperature_means <- function(temperature){
  
  # Calculate daily and monthly values
  
  # Temperature data
  # Daily temperature
  threshold <- 1 # Remove days with very little data
  #dailyTemperature <- 
  dailyTemp <- temperature %>%
    mutate(DateTime = ymd_hms(DateTime)) %>% 
    mutate(Date = ymd(format(DateTime, "%Y-%m-%d"))) %>% 
    group_by(Date, Site, PlotID, Treatment, LoggerLocation, Variable) %>%
    summarise(n = n(), Value = mean(Value)) %>% 
    filter(n > threshold) %>%
    select(-n)
  
  
  threshold2 <- 14 # remove month with less than 15 data points
  monthlyTemp <- dailyTemp %>%
    ungroup() %>% 
    mutate(YearMonth = dmy(paste0("15-",format(Date, "%b.%Y")))) %>%
    group_by(YearMonth, Site, PlotID, Treatment, LoggerLocation, Variable) %>%
    summarise(n = n(), Value = mean(Value)) %>%
    filter(n > threshold2) %>%
    select(-n)
  
  return(monthlyTemp)
  
}


calculate_climate_means <- function(climate){
  
  # Calculate daily values
  
  # Weather station data
  # Daily
  dailyClimate <- climate %>% 
    filter(!is.na(Value)) %>%
    separate(col = "DateTime", into= c("Date", "Time"), sep = " ") %>% 
    mutate(Date = ymd(Date)) %>% 
    group_by(Date, Variable) %>% 
    summarise(n = n(), Value = mean(Value))
  
  return(dailyClimate)
  
}



# Make Climate Figure
make_climate_figure <- function(monthlyTemp, dailyClimate){
  
  meta <- monthlyTemp %>% 
    ungroup() %>% 
    distinct(PlotID, Site, Treatment, LoggerLocation)
  
  MonthlyTemp <- tibble(YearMonth = seq(from = as.Date("2004-09-15"), 
                                        to = as.Date("2018-06-15"), 
                                        by = "month")) %>% 
    crossing(meta) %>% 
    left_join(monthlyTemp, by = c("YearMonth", "PlotID", "Site", "Treatment", "LoggerLocation")) %>% 
    mutate(Year = year(YearMonth),
           Date2 = ymd(paste(2020, month(YearMonth), day(YearMonth))),
           LoggerLocation = factor(LoggerLocation, levels = c("surface", "soil"))) %>% 
    filter(Year %in% c(2004, 2005, 2015:2018)) %>% 
    ggplot(aes(x = Date2, y = Value, group = interaction(Year, PlotID), colour = as.factor(Year), linetype = Treatment)) +
    geom_line() +
    labs(x = "", y = "Monthly temperature in °C") +
    scale_x_date(date_labels = "%b") +
    ggtitle("B") +
    facet_grid(LoggerLocation ~ Site) +
    theme_bw()
  
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
}


# Test monthly temperature in OTC vs CTL in the summer and winter

# 3 summer and 3 winter month, analyse per month
temperature_analysis <- function(monthlyTemp){
  
  temperature_models <- monthlyTemp %>% 
    mutate(Year = year(YearMonth),
           Month = lubridate::month(YearMonth)) %>% 
    #filter(Month %in% c(6, 7, 8)) %>% 
    filter(month(YearMonth) %in% c(1, 2, 3, 7, 8, 9)) %>%
    mutate(season = case_when(month(YearMonth) %in% c(1, 2, 3) ~ "winter",
                            TRUE ~ "summer")) %>%
    group_by(Site, LoggerLocation, season, Month) %>% 
    nest() %>% 
    mutate(mod1 = map(data, ~ lm(Value ~ Treatment, data = .x)),
           result1 = map(mod1, glance),
           mod2 = map(data, ~ lm(Value ~ 1, data = .x)),
           result2 = map(mod2, glance)
    )
  
  temperature_model_results <- bind_rows(Treatment_model = temperature_models %>%
              unnest(result1),
            Null_model = temperature_models %>% 
              unnest(result2),
            .id = "Model") %>% 
    select(Model, Site, LoggerLocation, season, Month, AIC) %>% 
    pivot_wider(names_from = "Model", values_from = "AIC") %>% 
    mutate(Model_Diff = Treatment_model - Null_model)
  # Only for surface DRY is model including Treatment better.
  
  return(temperature_model_results)
}






