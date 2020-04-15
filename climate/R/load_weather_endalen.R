#############################################
#### CLEAN RAW HOBO WEATHER STATION DATA ####
#############################################


#### READ HOBO WEATHER STATION DATA (Endalen SSW) ####
# Function to read in files
read_xl_csv <- function(f){
  if(grepl("csv$", f)){
    read.csv(f, skip = 1, stringsAsFactors = FALSE, check.names = FALSE)
  } else if(grepl("xls", f)){
      read_excel(f, skip = 1, sheet = 1)
  } else {
    stop("unknown file type")
  }
}


# Special case
specialFile <- read_xlsx(path = "climate/data/DATA_ITEX_2015_2018/Hobo_weather st/2018 downloads/20180919/Endalen_Weatherstn_SSE.xlsx", skip = 1) %>% 
  select(-`#`, -`Time, GMT+00:00`) %>%
  mutate(file = "climate/data/DATA_ITEX_2015_2018/Hobo_weather st/2018 downloads/20180919/Endalen_Weatherstn_SSE.xlsx") %>% 
  rename(
    DateTime = Date,
    PAR = `PAR, uE (LGR S/N: 2271742, SEN S/N: 2008892, LBL: Weatherstn_SSE)`,
    WaterContent = `Water Content, m³/m³ (LGR S/N: 2271742, SEN S/N: 2268271, LBL: Weatherstn_SSE)`,
    Temperature = `Temp, °C (LGR S/N: 2271742, SEN S/N: 10655574, LBL: Weatherstn_SSE)`,
    RelHumidity = `RH, % (LGR S/N: 2271742, SEN S/N: 10655574, LBL: Weatherstn_SSE)`,
    SolarRadiation = `Solar Radiation, W/m² (LGR S/N: 2271742, SEN S/N: 10663215, LBL: Weatherstn_SSE)`)

# Reading in rest of csv and xls files - into one dataframe
WeatherStation <- list.files(path = "climate/data/DATA_ITEX_2015_2018/Hobo_weather st/", recursive = TRUE, full.names = TRUE, pattern = "csv$|\\.xls") %>% 
  grep(pattern = "20180919", x = ., invert = TRUE, value = TRUE) %>% # special case
  grep(pattern = "SSE", x = ., value = TRUE) %>% 
  grep(pattern = "~",x = ., invert = TRUE, value = TRUE) %>% 
  set_names(.) %>% 
  map(read_xl_csv) %>% 
  map(select, -`#`) %>% 
  map(mutate, `PAR, uE (LGR S/N: 2271742, SEN S/N: 2008892, LBL: Weatherstn_SSE)` = gsub(",", "", `PAR, uE (LGR S/N: 2271742, SEN S/N: 2008892, LBL: Weatherstn_SSE)`)) %>% 
  map_df(mutate_at, .vars = vars(-matches("Date|Time")), .fun = as.numeric, .id = "file") %>% 
  mutate(
    date_time = if_else(!is.na(Date), paste(Date,  `Time, GMT+00:00`), NA_character_),
    date_time = coalesce(date_time, `Date Time, GMT+00:00`), 
    DateTime = lubridate::mdy_hms(date_time)) %>% 
  select(-Date, -`Date Time, GMT+00:00`, -`Time, GMT+00:00`) %>% 
  rename(
    PAR = `PAR, uE (LGR S/N: 2271742, SEN S/N: 2008892, LBL: Weatherstn_SSE)`,
    WaterContent = `Water Content, m³/m³ (LGR S/N: 2271742, SEN S/N: 2268271, LBL: Weatherstn_SSE)`,
    Temperature = `Temp, °C (LGR S/N: 2271742, SEN S/N: 10655574, LBL: Weatherstn_SSE)`,
    RelHumidity = `RH, % (LGR S/N: 2271742, SEN S/N: 10655574, LBL: Weatherstn_SSE)`,
    SolarRadiation = `Solar Radiation, W/m² (LGR S/N: 2271742, SEN S/N: 10663215, LBL: Weatherstn_SSE)`
    #wat_cont2 = `Water Content, mÂ³/mÂ³ (LGR S/N: 2271742, SEN S/N: 2268271, LBL: Weatherstn_SSE)`, 
    #temp2 = `Temp, Â°C (LGR S/N: 2271742, SEN S/N: 10655574, LBL: Weatherstn_SSE)`,
    #sol_rad2 = `Solar Radiation, W/mÂ² (LGR S/N: 2271742, SEN S/N: 10663215, LBL: Weatherstn_SSE)`
    ) %>%  
  # combine species case
  bind_rows(specialFile) %>% 
  # this does not seem to be needed
  # mutate(
  #   wat_cont = coalesce(`wat_cont1`, `wat_cont2`), 
  #   temp = coalesce(temp1, temp2),
  #   sol_rad = coalesce(sol_rad1, sol_rad2),
  #   wat_cont = coalesce(wat_cont1, wat_cont2)) %>% 
  #select(-matches('\\d')) %>% 
  mutate(
    WaterContent = if_else(WaterContent < -100, NA_real_, WaterContent)
  ) %>% 
  # remove duplicates
  group_by(DateTime, PAR, WaterContent, Temperature, RelHumidity, SolarRadiation) %>% 
  mutate(n = 1:n()) %>% 
  filter(n == 1) %>% 
  select(-n) %>% 
  # remove last day
  filter(!grepl("2018-09-19", DateTime)) %>% 
  mutate(Logger = "WeatherStation")


### Plotting and checking data
# ggplot(data = hobo)+
#   geom_point(mapping = aes(x = DateTime, y = PAR, color = file), show.legend = FALSE)
# 
# ggplot(data = hobo)+
#   geom_point(mapping = aes(x = DateTime, y = WaterContent, color = file), show.legend = FALSE)
# 
# ggplot(data = hobo)+
#   geom_point(mapping = aes(x = DateTime, y = RelHumidity, color = file), show.legend = FALSE)
# 
# ggplot(data = hobo)+
#   geom_point(mapping = aes(x = DateTime, y = Temperature, color = file), show.legend = FALSE)
# 
# ggplot(data = hobo)+
#   geom_point(mapping = aes(x = DateTime, y = SolarRadiation, color = file), show.legend = FALSE)
