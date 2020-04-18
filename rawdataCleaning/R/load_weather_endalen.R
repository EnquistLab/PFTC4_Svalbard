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
  map_df(~ mutate(.x, across(-matches("Date|Time"), as.numeric)), .id = "file") %>% 
  mutate(
    DateTime = if_else(!is.na(Date), paste(Date,  `Time, GMT+00:00`), NA_character_),
    DateTime = coalesce(DateTime, `Date Time, GMT+00:00`), 
    DateTime = lubridate::mdy_hms(DateTime)) %>% 
  select(-Date, -`Date Time, GMT+00:00`, -`Time, GMT+00:00`) %>% 
  rename(
    PAR = `PAR, uE (LGR S/N: 2271742, SEN S/N: 2008892, LBL: Weatherstn_SSE)`,
    WaterContent = `Water Content, m³/m³ (LGR S/N: 2271742, SEN S/N: 2268271, LBL: Weatherstn_SSE)`,
    Temperature = `Temp, °C (LGR S/N: 2271742, SEN S/N: 10655574, LBL: Weatherstn_SSE)`,
    RelHumidity = `RH, % (LGR S/N: 2271742, SEN S/N: 10655574, LBL: Weatherstn_SSE)`,
    SolarRadiation = `Solar Radiation, W/m² (LGR S/N: 2271742, SEN S/N: 10663215, LBL: Weatherstn_SSE)`) %>%  
  # combine species case
  bind_rows(specialFile) %>% 
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
# ggplot(data = WeatherStation)+
#   geom_point(mapping = aes(x = DateTime, y = PAR, color = file), show.legend = FALSE)
# 
# ggplot(data = WeatherStation)+
#   geom_point(mapping = aes(x = DateTime, y = WaterContent, color = file), show.legend = FALSE)
# 
# ggplot(data = WeatherStation)+
#   geom_point(mapping = aes(x = DateTime, y = RelHumidity, color = file), show.legend = FALSE)
# 
# ggplot(data = WeatherStation)+
#   geom_point(mapping = aes(x = DateTime, y = Temperature, color = file), show.legend = FALSE)
# 
# ggplot(data = WeatherStation)+
#   geom_point(mapping = aes(x = DateTime, y = SolarRadiation, color = file), show.legend = FALSE)
