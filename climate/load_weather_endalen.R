#############################################
#### CLEAN RAW HOBO WEATHER STATION DATA ####
#############################################

#LOAD LIBRARIES#
library("tidyverse")
library("lubridate")
library("readxl")
library("ggplot2")
#devtools::install_github("Between-the-Fjords/dataDownloader")
library("dataDownloader")

#### Download raw data form OSF ####
source("climate/Download_Raw_Climate_Data.R")


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
    date_time = Date,
    par = `PAR, uE (LGR S/N: 2271742, SEN S/N: 2008892, LBL: Weatherstn_SSE)`,
    wat_cont = `Water Content, m³/m³ (LGR S/N: 2271742, SEN S/N: 2268271, LBL: Weatherstn_SSE)`,
    temp = `Temp, °C (LGR S/N: 2271742, SEN S/N: 10655574, LBL: Weatherstn_SSE)`,
    rh = `RH, % (LGR S/N: 2271742, SEN S/N: 10655574, LBL: Weatherstn_SSE)`,
    sol_rad = `Solar Radiation, W/m² (LGR S/N: 2271742, SEN S/N: 10663215, LBL: Weatherstn_SSE)`)

# Reading in rest of csv and xls files - into one dataframe
hobo <- list.files(path = "climate/data/DATA_ITEX_2015_2018/Hobo_weather st/", recursive = TRUE, full.names = TRUE, pattern = "csv$|\\.xls") %>% 
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
    date_time = lubridate::mdy_hms(date_time)) %>% 
  select(-Date, -`Date Time, GMT+00:00`, -`Time, GMT+00:00`) %>% 
  rename(
    par = `PAR, uE (LGR S/N: 2271742, SEN S/N: 2008892, LBL: Weatherstn_SSE)`,
    wat_cont = `Water Content, m³/m³ (LGR S/N: 2271742, SEN S/N: 2268271, LBL: Weatherstn_SSE)`,
    temp = `Temp, °C (LGR S/N: 2271742, SEN S/N: 10655574, LBL: Weatherstn_SSE)`,
    rh = `RH, % (LGR S/N: 2271742, SEN S/N: 10655574, LBL: Weatherstn_SSE)`,
    sol_rad = `Solar Radiation, W/m² (LGR S/N: 2271742, SEN S/N: 10663215, LBL: Weatherstn_SSE)`
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
    wat_cont = if_else(wat_cont < -100, NA_real_, wat_cont)
  ) %>% 
  # remove duplicates
  group_by(date_time, par, wat_cont, temp, rh, sol_rad) %>% 
  mutate(n = 1:n()) %>% 
  filter(n == 1) %>% 
  select(-n) %>% 
  # remove last day
  filter(!grepl("2018-09-19", date_time))


### Plotting and checking data
# ggplot(data = hobo)+
#   geom_point(mapping = aes(x = date_time, y = par, color = file), show.legend = FALSE)
# 
# ggplot(data = hobo)+
#   geom_point(mapping = aes(x = date_time, y = wat_cont, color = file), show.legend = FALSE)
# 
# ggplot(data = hobo)+
#   geom_point(mapping = aes(x = date_time, y = rh, color = file), show.legend = FALSE)
# 
# ggplot(data = hobo)+
#   geom_point(mapping = aes(x = date_time, y = temp, color = file), show.legend = FALSE)
# 
# ggplot(data = hobo)+
#   geom_point(mapping = aes(x = date_time, y = sol_rad, color = file), show.legend = FALSE)


#### CALCULATE DAILY AND MONTHLY MEANS ####

# Daily
day_hobo <- hobo %>% 
  ungroup() %>% 
  select(-file) %>% 
  pivot_longer(col = c(par, wat_cont, temp, rh, sol_rad), names_to = "variable", values_to = "value") %>% 
  filter(!is.na(value)) %>%
  separate(col = "date_time", into= c("date", "time"), sep = " ") %>% 
  mutate(date = ymd(date)) %>% 
  group_by(date, variable) %>% 
  summarise(n = n(), dayValue = mean(value))


# Monthly
month_hobo <- day_hobo %>% 
  filter(!is.na(dayValue)) %>% 
  ungroup() %>% 
  mutate(date = dmy(paste0("15-", format(date , "%b.%Y")))) %>% # sets all the dates of the month to the 15th.
  group_by(date, variable) %>% 
  summarise(n = n(), monthValue = mean(dayValue))
