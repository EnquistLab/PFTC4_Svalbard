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

# Create new folder if not there yet
ifelse(!dir.exists("climate/data_clean"), dir.create("climate/data_clean"), FALSE)

write_csv(WeatherStation, path = "climate/data_clean/ItexSvalbard_Climate_2015_2018.csv", col_names = TRUE)

#### JOIN IBUTTON AND TINY TAGS DATA ####
ItexSvalbard_Temp_2005_2015 <- TinyTag %>% 
  bind_rows(ibutton) %>% 
  mutate(Unit = if_else(is.na(Unit), "C", Unit))
  
write_csv(ItexSvalbard_Temp_2005_2015, path = "climate/data_clean/ItexSvalbard_Temp_2005_2015.csv", col_names = TRUE)

# Check
# ItexSvalbard_Temp_2005_2015 %>% 
#   filter(Type == "soil") %>% 
#   ggplot(aes(x = DateTime, y = Value, colour = Logger)) +
#   geom_line() +
#   facet_grid(Treatment ~ Site)
