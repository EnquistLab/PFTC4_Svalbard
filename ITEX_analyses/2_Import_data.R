#### IMPORT DATA #### 

# packages
library("tidyverse")
library("lubridate")
library("vegan")
library("ggvegan")
library("readxl")
library("broom")
library("lme4")
library("patchwork")

# import community data
CommunitySV_ITEX_2003_2015 <- read_csv(file = "community/cleaned_data/ITEX_Svalbard_2003_2015_Community_cleaned.csv") %>% 
  # rename sites
  mutate(Site = case_when(Site == "BIS" ~ "SB",
                          Site == "CAS" ~ "CH",
                          Site == "DRY" ~ "DH"))


# import height data
height_raw <- read_excel(path = "community/data/ENDALEN_ALL-YEARS_TraitTrain.xlsx", sheet = "HEIGHT")



# import climate data
# weather station data
WeatherStation <- read_csv(file = "climate/data_clean/ItexSvalbard_Climate_2015_2018.csv")

# plot level temperature loggers
ItexSvalbard_Temp_2005_2015 <- read_csv(file = "climate/data_clean/ItexSvalbard_Temp_2005_2015.csv") %>% 
  mutate(Site = case_when(Site == "BIS" ~ "SB",
                          Site == "CAS" ~ "CH",
                          Site == "DRY" ~ "DH"))