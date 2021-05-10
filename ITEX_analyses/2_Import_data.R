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
library("ggpubr")
library("ggfortify")
library("traitstrap")

# import community data
CommunitySV_ITEX_2003_2015 <- read_csv(file = "community/cleaned_data/PFTC4_Svalbard_2003_2015_ITEX_Community.csv") %>% 
  # remove iced Cassiope plots
  filter(!PlotID %in% c("CH-4", "CH-6", "CH-9", "CH-10"))

#import trait data
Svalbard_2018_ITEX_Traits <- read_csv(file = "traits/cleaned_data/PFTC4_Svalbard_2018_ITEX_Traits.csv") %>% 
  # remove iced Cassiope plots
  filter(!PlotID %in% c("CH-4", "CH-6", "CH-9", "CH-10")) %>% 
  # remove NP ratio from data. Not part of original analysis
  filter(Trait != "NP_ratio")


# import height data
veg_structure_raw <- read_csv(file = "community/cleaned_data/PFTC4_Svalbard_2003_2015_ITEX_Vegetation_Structure.csv")



# import temperature and climate data
# weather station data
ItexSvalbard_Temp_2005_2015 <- read_csv(file = "climate/data_clean/PFTC4_Svalbard_2005_2018_ITEX_Temperature.csv")
WeatherStation <- read_csv(file = "climate/data_clean/PFTC4_Svalbard_2015_2018_ITEX_Climate.csv")

                          