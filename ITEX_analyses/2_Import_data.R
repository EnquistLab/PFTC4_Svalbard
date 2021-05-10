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
  filter(!PlotID %in% c("CH-4", "CH-6", "CH-9", "CH-10"))


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