#### IMPORT DATA #### 

# packages
library("tidyverse")
library("vegan")
library("ggvegan")
library("readxl")
library("broom")


# import community data
CommunitySV_ITEX_2003_2015 <- read_csv(file = "community/cleaned_data/ITEX_Svalbard_2003_2015_Community_cleaned.csv") %>% 
  # rename sites
  mutate(Site = case_when(Site == "BIS" ~ "SB",
                          Site == "CAS" ~ "CH",
                          Site == "DRY" ~ "DH"))


# import height data
height_raw <- read_excel(path = "community/data/ENDALEN_ALL-YEARS_TraitTrain.xlsx", sheet = "HEIGHT")