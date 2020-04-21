#### DOWNLOAD RAW COMMUNITY DATA FROM OSF ####
source(file = "rawdataCleaning/Download_Raw_Data.R")


# LOAD LIBRARIES
library("readxl")
library("tidyverse")
library("lubridate")
#devtools::install_github("gustavobio/tpldata")
#devtools::install_github("gustavobio/tpl")
library("tpl")
library("taxize")
library("googlesheets4")

pn <- . %>% print(n = Inf)


# Read in data
#community_raw <- read_sheet(ss = "1l4YHi32UlICPrC1U6INvIT3z_L2rEXWVvasqBmJJ_BA", sheet = "DATA") # directly from google sheet
communityRaw <- read_csv(file = "community/data/PFTC4_Svalbard_2018_Community.csv", col_names = TRUE)
drabas <- read_xlsx(path = "community/data/PFTC4_Svalbard_2018_Draba_dictionary.xlsx")


# Create a community structure file
metaCommunitySV_2018 <- communityRaw %>%
  select(Day, Site, Elevation, Plot, MedianHeight_cm, Max_height_cm, Vascular, Bryophytes, Lichen_soil, Lichen_rock, Rock, BareGround, BioCrust, Litter, Weather, PlotSize_cm2, Aspect, Slope_percent, Notes, Entered_by, Collected_by) %>% 
  filter(!is.na(Site)) %>% # remove empty line
  mutate(Country = "SV",
         Year = 2018,
         Elevation = as.character(Elevation)) %>% 
  rename(Gradient = Site, Site = Elevation, PlotID = Plot)
#write_csv(metaCommunitySV_2018, file = "community/data/metaCommunitySV_2018.csv", col_names = TRUE)


# Extra Draba data from Finns
drabas <- drabas %>% 
  gather(key = Taxon2, value = occurence, -site, -transect, -plot) %>% 
  filter(occurence > 0) %>% 
  rename(Site = site, Elevation = transect, Plot = plot) %>% 
  select(-occurence) %>% 
  mutate(Taxon2 = ifelse(Taxon2 == "No Drabas", "Cerastium arcticum", Taxon2))
#mutate(Cover_Fertile = "0.1_1")


# community
communitySV_2018 <- communityRaw %>%
  select(-GPS_Nr, -Lat_N, -Long_E, -Scat_species, -MedianHeight_cm, -Max_height_cm, -Vascular, -Bryophytes, -Lichen_soil, -Lichen_rock, -Rock, -BareGround, -BioCrust, -Litter, -Weather, -Elevation_m, -PlotSize_cm2, -Aspect, -Slope_percent, -GPSUnitAccuracy) %>% 
  pivot_longer(cols = c(-Entered_by, -Collected_by, -Day, -Site, -Elevation, -Plot, -Notes), names_to = "Taxon", values_to = "Cover_Fertile") %>% 
  filter(!is.na(Cover_Fertile)) %>% 
  separate(col = Cover_Fertile, into = c("Cover", "Fertile"), sep = "_") %>% 
  # remove white space in Cover column
  mutate(Cover = gsub(" ", "", Cover),
         Cover = as.numeric(Cover),
         Fertile = as.numeric(Fertile)) %>% 
  
  # Replace Draba sp1 and sp2 with Julias Draba list
  left_join(drabas, by = c("Site", "Elevation", "Plot")) %>% 
  mutate(Taxon = ifelse(Taxon %in% c("Draba sp1", "Draba sp2", "Draba nivalis", "Draba oxycarpa"), Taxon2, Taxon)) %>% 
  mutate(Taxon = tolower(Taxon)) %>% 
  # Rename species
  mutate(Taxon = ifelse(Taxon == "poa alpigena vivipara", "poa arctica_x_pratensis", Taxon),
                  Taxon = ifelse(Taxon == "cochleria groenlandica", "cochlearia groenlandica", Taxon),
                  Taxon = ifelse(Taxon == "micranthes hieracifolia", "micranthes hieraciifolia", Taxon)) %>% 
  mutate(Elevation = as.character(Elevation)) %>% 
  rename("PlotID" = "Plot", "Gradient" = "Site", "Site" = "Elevation") %>% 
  mutate(Country = "SV",
         Year = 2018,
         Project = "T") %>% 
  # 9 do not match = ITEX
  left_join(coords, by = c("Project", "Gradient" = "Treatment", "Site")) %>% 

 # reorder
  select(Country, Year, Day, Project, Latitude_N, Longitude_E, Elevation_m, Site, Gradient, PlotID, Taxon, Cover, Fertile, Notes, Collected_by, Entered_by) %>% 
  
  # Fix stuff
  mutate(Cover = if_else(Cover == 0, 0.1, Cover)) %>% # one observation has Cover 0, change to 0.1
  group_by(Country, Year, Site, Gradient, PlotID, Taxon, Cover) %>% 
  mutate(n = n()) %>% 
  # remove duplicates that are identical
  slice(1) %>% 
  ungroup() %>% 
  # remove  duplicate where cover is not identical
  filter(!(Gradient == "C" & Site == "5" & PlotID == "D" & Taxon == "cerastium arcticum" & Cover == 0.1))

write_csv(communitySV_2018, path = "community/cleaned_data/PFTC4_Svalbard_2018_Community_cleaned.csv", col_names = TRUE)



# Checks
# checkCommNames <- tpl.get(unique(communityRaw1$Taxon))
# checkCommNames %>% 
#   filter(note == "was misspelled|replaced synonym") %>% select(name, original.search)






