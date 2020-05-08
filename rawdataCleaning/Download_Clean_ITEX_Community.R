#### DOWNLOAD RAW TRAIT DATA FROM OSF ####
source(file = "rawdataCleaning/Download_Raw_Data.R")

# library
library("readxl")
library("tidyverse")
library("tpl")

# Read in files
ItexAbundance.raw <- read_excel(path = "community/data/ENDALEN_ALL-YEARS_TraitTrain.xlsx", sheet = "SP-ABUND")
ItexHeight.raw <- read_excel(path = "community/data/ENDALEN_ALL-YEARS_TraitTrain.xlsx", sheet = "HEIGHT")
sp <- read_excel(path = "community/data/Species lists_Iceland_Svalbard.xlsx", sheet = "Endalen")


# Species names
sp <- sp %>% 
  select(SPP, GFNARROWarft, GENUS, SPECIES) %>% 
  slice(-1) %>% 
  mutate(GFNARROWarft = tolower(GFNARROWarft)) %>% 
  rename(Spp = SPP, FunctionalGroup = GFNARROWarft, Genus = GENUS, Species = SPECIES)

# Community data
CommunitySV_ITEX_2003_2015 <- ItexAbundance.raw %>% 
  gather(key = Spp, value = Abundance, -SUBSITE, -TREATMENT, -PLOT, -YEAR, -`TOTAL-L`, -LITTER, -REINDRO, -BIRDRO, -ROCK, -SOIL, -CRUST) %>%
  filter(Abundance > 0) %>% 
  rename(Site = SUBSITE, Treatment = TREATMENT, PlotID = PLOT, Year = YEAR) %>% 
  mutate(Site2 = substr(Site, 5, 5),
         Site = substr(Site, 1, 3),
         PlotID = gsub("L", "", PlotID)) %>% 
  # I think we do not need/want H sites
  filter(Site2 == "L") %>% 
  select(-Site2, -`TOTAL-L`, -LITTER, -REINDRO, -BIRDRO, -ROCK, -SOIL, -CRUST) %>% 
  left_join(sp, by = c("Spp")) %>% 
  mutate(Genus = tolower(Genus), 
         Species = tolower(Species),
         Species = if_else(is.na(Species), "sp", Species),
         Taxon = paste(Genus, Species, sep = " ")) %>%
  mutate(Taxon = ifelse(Taxon == "NA oppositifolia", "saxifraga oppositifolia", Taxon)) %>% 
  mutate(Taxon = ifelse(Taxon == "festuca richardsonii", "festuca rubra", Taxon),
         Taxon = ifelse(Taxon == "pedicularis hisuta", "pedicularis hirsuta", Taxon),
         Taxon = ifelse(Taxon == "alopecurus boreale", "alopecurus ovatus", Taxon),
         Taxon = ifelse(Taxon == "stellaria crassipes", "stellaria longipes", Taxon),
         Taxon = ifelse(Taxon == "aulocomnium turgidum", "aulacomnium turgidum", Taxon),
         Taxon = ifelse(Taxon == "oncophorus whalenbergii", "oncophorus wahlenbergii", Taxon),
         Taxon = ifelse(Taxon == "racomitrium canescence", "niphotrichum canescens", Taxon),
         Taxon = ifelse(Taxon == "pedicularis dashyantha", "pedicularis dasyantha", Taxon),
         FunctionalGroup = if_else(Taxon == "ochrolechia frigida", "fungi", FunctionalGroup)) %>% 
  select(-Genus, -Species)

write_csv(CommunitySV_ITEX_2003_2015, path = "community/cleaned_data/ITEX_Svalbard_2003_2015_Community_cleaned.csv")


# Check community data over time
CommunitySV_ITEX_2003_2015 %>% 
  ggplot(aes(x = Year, y = Abundance, colour = Taxon)) +
  geom_point() +
  geom_line() +
  facet_wrap( ~ PlotID) +
  theme(legend.position = "none")



### CHECK SPECIES NAMES ###
# checks <- tpl.get(unique(CommunitySV_ITEX_2003_2015$Taxon))
# head(checks)
# checks %>% 
#   filter(note == "was misspelled|replaced synonym|family not in APG")

# Replace synonym:
# 1 Alopecurus magellanicus replaced synonym alopecurus ovatus
# 2     Persicaria vivipara replaced synonym bistorta vivipara
# 3          Luzula nivalis replaced synonym    luzula arctica


### ITEX HEIGHT DATA (not sure if used)
ItexHeight <- ItexHeight.raw %>% 
  select(SUBSITE, TREATMENT, PLOT, YEAR, HEIGHT) %>% 
  rename(Site = SUBSITE, Treatment = TREATMENT, PlotID = PLOT, Year = YEAR) %>% 
  filter(Site %in% c("DRY-L", "CAS-L", "BIS-L")) %>% 
  mutate(Site = gsub("-L", "", Site),
         PlotID = gsub("L", "", PlotID)) %>% 
  group_by(Year, Site, Treatment, PlotID) %>% 
  summarise(n = n(), Height_cm = mean(HEIGHT, na.rm = TRUE), se = sd(HEIGHT, na.rm = TRUE)/sqrt(n))
