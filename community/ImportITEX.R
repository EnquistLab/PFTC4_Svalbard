#### ITEX ####
# library
library("readxl")
library("tidyverse")
library("tpl")

#### Import data ####
ItexAbundance.raw <- read_excel(path = "community/data/ENDALEN_ALL-YEARS_TraitTrain.xlsx", sheet = "SP-ABUND")

ItexHeight.raw <- read_excel(path = "community/data/ENDALEN_ALL-YEARS_TraitTrain.xlsx", sheet = "HEIGHT")

sp <- read_excel(path = "data/Inga Svala Jonsdottir ITEX/Species lists_Iceland_Svalbard.xlsx", sheet = "Endalen")

coords <- read_excel(path = "Coordinates.xlsx", col_names = TRUE)

# Species names
sp <- sp %>% 
  select(SPP, GFNARROWarft, GENUS, SPECIES) %>% 
  slice(-1) %>% 
  mutate(GFNARROWarft = tolower(GFNARROWarft)) %>% 
  rename(Spp = SPP, FunctionalGroup = GFNARROWarft, Genus = GENUS, Species = SPECIES)

#Import soil moisture data from 2003 and 2004 by plot
soil_moisture2003 <- read_excel(path = "climate/data/Moisture_july_2003.xlsx", sheet = "Sheet1") %>% 
  group_by(Plot, Habitat, Treatment) %>% 
  summarize(soil_moist2003 = mean(Soil1, na.rm = T)) %>% 
  mutate(PlotID = paste(toupper(substr(Habitat,1,3)), "-", Plot, sep = ""))
save(soil_moisture2003, file = "climate/data/soil_moisture2003.rdata")

soil_moisture2004 <- read_excel(path = "climate/data/Soil_moisture_2004.xlsx", sheet = "Sheet1") %>% 
  group_by(Site, Plot) %>% 
  summarize(soil_moist2004 = mean(moisture, na.rm = T)) %>% 
  mutate(PlotID = paste(Site, "-", substr(Plot, 2, 3), sep = ""))
save(soil_moisture2004, file = "climate/data/soil_moisture2004.rdata")

#community data files
MetaCommunitySV_2003_2015 <- ItexAbundance.raw %>% 
  select(SUBSITE, TREATMENT, PLOT, YEAR, `TOTAL-L`, LITTER, REINDRO, BIRDRO, ROCK, SOIL) %>% 
  rename(Site = SUBSITE, Treatment = TREATMENT, PlotID = PLOT, Year = YEAR, TotalLitter = `TOTAL-L`, Litter = LITTER, ReinDrop = REINDRO, BirdDrop = BIRDRO, Rock = ROCK, Soil = SOIL) %>% 
  mutate(Site2 = substr(Site, 5, 5),
         Site = substr(Site, 1, 3),
         PlotID = gsub("L", "", PlotID)) %>% 
  # I think we do not need/want H sites
  filter(Site2 == "L") %>% 
  select(-Site2) %>%
  left_join(coords, by = c("Site", "Treatment"))


CommunitySV_ITEX_2003_2015 <- ItexAbundance.raw %>% 
  gather(key = Spp, value = Abundance, -SUBSITE, -TREATMENT, -PLOT, -YEAR, -`TOTAL-L`, -LITTER, -REINDRO, -BIRDRO, -ROCK, -SOIL) %>%
  filter(Abundance > 0) %>% 
  rename(Site = SUBSITE, Treatment = TREATMENT, PlotID = PLOT, Year = YEAR) %>% 
  mutate(Site2 = substr(Site, 5, 5),
         Site = substr(Site, 1, 3),
         PlotID = gsub("L", "", PlotID)) %>% 
  # I think we do not need/want H sites
  filter(Site2 == "L") %>% 
  select(-Site2, -`TOTAL-L`, -LITTER, -REINDRO, -BIRDRO, -ROCK, -SOIL) %>% 
  left_join(sp, by = c("Spp")) %>% 
  mutate(Genus = tolower(Genus), Species = tolower(Species),
         Taxon = paste(Genus, Species, sep = " ")) %>%
  mutate(Taxon = ifelse(Taxon == "NA oppositifolia", "saxifraga oppositifolia", Taxon)) %>% 
  mutate(Taxon = ifelse(Taxon == "festuca richardsonii", "festuca rubra", Taxon),
         Taxon = ifelse(Taxon == "pedicularis hisuta", "pedicularis hirsuta", Taxon),
         Taxon = ifelse(Taxon == "alopecurus boreale", "alopecurus ovatus", Taxon),
         Taxon = ifelse(Taxon == "stellaria crassipes", "stellaria longipes", Taxon),
         Taxon = ifelse(Taxon == "aulocomnium turgidum", "aulacomnium turgidum", Taxon),
         Taxon = ifelse(Taxon == "oncophorus whalenbergii", "oncophorus wahlenbergii", Taxon),
         Taxon = ifelse(Taxon == "racomitrium canescence", "niphotrichum canescens", Taxon),
         Taxon = ifelse(Taxon == "pedicularis dashyantha", "pedicularis dasyantha", Taxon)) %>% 
  select(-Genus, -Species)

save(CommunitySV_ITEX_2003_2015, file = "community/cleaned_data/CommunitySV_ITEX_2003_2015.Rdata")


### CHECK SPECIES NAMES ###
checks <- tpl.get(unique(CommunitySV_ITEX_2003_2015$Taxon))
head(checks)
checks %>% 
  filter(note == "was misspelled|replaced synonym|family not in APG")

# Replace synonym:
# 1 Alopecurus magellanicus replaced synonym alopecurus ovatus
# 2     Persicaria vivipara replaced synonym bistorta vivipara
# 3          Luzula nivalis replaced synonym    luzula arctica


# Get families
familySV <- checks %>% 
  select(family, name) %>% 
  mutate(name = tolower(name)) %>% 
  as.tibble() %>% 
  filter(!is.na(family))


### ITEX HEIGHT DATA
ItexHeight <- ItexHeight.raw %>% 
  select(SUBSITE, TREATMENT, PLOT, YEAR, HEIGHT) %>% 
  rename(Site = SUBSITE, Treatment = TREATMENT, PlotID = PLOT, Year = YEAR) %>% 
  filter(Site %in% c("DRY-L", "CAS-L", "BIS-L")) %>% 
  mutate(Site = gsub("-L", "", Site),
         PlotID = gsub("L", "", PlotID)) %>% 
  group_by(Year, Site, Treatment, PlotID) %>% 
  summarise(n = n(), Height_cm = mean(HEIGHT, na.rm = TRUE), se = sd(HEIGHT, na.rm = TRUE)/sqrt(n))
  


### HEIGHT WAS CALCULATED DIFFERENT IN 2009 AND 2015 !!!!
ggplot(ItexHeight, aes(x = as.factor(Year), y = Height_cm, fill = Treatment)) +
  geom_boxplot() +
  scale_fill_manual(values = c("grey", "red")) +
  facet_wrap(~ Site)


# Calculate species that sum up to 95% cover
Cover95 <- itex %>% 
  filter(!GFNARROWarft %in% c("LICHEN", "MOSS", "LIVERWORT")) %>% 
  mutate(SP = paste(GENUS, SPECIES, sep = "_")) %>% 
  group_by(PLOT) %>% 
  arrange(PLOT, desc(HITS)) %>% 
  mutate(cumprop = cumsum(HITS)/sum(HITS)) %>% 
  filter(cumprop <= 0.95) %>% 
  ungroup() %>% 
  count(SP)


field <- itex %>% 
  filter(!GFNARROWarft %in% c("LICHEN", "MOSS", "LIVERWORT")) %>% 
  select(TREATMENT, PLOT, Genus, Species, HITS) %>% 
  arrange(TREATMENT, PLOT, Genus)

itex.codes <- field %>% distinct(TREATMENT, PLOT) %>% 
  mutate(Habitat = substr(PLOT, 1, 3)) %>% 
  mutate(Plot = paste(PLOT, TREATMENT, sep = "-")) %>% 
  mutate(Plot = substr(Plot, 6, nchar(Plot))) %>% 
  select(-TREATMENT, -PLOT) %>% 
  arrange(Habitat)
  
#write_csv(field, path = "FieldSheetITEX.csv")


### COMMUNITX WEIGHTED MEANS
load(file = "community/cleaned_data/CommunitySV_ITEX_2003_2015.Rdata", verbose = TRUE)
traitMean <- readRDS(file = "traits/cleaned_data/community_weighted_means.RDS") %>% 
  as.tibble() %>% 
  mutate(mean = as.numeric(as.character(mean))) %>% 
  mutate(Year = as.numeric(as.character(Year)))

traitMean_noitv <- readRDS(file = "traits/cleaned_data/community_weighted_means_no_itv.RDS") %>% 
  as.tibble() %>% 
  mutate(mean = as.numeric(as.character(mean)))%>% 
  mutate(Year = as.numeric(as.character(Year)))

# for plotID etc.
metaItex <- CommunitySV_ITEX_2003_2015 %>% 
  distinct(Site, Treatment, PlotID)


### FLUX DATA
load(file = "fluxes/Standard_ITEXFluxes.Rdata", verbose = TRUE)


