#### ITEX ####
# library
library("readxl")
library("tidyverse")
library("tpl")

#### Import data ####
ItexAbundance.raw <- read_excel(path = "community/data/ENDALEN_ALL-YEARS_TraitTrain.xlsx", sheet = "SP-ABUND")

sp <- read_excel(path = "data/Inga Svala Jonsdottir ITEX/Species lists_Iceland_Svalbard.xlsx", sheet = "Endalen")

coords <- read_excel(path = "Coordinates.xlsx", col_names = TRUE)

# Species names
sp <- sp %>% 
  select(SPP, GFNARROWarft, GENUS, SPECIES) %>% 
  slice(-1) %>% 
  mutate(GFNARROWarft = tolower(GFNARROWarft)) %>% 
  rename(Spp = SPP, FunctionalGroup = GFNARROWarft, Genus = GENUS, Species = SPECIES)


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

save(CommunityITEX_SV_2015, file = "community/CommunityITEX_SV_2015.Rdata")


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
