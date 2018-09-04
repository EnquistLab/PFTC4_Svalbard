#### ITEX ####
# library
library("readxl")
library("tidyverse")

# import data
itex <- read_excel(path = "data/Inga Svala Jonsdottir ITEX/ENDALEN_SPP_2015.xlsx")
sp <- read_excel(path = "data/Inga Svala Jonsdottir ITEX/Species lists_Iceland_Svalbard.xlsx", sheet = "Endalen")

sp <- sp %>% 
  select(SPP, GFNARROWarft, GENUS, SPECIES) %>% 
  slice(-1) %>% 
  mutate(GFNARROWarft = tolower(GFNARROWarft))

CommunityITEX_SV_2015 <- itex %>% 
  gather(key = SPP, value = HITS, -SUBSITE, -TREATMENT, -PLOT, -YEAR, -TOTAL.L, -LITTER, -REINDRO, -BIRDRO, -ROCK, -SOIL, -CRUST) %>% 
  filter(HITS > 0) %>% 
  left_join(sp, by = c("SPP")) %>% 
  mutate(GENUS = tolower(GENUS), SPECIES = tolower(SPECIES)) %>% 
  mutate(Taxon = paste(GENUS, SPECIES, sep = " ")) %>% 
  rename(Genus = GENUS, Species = SPECIES) %>% 
  mutate(Elevation = substr(PLOT, 1, 3),
         Plot = paste(substr(PLOT, 6, nchar(PLOT)), TREATMENT, sep = "-")) %>% 
  filter(!GFNARROWarft %in% c("lichen", "moss", "liverworth")) %>% 
  mutate(Taxon = ifelse(Taxon == "NA oppositifolia", "saxifraga oppositifolia", Taxon)) %>% 
  mutate(Taxon = ifelse(Taxon == "festuca richardsonii", "festuca rubra", Taxon)) %>% 
  mutate(Taxon = ifelse(Taxon == "pedicularis hisuta", "pedicularis hirsuta", Taxon)) %>% 
  mutate(Taxon = ifelse(Taxon == "alopecurus boreale", "alopecurus ovatus", Taxon)) %>% 
  mutate(Taxon = ifelse(Taxon == "stellaria crassipes", "stellaria longipes", Taxon)) %>% 
  select(-SUBSITE) %>% 
  rename(Treatment = TREATMENT, Year = YEAR, Crust = CRUST, TotalL = TOTAL.L, Litter = LITTER, Reindro = REINDRO, Birdro = BIRDRO, Rock = ROCK, Soil = SOIL, Hits = HITS, FunctionalGroup = GFNARROWarft)

save(CommunityITEX_SV_2015, file = "community/CommunityITEX_SV_2015.Rdata")

# total hits per plot
itex %>% 
  filter(!GFNARROWarft %in% c("LICHEN", "MOSS", "LIVERWORT")) %>% 
  group_by(PLOT) %>% 
  summarise(sum = sum(HITS)) %>% 
  summarise(min = min(sum), max = max(sum))

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
