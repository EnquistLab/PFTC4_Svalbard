#### ITEX ####
# library
library("readxl")
library("tidyverse")

# import data
itex <- read_excel(path = "data/Inga Svala Jonsdottir ITEX/ENDALEN_SPP_2015.xlsx")
sp <- read_excel(path = "data/Inga Svala Jonsdottir ITEX/Species lists_Iceland_Svalbard.xlsx", sheet = "Endalen")

sp <- sp %>% 
  select(SPP, GFNARROWarft, GENUS, SPECIES)

itex <- itex %>% 
  gather(key = SPECIES, value = HITS, -SUBSITE, -TREATMENT, -PLOT, -YEAR, -TOTAL.L, -LITTER, -REINDRO, -BIRDRO, -ROCK, -SOIL, -CRUST) %>% 
  filter(HITS > 0) %>% 
  rename(SPP = SPECIES) %>% 
  left_join(sp, by = "SPP")

# total hits per plot
itex %>% 
  filter(!GFNARROWarft %in% c("LICHEN", "MOSS", "LIVERWORT")) %>% 
  group_by(PLOT) %>% 
  summarise(sum = sum(HITS)) %>% 
  summarise(min = min(sum), max = max(sum))

# Calculate species that sum up to 95% cover
itex %>% 
  filter(!GFNARROWarft %in% c("LICHEN", "MOSS", "LIVERWORT")) %>% 
  mutate(SP = paste(GENUS, SPECIES, sep = "_")) %>% 
  group_by(PLOT) %>% 
  arrange(PLOT, desc(HITS)) %>% 
  mutate(cumprop = cumsum(HITS)/sum(HITS)) %>% 
  filter(cumprop <= 0.95) %>% 
  ungroup() %>% 
  count(SP) %>% print(n = Inf)

