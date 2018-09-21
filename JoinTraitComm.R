traits %>% 
  filter(!Project %in% c("P", "M", "Sean"), Site != "X") %>% 
  anti_join(community.raw1, by = c("Site", "Elevation", "Plot", "Taxon")) %>% 
  filter(!is.na(Elevation)) %>% 
  filter(!is.na(Plot)) %>% 
  select(Site, Elevation, Plot, Taxon) %>% pn

### Join trait and community data
# used left_join because we are interested in the traits for now, but species with low cover and no trait data get lost!
TraitsCommGradients <- traits2018 %>% 
  filter(!Project %in% c("P", "M", "Sean"), Site != "X") %>% 
  left_join(community.raw1, by = c("Site", "Elevation", "Plot", "Taxon"))
save(TraitsCommGradients, file = "traits/data/TraitsCommGradients.Rdata")

TraitsAllProjects <- traits2018
save(TraitsAllProjects, file = "traits/data/TraitsAllProjects.Rdata")


### ITEX
TraitsCommITEX <- traits2018 %>% 
  filter(Site == "X") %>% 
  left_join(itex1, by = c("Elevation", "Plot", "Taxon"))
save(TraitsCommITEX, file = "traits/data/TraitsCommITEX.Rdata")

