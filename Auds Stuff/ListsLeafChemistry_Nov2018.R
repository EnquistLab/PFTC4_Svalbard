library("writexl")

# Gradient leaves
load(file = "traits/cleaned_data/traitsGradients_SV_2018.Rdata", verbose = TRUE)

TraitsGradien_Svalbard2018 <- traitsGradients_SV_2018 %>%
  filter(Project == "T") %>% 
  select(ID, Gradient, Site, PlotID, Taxon) %>% 
  arrange(Gradient, Taxon, Site, PlotID) %>%
  group_by(Gradient, Taxon, Site, PlotID) %>% 
  mutate(n = n()) %>% 
  slice(1:3) %>% 
  group_by(Gradient, Site, Taxon) %>% 
  arrange(Gradient, Taxon, Site, -n, PlotID) %>% 
  slice(1:9)

write_xlsx(TraitsGradien_Svalbard2018, path = "TraitsGradien_Svalbard2018.xlsx", col_names = TRUE)


# OTC leaves
load(file = "traits/cleaned_data/traitsITEX_SV_2018.Rdata", verbose = TRUE)

TraitsITEX_Svalbard2018 <- traits2018 %>% 
  filter(Project == "T", Site == "X") %>% 
  select(ID, Elevation, Genus, Species, Plot, Individual_nr)
write_xlsx(TraitsITEX_Svalbard2018, path = "TraitsITEX_Svalbard2018.xlsx", col_names = TRUE)
