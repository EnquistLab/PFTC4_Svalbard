#### CHECK TRAIT DATA ####

source(file = "traits/Ranalysis/DataImport2018.R")


# Draw plots

traitsSV2018 %>% 
  filter(Project == "T") %>% 
  #filter(Taxon == "equisetum arvense") %>% 
  ggplot(aes(x = Wet_Mass_g, y = Leaf_Area_cm2, color = Genus)) +
  geom_point() + 
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~ Taxon)


traitsSV2018 %>% 
  filter(Project == "T") %>% 
  ggplot(aes(x = Wet_Mass_g, y = Leaf_Thickness_Ave_mm, color = Genus)) +
  geom_point() + 
  scale_x_log10() +
  scale_y_log10()


# Should I mark these?
traitsSV2018 %>% 
  select(ID, Site, Elevation, Taxon, Plot, Individual_nr, Leaf_Area_cm2, Wet_Mass_g, Leaf_Thickness_Ave_mm, Bulk_nr_leaves, Remark) %>% 
  filter(Wet_Mass_g > 0.25, Leaf_Area_cm2 < 5) %>% 
  arrange(Taxon, ID) %>% 
  #filter(Taxon == "luzula nivalis") %>% 
  as.data.frame()
