###Generate CWM bootstrapping results

#load community data and make it skinny and remove equisetum
load(file = "community/cleaned_data/CommunitySV_ITEX_2003_2015.Rdata", verbose = TRUE)

comm <- CommunitySV_ITEX_2003_2015 %>% 
  select(-Spp, -FunctionalGroup) %>% 
  filter(Taxon != "equisetum arvense", Taxon != "equisetum scirpoides") #%>% 
  #filter(PlotID == "CAS-1", Year == "2015")

#load trait data and make it skinny
load("traits/data/traitsITEX_SV_2018.Rdata")

trait.pca <- traitsITEX_SV_2018 %>% 
  select(-Country, -Year, -Project, -Latitude_N, -Longitude_E, -Elevation_m, -Gradient, -Genus, -Species, -ID, -Date, -Individual_nr, -Wet_Mass_g, -Wet_Mass_Total_g, -Dry_Mass_Total_g, -Leaf_Area_Total_cm2, -Leaf_Thickness_1_mm, -Leaf_Thickness_2_mm, -Leaf_Thickness_3_mm, -Length_Ave_Moss_cm, - GreenLength_Ave_Moss_cm, -Length_1_cm, -Length_2_cm, -Length_3_cm, -GreenLength_1_cm, -GreenLength_2_cm, -GreenLength_3_cm, -NrLeaves, -Bulk_nr_leaves, -NumberLeavesScan, -Comment, -Data_entered_by)%>% na.omit() 

pca_res <- prcomp(trait.pca %>% select(-Taxon, -Site, -PlotID, -Treatment) , center = T, scale. = T)
  
  
trait <- cbind(trait.pca, pca_res$x[,1:3]) %>% 
  gather(key = Trait, value = Value, -PlotID, -Site, -Taxon, -Treatment) %>% 
  mutate(PlotID = paste0(Site, "-", substr(PlotID, 5,5))) %>% 
  select(-Treatment)


trait.null <- cbind(trait.pca, pca_res$x[,1:3]) %>% 
  gather(key = Trait, value = Value, -PlotID, -Site, -Taxon, -Treatment) %>% 
  filter(Treatment == "CTL") %>% 
  group_by(Taxon, Trait) %>% 
  summarize(Value = mean(Value)) %>% 
  right_join(trait.pca) %>% 
  select(Taxon:PlotID) %>%  
mutate(PlotID = paste0(Site, "-", substr(PlotID, 5,5)))

dat <- list(community = comm, trait_trans = trait)
dat.null <- list(community = comm, trait_trans = trait.null)

ITEX_CWM <- CWM_Bootstrapping(dat)
ITEX_CWM_noitv <- CWM_Bootstrapping(dat.null)

traitMean_noitv <- ITEX_CWM_noitv %>% 
  group_by(PlotID, Trait, Year) %>% 
  summarize(mean_noitv = mean(Mean))

traitMean <- ITEX_CWM %>% 
  group_by(PlotID, Trait, Year) %>% 
  summarize(mean = mean(Mean)) %>% 
  left_join(traitMean_noitv) %>% 
  left_join(metaItex)


