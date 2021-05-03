# Calculate Diversity Indices
ITEX_Diversity <- function(community_itex){
  diversity_itex = community_itex %>% 
    mutate(Treatment = recode(Treatment, "CTL" = "Control")) %>% 
    group_by(Site, Treatment, PlotID) %>%  
    summarise(n = n(),
              Richness = n, 
              Diversity = diversity(Abundance), 
              Evenness = Diversity/log(Richness),
              sumAbundance = sum(Abundance),
              propGraminoid = sum(Abundance[FunctionalGroup == "graminoid"])/sumAbundance,
              propForb = sum(Abundance[FunctionalGroup == "forbsv"])/sumAbundance,
              propShrub = sum(Abundance[FunctionalGroup %in% c("eshrub", "dshrub")])/sumAbundance,
              propEShrub = sum(Abundance[FunctionalGroup == "eshrub"])/sumAbundance,
              propDShrub = sum(Abundance[FunctionalGroup == "dshrub"])/sumAbundance,
              propLichen = sum(Abundance[FunctionalGroup == "lichen"])/sumAbundance,
              propBryophytes = sum(Abundance[FunctionalGroup %in% c("moss", "liverwort")])/sumAbundance,
              totalVascular = sum(Abundance[FunctionalGroup %in% c("graminoid", "forbsv", "eshrub", "dshrub")]),
              totalGraminoid = sum(Abundance[FunctionalGroup == "graminoid"]),
              totalForb = sum(Abundance[FunctionalGroup == "forbsv"]),
              totalShrub = sum(Abundance[FunctionalGroup %in% c("eshrub", "dshrub")])) %>% 
    pivot_longer(cols = n:totalShrub, names_to = "DiversityIndex", values_to = "Value")
  
  return(diversity_itex)  
}



ITEX_Diversity_Figure <- function(diversity_itex){
  ITEX_diversity_plot <- diversity_itex %>% 
    filter(DiversityIndex %in% c("Richness", "Diversity", "Evenness", "propGraminoid", "propForb", "propShrub", "propLichen", "propBryophytes")) %>%
    mutate(DiversityIndex = factor(DiversityIndex, levels = c("Richness", "Diversity", "Evenness", "propGraminoid", "propForb", "propShrub", "propLichen", "propBryophytes"))) %>% 
    ggplot(aes(x = factor(Year), y = Value, fill = Treatment)) +
    geom_boxplot() +
    scale_fill_manual(values = c("grey", "red")) +
    labs(x = "", y = "Diversity index") +
    facet_grid(DiversityIndex ~ Site, scales = "free_y") +
    theme_bw()
  
  return(ITEX_diversity_plot)
}


# Ordination
ITEX_Ordination <- function(community_itex){

  set.seed(32)
  
  # BISTORTA
  comm_fat_BIS = community_itex %>% 
    select(-Taxon, -FunctionalGroup) %>% 
    arrange(Year) %>% 
    spread(key = Spp, value = Abundance, fill = 0) %>% 
    filter(Site == "BIS")
  
  comm_fat_spp_BIS = comm_fat_BIS %>% select(-(Site:Year))
  
  NMDS_BIS <- metaMDS(comm_fat_spp_BIS, noshare = TRUE, try = 30)
  
  fNMDS_BIS <- fortify(NMDS_BIS) %>% 
    filter(Score == "sites") %>% 
    bind_cols(comm_fat_BIS %>% select(Site:Year))
  
  # CASSIOPE
  comm_fat_CAS <- community_itex %>% 
    select(-Taxon, -FunctionalGroup) %>% 
    arrange(Year) %>% 
    spread(key = Spp, value = Abundance, fill = 0) %>% 
    filter(Site == "CAS")
  
  comm_fat_spp_CAS <- comm_fat_CAS %>% select(-(Site:Year))
  
  NMDS_CAS <- metaMDS(comm_fat_spp_CAS, noshare = TRUE, try = 100)
  
  fNMDS_CAS <- fortify(NMDS_CAS) %>% 
    filter(Score == "sites") %>% 
    bind_cols(comm_fat_CAS %>% select(Site:Year))
  
  
  # DRYAS
  comm_fat_DRY <- community_itex %>% 
    select(-Taxon, -FunctionalGroup) %>% 
    arrange(Year) %>% 
    spread(key = Spp, value = Abundance, fill = 0) %>% 
    filter(Site == "DRY")
  
  comm_fat_spp_DRY <- comm_fat_DRY %>% select(-(Site:Year))
  
  NMDS_DRY <- metaMDS(comm_fat_spp_DRY, noshare = TRUE, try = 100)
  
  fNMDS <- fortify(NMDS_DRY) %>% 
    filter(Score == "sites") %>% 
    bind_cols(comm_fat_DRY %>% select(Site:Year)) %>% 
    bind_rows(fNMDS_BIS, fNMDS_CAS)
  
  return(fNMDS)
}

MakeITEXOrdination <- function(fNMDS_Itex){
  ITEX_ordinationOrdination <- ggplot(fNMDS_Itex, aes(x = NMDS1, y = NMDS2, group = PlotID, shape = Treatment, linetype = Treatment)) +
    geom_point(aes(size = ifelse(Year == min(as.numeric(Year)), "First", "Other"))) +
    geom_path() + 
    coord_equal() +
    scale_size_discrete(name = "Year", range = c(1.2, 2.5), limits = c("Other", "First"), breaks = c("First", "Other")) +
    scale_shape_manual(values = c(1, 16)) + 
    scale_linetype_manual(values = c("dashed", "solid")) + 
    labs(x = "NMDS axis 1", y = "NMDS axis 2") +
    facet_grid(~ Site) +
    theme_bw()
  
  return(ITEX_ordinationOrdination)
  
}



# Traits

traits_itex %>% 
  ggplot(aes(x = Value, fill = Site)) +
  geom_density(alpha = 0.5) +
  labs(x = "Mean trait value", y = "Density") +
  facet_wrap(~Traits, scales = "free")


