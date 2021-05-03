### CALCULATE COMMUNITY WEIGHTED MEANS ###

# NORMAL WAY WITHOUT BOOTSTRAPPING
CommunityW_GlobalAndLocalMeans <- function(dat){
  meanTraits <- dat$trait %>% 
    # Global means
    group_by(Taxon, Trait) %>% 
    mutate(TraitMean_global = mean(Value, na.rm = TRUE)) %>% 
    
    # Site means (site level)
    group_by(Site, Taxon, Trait) %>%
    mutate(TraitMean_site = mean(Value, na.rm = TRUE)) %>% 
    
    # Plot means
    group_by(PlotID, BlockID, Site, Taxon, Trait) %>%
    mutate(TraitMean_plot = mean(Value, na.rm = TRUE)) %>% 
    select(-Year, -Value) %>% 
    ungroup() %>% 
    distinct()
  
  dat2 <- dat$community 
  
  if(!dat2$Country[1] %in% c("NO", "CO")) {
    dat2 <- dat2 %>% 
      # join plot level means
      left_join(meanTraits %>% select(-TraitMean_global, -TraitMean_site))
  }
  
  dat2 <- dat2 %>% 
    # join site level means
    left_join(meanTraits %>% select(-TraitMean_global, -TraitMean_plot, -BlockID, -PlotID) %>% distinct()) %>% 
    
    # join global means
    left_join(meanTraits %>% select(-TraitMean_plot, -TraitMean_site, -Site, -BlockID, -PlotID) %>% distinct())
  
  if(dat2$Country[1] %in% c("NO", "CO")) {
    dat2 <- dat2 %>% 
      mutate(TraitMean = coalesce(TraitMean_site, TraitMean_global))
  } else{
    dat2 <- dat2 %>% 
      mutate(TraitMean = coalesce(TraitMean_plot, TraitMean_site, TraitMean_global))
  }
  
  ### Calculate Community weighted means
  dat2 <- dat2 %>% 
    group_by(Trait, Site, PlotID) %>% 
    mutate(CWTraitMean = weighted.mean(TraitMean, Abundance, na.rm=TRUE)) %>% 
    ungroup()
  
  return(dat2)
}

# Reduce to only CWMeans
CommunityW_Means <- function(TraitMeans_All){
  CWTraitMeans <- TraitMeans_All %>% 
    filter(!is.na(Trait)) %>% 
    select(-Taxon, -Abundance, -TraitMean_plot, -TraitMean_site, -TraitMean_global, -TraitMean) %>% 
    distinct()
  return(CWTraitMeans)
}


# TRANSFORMING THE TRAITS
LogTranformation <- function(dat){
  dat$trait_trans <- dat$trait %>% 
    
    # log transform
    mutate(Value = ifelse(Trait %in% c("Plant_Height_cm", "Wet_Mass_g", "Dry_Mass_g", "Leaf_Area_cm2"), log(Value), Value),
           Trait = recode(Trait, "Plant_Height_cm" = "Plant_Height_cm_log", "Wet_Mass_g" = "Wet_Mass_g_log", "Dry_Mass_g" = "Dry_Mass_g_log", "Leaf_Area_cm2" = "Leaf_Area_cm2_log"))
  return(dat)
}


# WITH BOOTSTRAPPING
CWM_Bootstrapping <- function(dat, nrep = 100, samplesize = 200){
  comm <- dat$community %>% 
    group_by(Year, Site, PlotID) %>% 
    mutate(sumAbundance = sum(Abundance))
  
  trait <- dat$trait_trans 
  
  TraitWeights_plot <- comm %>% 
    left_join(trait, by = c("Site", "PlotID", "Taxon")) %>% 
    group_by(Year, Site, PlotID, Taxon, Trait) %>% 
    mutate(weight = Abundance/n()) %>% 
    group_by(Year, Site, PlotID, Trait) 
  
  
  # Site level weights and traits
  TraitWeights_site <- comm %>%
    left_join(trait %>% select(-PlotID)) %>% 
    group_by(Year, Site, Taxon, Trait) %>% 
    mutate(weight = Abundance/n()) %>% 
    group_by(Year, Site, Trait) 
  
  
  # Global level weights and traits
  TraitWeights_global <- comm %>% 
    left_join(trait %>% select(-PlotID, -Site), by = c("Taxon")) %>% 
    group_by(Year, Taxon, Trait) %>% 
    mutate(weight = Abundance/n()) %>% 
    group_by(Year, Trait) 
  
  
  TraitWeights_all <- bind_rows(plot = TraitWeights_plot, site = TraitWeights_site, global = TraitWeights_global, .id = "level") %>% 
    mutate(level = factor(level, levels = c("plot", "site", "global"), ordered = TRUE)) %>%
    filter(!is.na(Value)) %>% 
    group_by(Year, Site, PlotID, Trait, Taxon) %>% 
    filter(level == min(level)) %>% 
    ungroup() %>% 
    group_by(Year, Site, PlotID, Trait) %>% 
    filter(!Abundance == 0)
    
  
  BootstrapMoments <- rerun(.n = nrep, sample_n(TraitWeights_all, size = samplesize,  replace = TRUE, weight = weight)) %>%
    bind_rows(.id = "n") %>% 
    group_by(n, add = TRUE) %>% 
    mutate(Value = as.numeric(Value)) %>% 
    # get all the happy moments
    summarize(Mean = mean(Value), Variance = var(Value), Skewness = skewness(Value), Kurtosis = kurtosis(Value))
  
  return(BootstrapMoments)
}


TraitWeights_all %>% 
  group_by(Country, Year, Site, Gradient, BlockID, PlotID, Trait, Taxon) %>% 
  slice(1) %>% 
  group_by(Country, Year, Site, Gradient, BlockID, PlotID, Trait) %>% summarise(Abundance = sum(Abundance)) %>% mutate(percent_Abundance = Abundance/sumAbundance * 100) %>% arrange(percent_Abundance) %>% pn


#### Filtering out turfs with less than 70% of the community present ###

#check_community_df <- wcommunity %>%
#group_by(Site, Species, turfID)%>%
#select(Site, turfID, Species, Abundance, SLA_mean, Lth_mean, Height_mean, LDMC_mean, LA_mean, CN_ratio_mean, sum_Abundance)%>%
#unique()%>%
#ungroup()%>%
#group_by(turfID)%>%
#mutate(Abundance_traits = (sum(Abundance)))%>%
#filter(!is.na(SLA_mean))%>%
#mutate(community_Abundance_trait=Abundance_traits/sum_Abundance*100)

#complete_turf <- check_community_df%>%
#filter(community_Abundance_trait>80)%>%
#distinct(turfID, .keep_all=TRUE)

#Complete_turfs<-as.vector(complete_turf$turfID)

#wcommunity_df <- filter(wcommunity, turfID %in% Complete_turfs)