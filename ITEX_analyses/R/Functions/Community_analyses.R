#### COMMUNITY DATA ANALYSIS ####



#### CALCULATE COMMUNITY METRICS #### 
calc_comm_metrics <- function(comm){
  
  comm_resp <- comm %>% 
    group_by(Year, Site, Treatment, PlotID) %>%  
    summarise(Richness = n(), 
              Diversity = diversity(Abundance), 
              N1 = exp(Diversity),
              Evenness = Diversity/log(Richness),
              sumAbundance = sum(Abundance),
              propGraminoid = sum(Abundance[FunctionalGroup %in% c("graminoid")])/sumAbundance,
              propForb = sum(Abundance[FunctionalGroup %in% c("forb")])/sumAbundance,
              propShrub = sum(Abundance[FunctionalGroup %in% c("eshrub", "dshrub")])/sumAbundance,
              propEShrub = sum(Abundance[FunctionalGroup %in% c("eshrub")])/sumAbundance,
              propDShrub = sum(Abundance[FunctionalGroup %in% c("dshrub")])/sumAbundance,
              propLichen = sum(Abundance[FunctionalGroup %in% c("lichen")])/sumAbundance,
              propBryo = sum(Abundance[FunctionalGroup %in% c("moss", "liverwort")])/sumAbundance,
              totalVascular = sum(Abundance[FunctionalGroup %in% c("graminoid", "forb", "eshrub", "dshrub")]),
              totalGraminoid = sum(Abundance[FunctionalGroup %in% c("graminoid")]),
              totalForb = sum(Abundance[FunctionalGroup %in% c("forb")]),
              totalShrub = sum(Abundance[FunctionalGroup %in% c("eshrub", "dshrub")]),
              totaleShrub = sum(Abundance[FunctionalGroup %in% c("eshrub")]),
              totaldShrub = sum(Abundance[FunctionalGroup %in% c("dshrub")])
    )
  return(comm_resp)
}


#### plot 1: change in community metrics ####
community_metric_change <- function(comm, meta, comm_resp){
  
  #multivariate community distances
  comm_distances <- comm %>%
    select(-FunctionalGroup, -Elevation_m, -Latitude_N, -Longitude_E, -Flag) %>%
    filter(Year != 2009) %>%
    spread(key = Taxon, value = Abundance, fill = 0) %>%
    group_by(PlotID) %>%
    do( data_frame(out = as.vector(vegdist(select(., -(Year:PlotID)), method = "bray")))) %>%
    left_join(meta, by = "PlotID")  %>%
    mutate(Site = factor(Site, levels = c("SB", "CH", "DH")))
  
  #calculate change in community metrics
  metric_plot_dist <- comm_resp %>%
    filter(Year != 2009) %>%
    gather(key = response, value = value, -Year, -Site, -Treatment, -PlotID) %>%
    group_by(Treatment, PlotID, Site, response) %>%
    summarize(dist = diff(value))%>%
    #left_join(soil_moisture2003, by = "PlotID") %>%
    #left_join(soil_moisture2004, by = "PlotID") %>%
    filter(response == "Richness" | response == "Diversity" | response == "Evenness" | response == "sumAbundance" | response == "totalGraminoid" | response == "totalForb" | response == "totaldShrub" | response == "totaleShrub" | response == "propLichen" | response == "propBryo")
  
  comm_distances_merge <- comm_distances %>%
    rename("dist" = "out") %>%
    mutate(response = "Bray Curtis Distance")
  
  metric_plot_dist <- bind_rows(metric_plot_dist, comm_distances_merge)
 
  return(metric_plot_dist) 
}


community_t_test <- function(metric_plot_dist){
  t_test <- metric_plot_dist %>% filter(response != "Diversity") %>%
    mutate(response = plyr::mapvalues(response, from = c("propBryo", "propLichen", "sumAbundance", "totalForb", "totalGraminoid", "totaleShrub", "totaldShrub"), to = c("Bryophyte Abundance", "Lichen Abundance", "Vascular Abundance", "Forb Abundance", "Graminoid Abundance", "Evergreen Shrub Abundance", "Deciduous Shrub Abundance"))) %>%
    mutate(response = factor(response, levels = c("Bray Curtis Distance", "Evenness", "Richness","Vascular Abundance", "Forb Abundance", "Graminoid Abundance", "Evergreen Shrub Abundance", "Deciduous Shrub Abundance", "Bryophyte Abundance", "Lichen Abundance"))) %>%
    #dplyr::filter(response != "Forb Abundance") %>%
    #filter(response != "Bryophyte Abundance") %>%
    #filter(response != "Lichen Abundance") %>%
    droplevels() %>%
    group_by(response, Site, Treatment) %>%
    summarise(P = t.test(dist, mu = 0)$p.value,
              Sig = ifelse(P < 0.05, "*", ifelse(P<0.1 & P > 0.05, "+", "")),
              MaxWidth = max(dist))%>% ungroup() %>%
    mutate(response = factor(response, levels = c("Bray Curtis Distance", "Evenness", "Richness","Vascular Abundance", "Forb Abundance", "Graminoid Abundance", "Evergreen Shrub Abundance", "Deciduous Shrub Abundance", "Bryophyte Abundance", "Lichen Abundance")))
  
  return(t_test)
  
}



#### Figure S4 change in other community metrics ####
t_test_supp <- function(metric_plot_dist){
  
  t_test_supp <- metric_plot_dist %>% 
    filter(response != "Diversity") %>%
    mutate(response = plyr::mapvalues(response, from = c("propBryo", "propLichen", "sumAbundance", "totalForb", "totalGraminoid", "totaleShrub", "totaldShrub"), to = c("Bryophyte Abundance", "Lichen Abundance", "Vascular Abundance", "Forb Abundance", "Graminoid Abundance", "Evergreen Shrub Abundance", "Deciduous Shrub Abundance"))) %>%
    mutate(response = factor(response, levels = c("Bray Curtis Distance", "Evenness", "Richness","Vascular Abundance", "Forb Abundance", "Graminoid Abundance", "Evergreen Shrub Abundance", "Deciduous Shrub Abundance", "Bryophyte Abundance", "Lichen Abundance"))) %>%
    filter(response != "Bray Curtis Distance", response != "Evenness", response != "Richness", response != "Vascular Abundance", response != "Graminoid Abundance", response != "Evergreen Shrub Abundance", response != "Deciduous Shrub Abundance") %>%
    droplevels(.) %>%
    group_by(response, Site, Treatment) %>%
    summarise(P = t.test(dist, mu = 0)$p.value,
              Sig = ifelse(P < 0.05, "*", ifelse(P<0.1 & P > 0.05, "+", "")),
              MaxWidth = max(dist))%>% ungroup() %>%
    mutate(response = factor(response, levels = c("Bray Curtis Distance", "Evenness", "Richness","Vascular Abundance", "Forb Abundance", "Graminoid Abundance", "Evergreen Shrub Abundance", "Deciduous Shrub Abundance", "Bryophyte Abundance", "Lichen Abundance")))
  
  return(t_test_supp)
}





#### NMDS ORDINATION ####
nmds_ordination <- function(comm){
  set.seed(32)
  
  # SNOWBED (SB)
  comm_fat_SB <- comm %>% 
    select(-c(FunctionalGroup:Flag)) %>% 
    arrange(Year) %>% 
    spread(key = Taxon, value = Abundance, fill = 0) %>% 
    filter(Site == "SB")
  
  comm_fat_spp_SB <- comm_fat_SB %>% select(-(Year:PlotID))
  
  NMDS_SB <- metaMDS(comm_fat_spp_SB, noshare = TRUE, try = 30)
  
  fNMDS_SB <- fortify(NMDS_SB) %>% 
    filter(Score == "sites") %>% 
    bind_cols(comm_fat_SB %>% select(Year:PlotID))
  
  
  # CASSIOPE HEATH (CH)
  comm_fat_CH <- comm %>% 
    select(-c(FunctionalGroup:Flag)) %>% 
    arrange(Year) %>% 
    spread(key = Taxon, value = Abundance, fill = 0) %>% 
    filter(Site == "CH")
  
  comm_fat_spp_CH <- comm_fat_CH %>% select(-(Year:PlotID))
  
  NMDS_CH <- metaMDS(comm_fat_spp_CH, noshare = TRUE, try = 100)
  
  fNMDS_CH <- fortify(NMDS_CH) %>% 
    filter(Score == "sites") %>% 
    bind_cols(comm_fat_CH %>% select(Year:PlotID))
  
  
  # DRYAS HEATH
  comm_fat_DH <- comm %>% 
    select(-c(FunctionalGroup:Flag)) %>% 
    arrange(Year) %>% 
    spread(key = Taxon, value = Abundance, fill = 0) %>% 
    filter(Site == "DH")
  
  comm_fat_spp_DH <- comm_fat_DH %>% select(-(Year:PlotID))
  
  NMDS_DH <- metaMDS(comm_fat_spp_DH, noshare = TRUE, try = 100)
  
  fNMDS <- fortify(NMDS_DH) %>% 
    filter(Score == "sites") %>% 
    bind_cols(comm_fat_DH %>% select(Year:PlotID)) %>% 
    bind_rows(fNMDS_SB, fNMDS_CH)
  
  return(fNMDS)
  
}


# Check if community composition changes in treatments and over time
NMDS_analysis <- function(NMDS_output){
  
  NMDS_result <- NMDS_output %>%
    nest(data = -Site) %>% 
    mutate(
      fit = map(data, ~ lm(NMDS1 ~ Treatment * Year, data = .x)),
      tidied = map(fit, tidy)
    ) %>% 
    unnest(tidied) %>% 
    filter(p.value < 0.05,
           term != "(Intercept)")
  
  return(NMDS_result)
}


height_analysis <- function(height){
  
  height_model <- height %>% 
    nest(data = -Site) %>% 
    mutate(
      fit1 = map(data, ~ lmer(Value ~ Treatment + (1|PlotID), data = .x)),
      fit2 = map(data, ~ lmer(Value ~ 1 + (1|PlotID), data = .x)),
      tidy1 = map(fit1, glance),
      tidy2 = map(fit2, glance)
  )
  
  height_model_result <- bind_rows(Treatment_model = height_model %>% 
              unnest(tidy1),
            Null_model = height_model %>% 
              unnest(tidy2),
            .id = "Model") %>% 
    select(Model, Site, AIC) %>% 
    pivot_wider(names_from = "Model", values_from = "AIC") %>% 
    mutate(Model_Diff = Treatment_model - Null_model)
  
  return(height_model_result)
  
}

