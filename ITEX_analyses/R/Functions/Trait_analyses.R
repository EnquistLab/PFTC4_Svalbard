#### TRAIT DATA ANALYSIS ####

#### TRAIT BOOTSTRAPPING ####

make_bootstrapping <- function(comm_raw, trait_raw){
  
  #prepare community data
  comm <- comm_raw %>%
    filter(FunctionalGroup != "lichen", FunctionalGroup != "moss", FunctionalGroup != "liverwort", FunctionalGroup != "fungi") %>% 
    select(-FunctionalGroup) %>% 
    filter(Taxon != "equisetum arvense", Taxon != "equisetum scirpoides") %>% 
    filter(Year == 2015) %>% 
    mutate(Site_trt = paste0(Site, Treatment))
  
  #prepare trait data
  trait <- trait_raw %>% 
    select(Treatment, Site, PlotID, Taxon, Trait, Value) %>% 
    filter(!is.na(Value)) %>% 
    mutate(Site_trt = paste0(Site, Treatment)) %>%  
    mutate(Site = factor(Site, levels = c("SB", "CH", "DH"))) %>% 
    filter(Trait != "Wet_Mass_g")
  
  #prepare trait data without intraspecific variation
  trait.null <- trait_raw %>% 
    select(Treatment, Site, PlotID, Taxon, Trait, Value) %>%   
    filter(!is.na(Value)) %>% 
    filter(Treatment == "CTL") %>% 
    filter(Trait != "Wet_Mass_g") %>% 
    group_by(Taxon, Trait) %>% 
    summarize(Value = mean(as.numeric(Value), na.rm = T)) %>% 
    right_join(trait, by = c("Taxon", "Trait")) %>% 
    select(-Value.y, "Value" = Value.x) %>%  
    mutate(Site = factor(Site, levels = c("SB", "CH", "DH")))
  
  #set seed for bootstrapping repeatability
  set.seed(2525)
  trait_imp <- trait_impute(comm = comm, 
                            traits = trait, 
                            scale_hierarchy = c("Site", "Site_trt", "PlotID"),
                            global = F,
                            taxon_col = "Taxon",
                            trait_col = "Trait",
                            value_col = "Value",
                            abundance_col = "Abundance",
                            min_n_in_sample = 2
  )
  
  trait_imp_null <- trait_impute(comm = comm, 
                                 traits = trait.null, 
                                 scale_hierarchy = c("Site", "Site_trt", "PlotID"),
                                 global = F,
                                 taxon_col = "Taxon",
                                 trait_col = "Trait",
                                 value_col = "Value",
                                 abundance_col = "Abundance",
                                 min_n_in_sample = 2
  )
  
  #check trait coverage
  trait_imp %>% 
    #filter(Trait == "C_percent") %>% 
    autoplot(.) +
    theme(axis.text.x = element_text(angle = 90))
  
  trait_imp_null %>% 
    autoplot(.) +
    theme(axis.text.x = element_text(angle = 90))
  
  #do the bootstrapping
  CWM <- trait_np_bootstrap(trait_imp, nrep = 100, sample_size = 200)
  CWM_notiv <- trait_np_bootstrap(trait_imp_null, nrep = 100, sample_size = 200)
  
  CWM_mean <- trait_summarise_boot_moments(CWM) %>% 
    select(Site:mean) 
  
  CWM_notiv_mean <- trait_summarise_boot_moments(CWM_notiv) %>% 
    select(Site:mean) %>% 
    rename("mean_noitv" = "mean")
  
  traitMean <- CWM_mean %>% 
    left_join(CWM_notiv_mean) %>% 
    select(-n) %>% 
    mutate(Year = 2015)
  
  #prepare bootstrapped trait data for analyses
  traitMean <- traitMean %>% ungroup() %>% 
    mutate(Trait = plyr::mapvalues(Trait, from = c("P_percent", "dC13_permil", "dN15_permil"), to = c("P_Ave", "dC13_percent", "dN15_percent"))) %>% 
    mutate(Treatment = substr(Site_trt, 3, 6)) %>% 
    mutate(Site = factor(Site, levels = c("SB", "CH", "DH")))
  
  return(traitMean)
  
}


 
#### Intraspecific vs. interspecific variation ####


Intra_vs_Inter <- function(trait_raw, traitMean){
  
  #prepare trait data
  trait <- trait_raw %>% 
    select(Treatment, Site, PlotID, Taxon, Trait, Value) %>% 
    filter(!is.na(Value)) %>% 
    mutate(Site_trt = paste0(Site, Treatment)) %>%  
    mutate(Site = factor(Site, levels = c("SB", "CH", "DH"))) %>% 
    filter(Trait != "Wet_Mass_g")
  
  var_res <- data.frame()
  
  for(i in unique(trait$Trait)){
    v <- varcomp(lme(Value~1, random=~1|Taxon, data=trait %>% filter(Trait == i), na.action = na.omit), 1)[c(1,2)] 
    
    v$trait <- i
    
    v <- unlist(v)
    
    var_res <- bind_rows(var_res, v)
    
  }
  
  var_split <- traitMean %>%
    group_by(Trait) %>% 
    do(test = trait.flex.anova(~Site * Treatment, mean, mean_noitv, data = .)) 
  
  var_split_exp <- data.frame(RelSumSq.Turnover = 1000, RelSumSq.Intraspec. = 1000, RelSumSq.Covariation = 1000, RelSumSq.Total = 1000, trait = "E", level = "F")
  
  for(i in 1:nrow(var_split)){
    out <- as.data.frame(var_split$test[[i]][2])
    out$trait <- as.factor(rep(var_split[i,1], 5))
    out$level <- rownames(out)
    var_split_exp <- rbind(var_split_exp, out)
  }
  
  return(var_split_exp)
  
}


Intra_vs_Inter_var_split <- function(var_split_exp){
  
  var_split <- var_split_exp %>% 
    mutate(level = trimws(level)) %>% 
    filter(RelSumSq.Turnover < 999) %>% 
    rename(Turnover = RelSumSq.Turnover, Intraspecific = RelSumSq.Intraspec., Covariation = RelSumSq.Covariation, Total = RelSumSq.Total) %>% 
    mutate(level = plyr::mapvalues(level, from = c("Site", "Site:Treatment"), to = c("Habitat", "Habitat:Treatment"))) %>% 
    gather(key = variable, value = value, -trait, -level) %>% 
    filter(variable == "Total") %>% 
    filter(level != "Total") %>% 
    mutate(level = factor(level, levels = c("Habitat", "Treatment", "Habitat:Treatment", "Residuals"))) %>% 
    mutate(level = plyr::mapvalues(level, from = c("Habitat", "Treatment", "Habitat:Treatment", "Residuals"), to = c("H", "T", "HxT", "Resid"))) %>% 
    mutate(trait = plyr::mapvalues(trait, from = c("SLA_cm2_g", "LDMC", "Leaf_Area_cm2", "Leaf_Thickness_mm", "N_percent", "C_percent", "P_Ave", "CN_ratio", "dC13_percent", "dN15_percent", "Dry_Mass_g", "Plant_Height_cm"), to = c("`SLA`*` `*(cm^2/g)", "`LDMC`*` `*(g/g)", "'Leaf'*' '*'Area'*' '*(cm^2)", "'Leaf'*' '*'Thickness'*' '*(mm)", "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')", "'Dry'*' '*'Mass'*' '*'(g)'", "'Plant'*' '*'Height'*' '*'(cm)'"))) %>% 
    mutate(trait = factor(trait, levels = c("'Plant'*' '*'Height'*' '*'(cm)'", "`SLA`*` `*(cm^2/g)", "'Dry'*' '*'Mass'*' '*'(g)'","'Leaf'*' '*'Area'*' '*(cm^2)",  "'Leaf'*' '*'Thickness'*' '*(mm)", "`LDMC`*` `*(g/g)",  "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')" )))
  
  return(var_split)
}



