# Analysis and Figure plan

AnalysisPlan <- drake_plan(
  
  # diversity indices
  CommResp = calc_comm_metrics(Community),
  
  # change in community metrics over time
  Comm_Metric_Change = community_metric_change(Community, metaItex, CommResp),
  Comm_t_Test = community_t_test(Comm_Metric_Change),
  Comm_t_Test_Supp = community_t_test(Comm_Metric_Change),
  
  # Community ANOVA
  Comm_Anova = Comm_Metric_Change %>%
      group_by(response) %>%
      nest(data = -response) %>%
      mutate(
        aov = map(data, ~ aov(dist ~ Treatment*Site, data = .x)),
        aov_tidy = map(aov, tidy)
        ),
    
    Comm_Anova_tidy = Comm_Anova %>%
      select(response, aov_tidy) %>%
      unnest(aov_tidy),
  
  
  # NMDS ordination
  NMDS_output = nmds_ordination(Community),
  NMDS_result = NMDS_analysis(NMDS_output),
  
  
  # Bootstrapping
  Trait_Mean = make_bootstrapping(Community, Traits),
  
  # Trait PCA
  trait_pca = Trait_Mean %>% 
      select(-mean_noitv) %>% 
      mutate(Trait = plyr::mapvalues(Trait, from = c("SLA_cm2_g", "LDMC", "Leaf_Area_cm2", "Leaf_Thickness_mm", "N_percent", "C_percent", "P_Ave", "CN_ratio", "dC13_percent", "dN15_percent", "Dry_Mass_g", "Plant_Height_cm"), to = c("SLA", "LDMC", "Leaf Area", "Leaf Thickness", "%N", "%C", "%P", "C:N", "delta13C", "delta15N", "Dry Mass", "Plant Height"))) %>% 
      pivot_wider(names_from = Trait, values_from = mean),
    
    trait_pca_data = trait_pca %>% 
      select("%C":"SLA"),
    
    trait_pca_info = trait_pca %>% 
      select(Site:Treatment),
    
    pca_res = prcomp(trait_pca_data, center = T, scale. = T),
  
  #PERMANOVA of PCA groups
  pca_test = cbind(trait_pca_info, pca_res$x) %>% 
    rename("Habitat" = "Site") %>% 
    group_by(Habitat, Treatment) %>% 
    select(-Site_trt, -Year, -Treatment, -PlotID), 
  
  perm = adonis(pca_test[c(3:14)] ~ pca_test$Treatment * pca_test$Habitat, method='eu'),
  
  
  
  #### plot 3: mean trait values by plot ####
  Anova_Trait = Trait_Mean %>%
    group_by(Trait) %>%
    nest(data = -Trait) %>%
    mutate(
      aov = map(data, ~ aov(mean ~ Treatment*Site, data = .x)),
      aov_tidy = map(aov, tidy)
    ),

  Anova_Trait_Tidy = Anova_Trait %>%
    select(Trait, aov_tidy) %>%
    unnest(aov_tidy),
  
  
  ## Intra vs. Inter
  Var_Split_Exp = Intra_vs_Inter(Traits, Trait_Mean),
  Var_Split = Intra_vs_Inter_var_split(Var_Split_Exp)
  
)