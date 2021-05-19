#### Flux Figures #### 


#### GRAPH of flux-means of sites ####

make_flux_mean_figures <- function(ITEX.mean.fluxes){
  
  ITEX.mean.fluxes <- ITEX.mean.fluxes %>% 
    mutate(Site = factor(Site, levels = c("SB", "CH", "DH")))
  
  NEEplot <- ggplot (ITEX.mean.fluxes, aes(x = Site, y = NEE_ln, fill = Treatment)) +
    geom_boxplot() +
    scale_size_manual(values=c(3,3,3)) +
    scale_fill_manual(values = c('gray70', 'red')) +
    labs(x = "Habitat") +
    ylab(bquote('NEE ('*mu~'mol' ~CO[2]~ m^-2~s^-1*')')) +
    ylim(-9, 0.5) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      legend.position = "none"
    ) +
    stat_summary(geom = 'text', label = c('a', 'b', 'a'),
                 fun = max,
                 vjust = -1,
                 size = 5)
  
  
  #### figure code for Reco ####
  #pdf("Reco_2019.pdf")
  Recoplot <- ggplot(ITEX.mean.fluxes, aes(x = Site, y = ER_ln, fill = Treatment)) +
    geom_boxplot() +
    scale_fill_manual(values = c('gray70', 'red')) +
    labs(x = "Habitat") +
    ylab(bquote('Reco ('*mu~'mol' ~CO[2]~ m^-2~s^-1*')')) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      legend.position = "none") +
    stat_summary(geom = 'text', label = c('a', 'ab', 'b'),
                 fun = max,
                 vjust = -1,
                 size = 5) +
    ylim(-9, 0.5)
  
  
  #### figure code for GEP ####
  #pdf("GEP_2019.pdf")
  GEPplot <- ggplot(ITEX.mean.fluxes, aes(x = Site, y = GPP700, fill = Treatment)) +
    geom_boxplot() +
    scale_fill_manual(values = c('gray70', 'red')) +
    labs(x = "Habitat") +
    ylab(bquote('PAR-Standardized GPP ('*mu~'mol' ~CO[2]~ m^-2~s^-1*')')) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      legend.position = "none") +
    geom_text(aes(y = 11), label = c('a', 'ab', 'b')) +
    ylim(-0.5, 9)
  geom_text(geom = 'text', label = c('a', 'ab', 'b'),
            fun = max,
            vjust = -1,
            size = 5) 
  
}


###### USING NOT STANDARDIZED FLUXES!!!!!! #########
# Effect of single predictors on NEE. GPP and Reco
#### Effect size plots ####
#ITEX.Trait_Fluxes <- Flux_and_Traits

make_effect_size_figure <- function(ITEX.Trait_Fluxes){
  
  #effect of single predictors on fluxes
  Flux_analyses <- ITEX.Trait_Fluxes %>% 
    select(Site, PlotID, Treatment, NEE_ln, ER_ln, GPP700, SoilTemp, SoilMoist, CanTemp_Light, Richness, Evenness, Diversity, Height_cm, Graminoid:Lichen, Evergreen, Decidious, ITV_C:No_ITV_SLA) %>%
    rename(NEE = NEE_ln, GPP = GPP700, Reco = ER_ln) %>%
    pivot_longer(cols = SoilTemp:No_ITV_SLA, names_to = "prediction", values_to = "value") %>% 
    pivot_longer(cols = NEE:GPP, names_to = "response", values_to = "C_flux") %>% 
    group_by(response, prediction) %>%
    mutate(value = scale(value)[,1]) %>%
    do(tidy(lm(C_flux ~ value, data = .))) %>%
    filter(!term == "(Intercept)") %>% 
    mutate(lower = (estimate - std.error * 1.96),
           upper = (estimate + std.error * 1.96),
           overlapp_zero = if_else(p.value < 0.05, "signi", "non-signi"),
           response = factor(response, levels = c("GPP", "Reco", "NEE")))
  
  
  effect_size_figure <- ggplot(Flux_analyses, aes(x = prediction, y = estimate, 
                                                  shape = response, 
                                                  fill = prediction, 
                                                  alpha = p.value < 0.05,
                                                  ymin = lower, ymax = upper)) +
    geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 0, linetype = "solid", color = "grey") +
    geom_point(position = position_dodge(width = 0.5), size = 3) +
    scale_shape_manual(labels = c("NEE", "GPP", "Reco"), values = c(22, 24,21)) +
    scale_fill_manual(values = c(rep("#F0E442", 18), rep("#0072B2", 10), rep("grey90", 3))) +
    scale_alpha_manual(values = c(0.5, 1)) +
    scale_x_discrete(labels = c("No ITV CN", "No ITV N", "No ITV C", "No ITV P", "No ITV SLA", "No ITV LDMC", "No ITV LT", "No ITV LA", "No ITV Height", "ITV CN", "ITV N", "ITV C", "ITV P", "ITV SLA", "ITV LDMC", "ITV LT", "ITV LA", "ITV Height", "Richness", "Evenness", "Diversity", "Plant Height", "Graminoid", "Forb", "Bryophyte", "Evergreen", "Decidious", "Lichen", "CanTemp Light","SoilTemp", "SoilMoist")) +
    labs(x = "", y = "Effect size") +
    facet_grid(~response) +
    theme(axis.title.x = element_text(size = 14), 
          axis.text = element_text(size = 13),
          strip.background = element_rect(colour="black", fill="white"), 
          panel.background = element_rect(fill= "white"), 
          panel.border = element_rect(colour = "black", fill=NA), 
          strip.text.x = element_text(size=12, face="bold"),  
          axis.line = element_line(colour = "black"), 
          legend.position = "none") +
    coord_flip()
  #ggsave("effect_size_figure.jpg", effect_size_figure, height = 10, width = 13)
  
  return(effect_size_figure)
  
}

# trait_model_output <- Trait_Model_Output
# model_selection_output <- Model_Output
make_flux_figure <- function(trait_model_output, model_selection_output){
  
  p1 <- trait_model_output %>%
    mutate(ITV = ITV - noITV,
           ITV = abs(ITV)) %>%
    gather(key = ITV, value = Var.exp, noITV:ITV) %>%
    ggplot(aes(x = Cflux, y = Var.exp * 100, fill = ITV)) +
    geom_col(position = "stack") +
    scale_fill_manual(values = c("grey20", "grey70"), 
                      #limits = c("ITV", "noITV"),
                      labels = c("intra", "inter"), 
                      name = "Trait variation") +
    geom_hline(yintercept = 0, colour = "grey40") +
    labs(y = '', tag = "C") +
    scale_y_continuous(limits = c(-10, 70), 
                       breaks = seq(-10, 70, by = 10)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = c(0.2, 0.8))
  
  p2 <- model_selection_output %>% 
    filter(ITV == "ITV") %>% 
    ggplot(aes(x = Cflux, y = value*100, fill = Variables)) +
    geom_col(position = "stack") +
    scale_fill_manual(values = c("grey90", "#0072B2", "#0072B2", "#009E73" , "#009E73", "#F0E442", "#F0E442"), 
                      labels = c("Environment", "Environment & Taxonomy", "Taxonomy", "All", "Taxonomy & Trait", "Environment & Trait", "Trait")) +
    geom_col_pattern(aes(#specify angle
      pattern_angle = Variables,
      # specify patter
      pattern = Variables),
      pattern_fill = "black",
      pattern_spacing = 0.01) +
    # distinguish pattern type
    scale_pattern_manual(values = c("stripe", "stripe", "none", "stripe" , "none", "stripe", "none"),
                         labels = c("Environment", "Environment & Taxonomy", "Taxonomy", "All", "Taxonomy & Trait", "Environment & Trait", "Trait")) +
    #distinguish pattern angle
    scale_pattern_angle_manual(values = c(45, 45, 0, 45 , 0, 45, 0),
                               labels = c("Environment", "Environment & Taxonomy", "Taxonomy", "All", "Taxonomy & Trait", "Environment & Trait", "Trait"),
                               guide = guide_legend(override.aes = list(pattern_spacing = 0.005))) +
    geom_hline(yintercept = 0, colour = "grey40") +
    scale_y_continuous(limits = c(-10, 70), breaks = seq(-10, 70, by = 10)) +
    labs(x = "", y = "Explained variance %", tag = "A") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 13),
          axis.title = element_text(size = 14),
          legend.title=element_text(size=13),
          legend.text=element_text(size=12),
          legend.position = c(0.65,0.85))
  
  
  
  p3 <- model_selection_output %>% 
    filter(ITV == "no_ITV") %>%
    ggplot(aes(x = Cflux, y = value*100, fill = Variables)) +
    geom_col(position = "stack") +
    scale_fill_manual(values = c("grey90", "#0072B2", "#0072B2", "#009E73" , "#009E73", "#F0E442", "#F0E442"), 
                      labels = c("Environment", "Environment & Taxonomy", "Taxonomy", "All", "Taxonomy & Trait", "Environment & Trait", "Trait")) +
    geom_col_pattern(aes(#specify angle
      pattern_angle = Variables,
      # specify patter
      pattern = Variables),
      pattern_fill = "black",
      pattern_spacing = 0.01) +
    # distinguish pattern type
    scale_pattern_manual(values = c("stripe", "stripe", "none", "stripe" , "none", "stripe", "none"),
                         labels = c("Environment", "Environment & Taxonomy", "Taxonomy", "All", "Taxonomy & Trait", "Environment & Trait", "Trait")) +
    #distinguish pattern angle
    scale_pattern_angle_manual(values = c(45, 45, 0, 45 , 0, 45, 0),
                               labels = c("Environment", "Environment & Taxonomy", "Taxonomy", "All", "Taxonomy & Trait", "Environment & Trait", "Trait")) +
    geom_hline(yintercept = 0, colour = "grey40") +
    scale_y_continuous(limits = c(-10, 70), breaks = seq(-10, 70, by = 10)) +
    labs(x = "", y = "", tag = "B") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 13),
          axis.title = element_text(size = 14),
          legend.position = "none")
  
  carbon_flux_figure <- p2 + p3 + p1
  
  #ggsave("carbon_flux_figure.jpg", carbon_flux_figure, height = 8, width = 15)
  
  return(carbon_flux_figure)
  
}



