# Community fiugres

# change in community metrics
community_metrics_figure <- function(anova_t, metric_plot_dist, t_test){
  
  anova_text <- anova_t %>% 
    ungroup() %>% 
    filter(response != "Diversity") %>%
    mutate(response = plyr::mapvalues(response, from = c("propBryo", "propLichen", "sumAbundance", "totalForb", "totalGraminoid", "totaleShrub", "totaldShrub"), to = c("Bryophyte Abundance", "Lichen Abundance", "Vascular Abundance", "Forb Abundance", "Graminoid Abundance", "Evergreen Shrub Abundance", "Deciduous Shrub Abundance"))) %>%
    #filter(response != "Forb Abundance", response != "Bryophyte Abundance", response != "Lichen Abundance") %>%
    mutate(term = plyr::mapvalues(term, from = c("Treatment", "Site", "Treatment:Site"), to = c("T", "H", "TxH"))) %>%
    filter(term != "Residuals") %>%
    mutate(test = paste(term, ifelse(p.value < 0.05, "*", ifelse(p.value<0.1 & p.value > 0.05, "+", "")), sep = " ")) %>%
    mutate(test = ifelse(grepl("\\*", test), test, ifelse(grepl("\\+", test), test, NA))) %>%
    pivot_wider(id_cols = response, names_from = term, values_from = test) %>%
    mutate(T = ifelse(is.na(T), "", T)) %>%
    mutate(H = ifelse(is.na(H), "", H)) %>%
    mutate(text = trimws(ifelse(!is.na(TxH), TxH, paste(T, H)))) %>%
    mutate(response = factor(response, levels = c("Bray Curtis Distance", "Evenness", "Richness","Vascular Abundance", "Forb Abundance", "Graminoid Abundance", "Evergreen Shrub Abundance", "Deciduous Shrub Abundance", "Bryophyte Abundance", "Lichen Abundance")))
  
  metric_change <- metric_plot_dist %>% 
    filter(response != "Diversity") %>%
    mutate(response = plyr::mapvalues(response, from = c("propBryo", "propLichen", "sumAbundance", "totalForb", "totalGraminoid", "totaleShrub", "totaldShrub"), to = c("Bryophyte Abundance", "Lichen Abundance", "Vascular Abundance", "Forb Abundance", "Graminoid Abundance", "Evergreen Shrub Abundance", "Deciduous Shrub Abundance"))) %>%
    mutate(response = factor(response, levels = c("Bray Curtis Distance", "Evenness", "Richness","Vascular Abundance", "Forb Abundance", "Graminoid Abundance", "Evergreen Shrub Abundance", "Deciduous Shrub Abundance", "Bryophyte Abundance", "Lichen Abundance"))) %>%
    group_by(response) %>%
    #filter(response != "Forb Abundance", response != "Bryophyte Abundance", response != "Lichen Abundance") %>%
    mutate(y_max = max(dist), y_min = min(dist)) %>%
    mutate(Site = factor(Site, levels = c("SB", "CH", "DH"))) %>%
    #filter(Year != 2003) %>%
    ggplot() +
    geom_hline(yintercept = 0) +
    geom_boxplot(aes(x = Site, y = dist, fill = Treatment)) +
    scale_fill_manual(values = c("darkgray", "red")) +
    facet_wrap(~response, scales = "free", ncol = 2) +
    ylab("Change in Metric") +
    xlab("Habitat Type") +
    theme_classic() +
    theme(text = element_text(size = 15),
          legend.position = "top",
          legend.direction = "horizontal",
          strip.background = element_blank())+
    #stat_compare_means(aes(group = Site), label = "p.signif", method = "anova", hide.ns = F, label.x.npc = 0.05, label.y.npc = 0.05)+
    #stat_compare_means(aes(x = Site, y = dist, group = Treatment), label = "p.signif", method = "anova", hide.ns = T, label.y.npc = 0) +
    geom_blank(aes(y = y_min + 0.5*y_min)) +
    geom_blank(aes(y = y_max + 0.4*y_max)) +
    geom_text(aes(label = text, x = 0.5, y = Inf, hjust = 0, vjust = 2), size = 4, color = "black",  data = anova_text) +
    geom_text(aes(label = Sig, x = Site, y = -Inf, hjust = 0.5, vjust = 0, group = Treatment), size = 6, position = position_dodge(0.75),color = "black",  data = t_test)
  
  return(metric_change)
  
}


community_metrics_figure_supp <- function(anova_t, metric_plot_dist, t_test_supp){
  
anova_text_supp <- anova_t %>% 
  ungroup() %>% 
  filter(response != "Diversity") %>%
  mutate(response = plyr::mapvalues(response, from = c("propBryo", "propLichen", "sumAbundance", "totalForb", "totalGraminoid", "totaleShrub", "totaldShrub"), to = c("Bryophyte Abundance", "Lichen Abundance", "Vascular Abundance", "Forb Abundance", "Graminoid Abundance", "Evergreen Shrub Abundance", "Deciduous Shrub Abundance"))) %>%
  filter(response != "Bray Curtis Distance", response != "Evenness", response != "Richness", response != "Vascular Abundance", response != "Graminoid Abundance", response != "Shrub Abundance") %>%
  mutate(term = plyr::mapvalues(term, from = c("Treatment", "Site", "Treatment:Site"), to = c("T", "H", "TxH"))) %>%
  filter(term != "Residuals") %>%
  mutate(test = paste(term, ifelse(p.value < 0.05, "*", ifelse(p.value<0.1 & p.value > 0.05, "+", "")), sep = " ")) %>%
  mutate(test = ifelse(grepl("\\*", test), test, ifelse(grepl("\\+", test), test, NA))) %>%
  pivot_wider(id_cols = response, names_from = term, values_from = test) %>%
  mutate(T = ifelse(is.na(T), "", T)) %>%
  mutate(H = ifelse(is.na(H), "", H)) %>%
  mutate(text = trimws(ifelse(!is.na(TxH), TxH, paste(T, H)))) %>%
  mutate(response = factor(response, levels = c("Bray Curtis Distance", "Evenness", "Richness","Vascular Abundance", "Forb Abundance", "Graminoid Abundance", "Evergreen Shrub Abundance", "Deciduous Shrub Abundance", "Bryophyte Abundance", "Lichen Abundance")))


metric_change_supp <- metric_plot_dist %>% filter(response != "Diversity") %>%
  mutate(response = plyr::mapvalues(response, from = c("propBryo", "propLichen", "sumAbundance", "totalForb", "totalGraminoid", "totaleShrub", "totaldShrub"), to = c("Bryophyte Abundance", "Lichen Abundance", "Vascular Abundance", "Forb Abundance", "Graminoid Abundance", "Evergreen Shrub Abundance", "Deciduous Shrub Abundance"))) %>%
  mutate(response = factor(response, levels = c("Bray Curtis Distance", "Evenness", "Richness","Vascular Abundance", "Forb Abundance", "Graminoid Abundance", "Evergreen Shrub Abundance", "Deciduous Shrub Abundance", "Bryophyte Abundance", "Lichen Abundance"))) %>%
  group_by(response) %>%
  filter(response != "Bray Curtis Distance", response != "Evenness", response != "Richness", response != "Vascular Abundance", response != "Graminoid Abundance", response != "Shrub Abundance") %>%
  mutate(y_max = max(dist), y_min = min(dist)) %>%
  mutate(Site = factor(Site, levels = c("SB", "CH", "DH"))) %>%
  #filter(Year != 2003) %>%
  ggplot() +
  geom_hline(yintercept = 0) +
  geom_boxplot(aes(x = Site, y = dist, fill = Treatment)) +
  scale_fill_manual(values = c("darkgray", "red")) +
  facet_wrap(~response, scales = "free") +
  ylab("Change in Metric") +
  xlab("Habitat Type") +
  theme_classic() +
  theme(text = element_text(size = 15),
        legend.position = "top",
        legend.direction = "horizontal",
        strip.background = element_blank())+
  #stat_compare_means(aes(group = Site), label = "p.signif", method = "anova", hide.ns = F, label.x.npc = 0.05, label.y.npc = 0.05)+
  #stat_compare_means(aes(x = Site, y = dist, group = Treatment), label = "p.signif", method = "anova", hide.ns = T, label.y.npc = 0) +
  geom_blank(aes(y = y_min + 0.5*y_min)) +
  geom_blank(aes(y = y_max + 0.4*y_max)) +
  geom_text(aes(label = text, x = 0.5, y = Inf, hjust = 0, vjust = 2), size = 4, color = "black",  data = anova_text_supp) +
  geom_text(aes(label = Sig, x = Site, y = -Inf, hjust = 0.5, vjust = 0, group = Treatment), size = 6, position = position_dodge(0.75),color = "black",  data = t_test_supp)

return(metric_change_supp)

}



#### Figure S5. change by year ####
metric_time_figure <- function(comm_resp){
  
  metric_time <- comm_resp %>%
    select(Year, Site, Treatment, Richness, Evenness, totalForb, totaleShrub, totaldShrub, totalGraminoid, propBryo, propLichen, PlotID) %>%
    rename("Forb\nAbundance" = totalForb, "Ever. Shrub\nAbundance" = totaleShrub, "Decid. Shrub\nAbundance" = totaldShrub, "Graminoid\nAbundance" = totalGraminoid, "Bryo\nAbundance" = propBryo, "Lichen\nAbundance" = propLichen) %>%
    gather(key = metric, value = value, -Year, -Site, -Treatment, -PlotID) %>%
    group_by(metric) %>% mutate(max_val = max(value)) %>% ungroup() %>%
    mutate(metric = factor(metric, levels = c("Richness", "Evenness", "Forb\nAbundance", "Graminoid\nAbundance", "Ever. Shrub\nAbundance", "Decid. Shrub\nAbundance", "Bryo\nAbundance", "Lichen\nAbundance"))) %>%
    mutate(Site = factor(Site, levels = c("SB", "CH", "DH"))) %>%
    ggplot(aes(x = as.factor(Year), y = value, color = Treatment, group = PlotID)) +
    geom_point() +
    geom_line() +
    facet_grid(metric ~ Site, scales = "free", switch = "both") +
    scale_color_manual(values = c("gray45", "red")) +
    ylab("Community Metric") +
    xlab("Habitat Type") +
    theme_classic() +
    theme(text = element_text(size = 13),
          panel.background = element_rect(color = "black", fill = NA),
          strip.placement = "outside",
          strip.background = element_rect(fill = "white", color = "white"),
          legend.position = "top") +
    stat_compare_means(aes(group = Treatment), label = "p.signif", method = "anova", hide.ns = T, label.y.npc = 0.9) +
    geom_blank(aes(y = max_val + 0.2*max_val))
  
  return(metric_time)
  
}



make_ordination <- function(NMDS_data){
  
  plot_annotation <- tibble(Site = c("SB", "CH", "DH"),
                            label = c("Year*", "", "Year*"))
  CommunityOrdination <- NMDS_data %>% 
    mutate(Site = factor(Site, levels = c("SB", "CH", "DH"))) %>% 
    ggplot(aes(x = NMDS1, y = NMDS2)) +
    geom_point(aes(size = ifelse(Year == min(as.numeric(Year)), "First", "Other"), shape = Treatment)) +
    geom_path(aes(linetype = Treatment, group = PlotID)) + 
    coord_equal() +
    scale_size_discrete(name = "Year", range = c(1.2, 2.5), limits = c("Other", "First"), breaks = c("First", "Other")) +
    scale_shape_manual(values = c(1, 16)) + 
    scale_linetype_manual(values = c("dashed", "solid")) + 
    labs(x = "NMDS axis 1", y = "NMDS axis 2") +
    geom_text(data = plot_annotation, aes(x = 0.5, y = 0.5, label = label)) +
    facet_grid(~ fct_relevel(Site, "SB", "CH", "DH")) +
    theme_bw()
  
  return(CommunityOrdination)
  
}



make_height_figure <- function(height){
  
  canopy_height_figure <- ggplot(height, aes(x = Site, y = Value, fill = Treatment)) +
    geom_boxplot() +
    scale_fill_manual(values = c("grey", "red")) +
    labs(y = "Canopy height cm", x = "Habitat Type") +
    annotate("text", x = 2, y = 30, label = "T *") +
    annotate("text", x = 3, y = 30, label = "T *") +
    theme_classic() +
    theme(text = element_text(size = 20))
  
  return(canopy_height_figure)

}