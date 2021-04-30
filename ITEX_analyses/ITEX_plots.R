#Load packages
library(tidyverse)
library(ggpubr)
library(vegan)

#Load data
load("community/cleaned_data/CommResp.Rdata")
CommunitySV_ITEX_2003_2015 <- read.csv("community/cleaned_data/ITEX_Svalbard_2003_2015_Community_cleaned.csv")
load("climate/data/soil_moisture2003.rdata")
load("climate/data/soil_moisture2004.rdata")
tpi <- read.csv("climate/data/twiFull.csv") %>% 
  mutate(PlotID = paste(habitat, id, sep = "-"))
load("traits/cleaned_data/traitMean.RData")

traitMean <- traitMean %>% ungroup() %>% 
  mutate(Trait = plyr::mapvalues(Trait, from = c("P_percent", "dC13_permil", "dN15_permil"), to = c("P_Ave", "dC13_percent", "dN15_percent"))) %>% 
  mutate(Treatment = substr(Site_trt, 4, 6))

metaItex <- CommunitySV_ITEX_2003_2015 %>% 
  distinct(Site, Treatment, PlotID)

#to filter away the plots that were "iced", run these lines of code
#also change habitat labels here
CommunitySV_ITEX_2003_2015 <- CommunitySV_ITEX_2003_2015 %>% 
  filter(PlotID != "CAS-4", PlotID != "CAS-6", PlotID != "CAS-9", PlotID != "CAS-10") %>% 
  mutate(Site = plyr::mapvalues(Site, from = c("BIS", "CAS", "DRY"), to = c("SB", "CH", "DH"))) %>% 
  mutate(Site = factor(Site, levels = c("SB", "CH", "DH")))


traitMean <- traitMean %>% 
  filter(PlotID != "CAS-4", PlotID != "CAS-6", PlotID != "CAS-9", PlotID != "CAS-10") %>% 
  mutate(Site = plyr::mapvalues(Site, from = c("BIS", "CAS", "DRY"), to = c("SB", "CH", "DH"))) %>% 
  mutate(Site = factor(Site, levels = c("SB", "CH", "DH")))

CommResp <- CommResp %>% ungroup() %>% 
  filter(PlotID != "CAS-4", PlotID != "CAS-6", PlotID != "CAS-9", PlotID != "CAS-10") %>% 
  mutate(Site = plyr::mapvalues(Site, from = c("BIS", "CAS", "DRY"), to = c("SB", "CH", "DH"))) %>% 
  mutate(Site = factor(Site, levels = c("SB", "CH", "DH")))

#### plot 1: multivariate space ####
#community distances
comm_distances <- CommunitySV_ITEX_2003_2015 %>% 
  select(-Spp, -FunctionalGroup) %>% 
  filter(Year != 2009) %>% 
  spread(key = Taxon, value = Abundance, fill = 0) %>% 
  group_by(PlotID) %>% 
  do( data_frame(out = as.vector(vegdist(select(., -(Site:Year)), method = "bray")))) %>% 
  left_join(metaItex)  %>% 
  mutate(Site = plyr::mapvalues(Site, from = c("BIS", "CAS", "DRY"), to = c("SB", "CH", "DH"))) %>% 
  mutate(Site = factor(Site, levels = c("SB", "CH", "DH")))

comm_dist <- comm_distances %>% ungroup() %>% mutate(y_max = max(out), y_min = min(out)) %>%
  ggplot(aes(x = Site, y = out, fill = Treatment)) +
  geom_boxplot() +
  scale_fill_manual(values = c("gray", "red")) +
  ylab("Bray Curtis Distance") +
  xlab("Habitat Type") +
  ggtitle("Community Change") +
  theme_classic() +
  theme(text = element_text(size = 20))+
  stat_compare_means(aes(group = Site), label = "p.signif", method = "anova", hide.ns = F, label.x = 0.5, label.y.npc = 0.05)+
  stat_compare_means(aes(group = Treatment), label = "p.signif", method = "anova", hide.ns = F, label.y.npc = 0.95) +
  geom_blank(aes(y = y_min)) +
  geom_blank(aes(y = y_max + 0.35*y_max))

# trait distances, no longer used.
# trait_distances <- traitMean %>% 
#   filter(Year != 2009) %>% 
#   select(-mean_noitv) %>% 
#   filter(Trait != "Dry_Mass_Total_g", Trait != "Wet_Mass_Total_g", Trait != "wetSLA_cm2_per_g", Trait != "Wet_mass_g") %>% 
#   spread(key = Trait, value = mean) %>% 
#   group_by(PlotID) %>% 
#   do( tibble(out = as.vector(dist(select(., -(Site:Year)), method = "euclidian")))) %>% 
#   left_join(metaItex)
# 
# trait_distances_noitv <- traitMean %>% 
#   filter(Year != 2009) %>% 
#   select(-mean) %>% 
#   filter(Trait != "Dry_Mass_Total_g", Trait != "Wet_Mass_Total_g", Trait != "wetSLA_cm2_per_g", Trait != "Wet_mass_g") %>% 
#   spread(key = Trait, value = mean_noitv) %>% 
#   group_by(PlotID) %>% 
#   do( tibble(out = as.vector(dist(select(., -(Site:Year)), method = "euclidian")))) %>% 
#   left_join(metaItex)
# 
# 
# trait_dist <- trait_distances %>% ungroup() %>% mutate(y_max = max(out), y_min = min(out)) %>% 
#   ggplot(aes(x = Site, y = out, fill = Treatment)) +
#   geom_boxplot() +
#   scale_fill_manual(values = c("gray", "red")) +
#   ylab("Euclidian Distance") +
#   xlab("Habitat Type") +
#   ggtitle("Trait Change") +
#   theme_classic() +
#   theme(text = element_text(size = 20)) +
#   stat_compare_means(aes(group = Site), label = "p.signif", method = "anova", hide.ns = F, label.x = 0.5, label.y.npc = 0.05)+
#   stat_compare_means(aes(group = Treatment), label = "p.signif", method = "anova", hide.ns = F, label.y.npc = 0.95) +
#   geom_blank(aes(y = y_min)) +
#   geom_blank(aes(y = y_max + 0.35*y_max))
# 
# ggarrange(comm_dist, trait_dist, ncol = 2, common.legend = T)

#### plot 2: change in community metrics ####
#calculate change in community metrics
metric_plot_dist <- CommResp %>% 
  filter(Year != 2009) %>% 
  gather(key = response, value = value, -Year, -Site, -Treatment, -PlotID) %>% 
  group_by(Treatment, PlotID, Site, response) %>% 
  summarize(dist = diff(value))%>% 
  #left_join(soil_moisture2003, by = "PlotID") %>% 
  #left_join(soil_moisture2004, by = "PlotID") %>% 
  filter(response == "Richness" | response == "Diversity" | response == "Evenness" | response == "sumAbundance" | response == "totalGraminoid" | response == "totalForb" | response == "totalShrub" | response == "propLichen" | response == "propBryo")
  #left_join(soil_moisture2004, by = "PlotID") %>% 
  #left_join(tpi, by = "PlotID")

comm_distances_merge <- comm_distances %>% 
  rename("dist" = "out") %>% 
  mutate(response = "Bray Curtis Distance")

metric_plot_dist <- bind_rows(metric_plot_dist, comm_distances_merge)

t_test <- metric_plot_dist %>% filter(response != "Diversity") %>% 
  mutate(response = plyr::mapvalues(response, from = c("propBryo", "propLichen", "sumAbundance", "totalForb", "totalGraminoid", "totalShrub"), to = c("Bryophyte Abundance", "Lichen Abundance", "Vascular Abundance", "Forb Abundance", "Graminoid Abundance", "Shrub Abundance"))) %>%
  mutate(response = factor(response, levels = c("Bray Curtis Distance", "Evenness", "Richness","Vascular Abundance", "Forb Abundance", "Graminoid Abundance", "Shrub Abundance", "Bryophyte Abundance", "Lichen Abundance"))) %>% 
  dplyr::filter(response != "Forb Abundance") %>% 
  filter(response != "Bryophyte Abundance") %>% 
  filter(response != "Lichen Abundance") %>% 
  droplevels() %>% 
  group_by(response, Site, Treatment) %>% 
  summarise(P = t.test(dist, mu = 0)$p.value,
            Sig = ifelse(P < 0.05, "*", ifelse(P<0.1 & P > 0.05, "+", "")),
            MaxWidth = max(dist))%>% ungroup() %>% 
  mutate(response = factor(response, levels = c("Bray Curtis Distance", "Evenness", "Richness","Vascular Abundance", "Forb Abundance", "Graminoid Abundance", "Shrub Abundance", "Bryophyte Abundance", "Lichen Abundance")))

library(multcompView)
library(broom)
anova_t <- metric_plot_dist %>% 
  group_by(response) %>% 
  nest(data = -response) %>% 
  mutate(
    aov = map(data, ~ aov(dist ~ Treatment*Site, data = .x)),
    aov_tidy = map(aov, tidy)
  ) 

anova_t <- anova_t %>% 
  select(response, aov_tidy) %>% 
  unnest(aov_tidy)

anova_text <- anova_t %>% ungroup() %>% filter(response != "Diversity") %>% 
  mutate(response = plyr::mapvalues(response, from = c("propBryo", "propLichen", "sumAbundance", "totalForb", "totalGraminoid", "totalShrub"), to = c("Bryophyte Abundance", "Lichen Abundance", "Vascular Abundance", "Forb Abundance", "Graminoid Abundance", "Shrub Abundance"))) %>%
  filter(response != "Forb Abundance", response != "Bryophyte Abundance", response != "Lichen Abundance") %>% 
  mutate(term = plyr::mapvalues(term, from = c("Treatment", "Site", "Treatment:Site"), to = c("T", "H", "TxH"))) %>% 
  filter(term != "Residuals") %>% 
  mutate(test = paste(term, ifelse(p.value < 0.05, "*", ifelse(p.value<0.1 & p.value > 0.05, "+", "")), sep = " ")) %>% 
  mutate(test = ifelse(grepl("\\*", test), test, ifelse(grepl("\\+", test), test, NA))) %>% 
  pivot_wider(id_cols = response, names_from = term, values_from = test) %>% 
  mutate(T = ifelse(is.na(T), "", T)) %>% 
  mutate(H = ifelse(is.na(H), "", H)) %>% 
  mutate(text = trimws(ifelse(!is.na(TxH), TxH, paste(T, H)))) %>%
  mutate(response = factor(response, levels = c("Bray Curtis Distance", "Evenness", "Richness","Vascular Abundance", "Forb Abundance", "Graminoid Abundance", "Shrub Abundance", "Bryophyte Abundance", "Lichen Abundance")))


metric_change <- metric_plot_dist %>% filter(response != "Diversity") %>% 
  mutate(response = plyr::mapvalues(response, from = c("propBryo", "propLichen", "sumAbundance", "totalForb", "totalGraminoid", "totalShrub"), to = c("Bryophyte Abundance", "Lichen Abundance", "Vascular Abundance", "Forb Abundance", "Graminoid Abundance", "Shrub Abundance"))) %>%
  mutate(response = factor(response, levels = c("Bray Curtis Distance", "Evenness", "Richness","Vascular Abundance", "Forb Abundance", "Graminoid Abundance", "Shrub Abundance", "Bryophyte Abundance", "Lichen Abundance"))) %>% 
  group_by(response) %>% 
  filter(response != "Forb Abundance", response != "Bryophyte Abundance", response != "Lichen Abundance") %>% 
  mutate(y_max = max(dist), y_min = min(dist)) %>% 
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
  geom_text(aes(label = text, x = 0.5, y = Inf, hjust = 0, vjust = 2), size = 4, color = "black",  data = anova_text) +
  geom_text(aes(label = Sig, x = Site, y = -Inf, hjust = 0.5, vjust = 0, group = Treatment), size = 6, position = position_dodge(0.75),color = "black",  data = t_test)

tiff("plots/metric_change.tiff", width = 8, height = 6, units = "in", res = 400)
metric_change
dev.off()

#### create supplemental data community figures ####
t_test_supp <- metric_plot_dist %>% filter(response != "Diversity") %>% 
  mutate(response = plyr::mapvalues(response, from = c("propBryo", "propLichen", "sumAbundance", "totalForb", "totalGraminoid", "totalShrub"), to = c("Bryophyte Abundance", "Lichen Abundance", "Vascular Abundance", "Forb Abundance", "Graminoid Abundance", "Shrub Abundance"))) %>%
  mutate(response = factor(response, levels = c("Bray Curtis Distance", "Evenness", "Richness","Vascular Abundance", "Forb Abundance", "Graminoid Abundance", "Shrub Abundance", "Bryophyte Abundance", "Lichen Abundance"))) %>% 
  filter(response != "Bray Curtis Distance", response != "Evenness", response != "Richness", response != "Vascular Abundance", response != "Graminoid Abundance", response != "Shrub Abundance") %>% 
  droplevels(.) %>% 
  group_by(response, Site, Treatment) %>% 
  summarise(P = t.test(dist, mu = 0)$p.value,
            Sig = ifelse(P < 0.05, "*", ifelse(P<0.1 & P > 0.05, "+", "")),
            MaxWidth = max(dist))%>% ungroup() %>% 
  mutate(response = factor(response, levels = c("Bray Curtis Distance", "Evenness", "Richness","Vascular Abundance", "Forb Abundance", "Graminoid Abundance", "Shrub Abundance", "Bryophyte Abundance", "Lichen Abundance")))


anova_text_supp <- anova_t %>% ungroup() %>% filter(response != "Diversity") %>% 
  mutate(response = plyr::mapvalues(response, from = c("propBryo", "propLichen", "sumAbundance", "totalForb", "totalGraminoid", "totalShrub"), to = c("Bryophyte Abundance", "Lichen Abundance", "Vascular Abundance", "Forb Abundance", "Graminoid Abundance", "Shrub Abundance"))) %>%
  filter(response != "Bray Curtis Distance", response != "Evenness", response != "Richness", response != "Vascular Abundance", response != "Graminoid Abundance", response != "Shrub Abundance") %>% 
  mutate(term = plyr::mapvalues(term, from = c("Treatment", "Site", "Treatment:Site"), to = c("T", "H", "TxH"))) %>% 
  filter(term != "Residuals") %>% 
  mutate(test = paste(term, ifelse(p.value < 0.05, "*", ifelse(p.value<0.1 & p.value > 0.05, "+", "")), sep = " ")) %>% 
  mutate(test = ifelse(grepl("\\*", test), test, ifelse(grepl("\\+", test), test, NA))) %>% 
  pivot_wider(id_cols = response, names_from = term, values_from = test) %>% 
  mutate(T = ifelse(is.na(T), "", T)) %>% 
  mutate(H = ifelse(is.na(H), "", H)) %>% 
  mutate(text = trimws(ifelse(!is.na(TxH), TxH, paste(T, H)))) %>%
  mutate(response = factor(response, levels = c("Bray Curtis Distance", "Evenness", "Richness","Vascular Abundance", "Forb Abundance", "Graminoid Abundance", "Shrub Abundance", "Bryophyte Abundance", "Lichen Abundance")))


metric_change_supp <- metric_plot_dist %>% filter(response != "Diversity") %>% 
  mutate(response = plyr::mapvalues(response, from = c("propBryo", "propLichen", "sumAbundance", "totalForb", "totalGraminoid", "totalShrub"), to = c("Bryophyte Abundance", "Lichen Abundance", "Vascular Abundance", "Forb Abundance", "Graminoid Abundance", "Shrub Abundance"))) %>%
  mutate(response = factor(response, levels = c("Bray Curtis Distance", "Evenness", "Richness","Vascular Abundance", "Forb Abundance", "Graminoid Abundance", "Shrub Abundance", "Bryophyte Abundance", "Lichen Abundance"))) %>% 
  group_by(response) %>% 
  filter(response != "Bray Curtis Distance", response != "Evenness", response != "Richness", response != "Vascular Abundance", response != "Graminoid Abundance", response != "Shrub Abundance") %>% 
  mutate(y_max = max(dist), y_min = min(dist)) %>% 
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

tiff("plots/metric_change_supp.tiff", width = 8, height = 4, units = "in", res = 400)
metric_change_supp
dev.off()


#### supplemental plot. change in richness, evenness, and abundance through time ####

metric_time <- CommResp %>% 
  select(Year, Site, Treatment, Richness, Evenness, totalForb, totalShrub, totalGraminoid, propBryo, propLichen, PlotID) %>%
  rename("Forb\nAbundance" = totalForb, "Shrub\nAbundance" = totalShrub, "Graminoid\nAbundance" = totalGraminoid, "Bryo\nAbundance" = propBryo, "Lichen\nAbundance" = propLichen) %>% 
  gather(key = metric, value = value, -Year, -Site, -Treatment, -PlotID) %>% 
  group_by(metric) %>% mutate(max_val = max(value)) %>% ungroup() %>% 
  mutate(metric = factor(metric, levels = c("Richness", "Evenness", "Forb\nAbundance", "Graminoid\nAbundance", "Shrub\nAbundance", "Bryo\nAbundance", "Lichen\nAbundance"))) %>% 
  ggplot(aes(x = as.factor(Year), y = value, fill = Treatment)) +
  geom_boxplot() +
  facet_grid(metric ~ Site, scales = "free", switch = "both") + 
  scale_fill_manual(values = c("darkgray", "red")) +
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

tiff("plots/metric_time.tiff", width = 5, height = 9, units = "in", res = 400)
metric_time
dev.off()

#### pca plot ####

trait_pca <- traitMean %>% 
  select(-mean_noitv) %>% 
  mutate(Trait = plyr::mapvalues(Trait, from = c("SLA_cm2_g", "LDMC", "Leaf_Area_cm2", "Leaf_Thickness_Ave_mm", "N_percent", "C_percent", "P_Ave", "CN_ratio", "dC13_percent", "dN15_percent", "Dry_Mass_g", "Plant_Height_cm"), to = c("SLA", "LDMC", "Leaf Area", "Leaf Thickness", "%N", "%C", "%P", "C:N", "delta13C", "delta15N", "Dry Mass", "Plant Height"))) %>% 
  pivot_wider(names_from = Trait, values_from = mean)

trait_pca_data <- trait_pca %>% 
  select("%C":"SLA")

trait_pca_info <- trait_pca %>% 
  select(Site:Treatment)

pca_res <- prcomp(trait_pca_data, center = T, scale. = T)

pca_points <- cbind(trait_pca_info, pca_res$x) %>% rename("Habitat" = "Site")
pca_arrows <- pca_res$rotation

library(ggfortify)
pca_plot <- autoplot(pca_res, data = pca_points, 
         shape = 'Treatment', 
         colour = "Habitat", 
         size = 3,
         loadings = TRUE, 
         loadings.colour = 'black',
         loadings.label = TRUE, 
         loadings.label.size = 4, 
         loadings.label.colour = "black",
         loadings.label.repel = T,
         parse = T) +
  theme_classic() +
  theme(legend.position = "top",
        text = element_text(size = 12)) +
  scale_color_manual(values = c("blue", "forestgreen", "orange"))

tiff(filename = "plots/pca_plot.tiff", width = 7, height = 5.75, units = "in", res = 400)
pca_plot
dev.off()

#PERMANOVA of PCA groups
pca_test <- pca_points %>% 
  group_by(Habitat, Treatment) %>% 
  select(-Site_trt, -Year, -Treatment, -PlotID) 

perm <- adonis(pca_test[c(3:14)] ~ pca_test$Treatment * pca_test$Habitat, method='eu')

perm

#### plot 3: mean trait values by plot ####
anova_trait <- traitMean %>% 
  group_by(Trait) %>% 
  nest(data = -Trait) %>% 
  mutate(
    aov = map(data, ~ aov(mean ~ Treatment*Site, data = .x)),
    aov_tidy = map(aov, tidy)
  ) 


anova_trait <- anova_trait %>% 
  select(Trait, aov_tidy) %>% 
  unnest(aov_tidy)




anova_text_trait <- anova_trait %>% ungroup() %>% mutate(Trait = plyr::mapvalues(Trait, from = c("SLA_cm2_g", "LDMC", "Leaf_Area_cm2", "Leaf_Thickness_Ave_mm", "N_percent", "C_percent", "P_Ave", "CN_ratio", "dC13_percent", "dN15_percent", "Dry_Mass_g", "Plant_Height_cm"), to = c("`SLA`*` `*(cm^2/g)", "`LDMC`*` `*(g/g)", "'Leaf'*' '*'Area'*' '*(cm^2)", "'Leaf'*' '*'Thickness'*' '*(mm)", "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')", "'Dry'*' '*'Mass'*' '*'(g)'", "'Plant'*' '*'Height'*' '*'(cm)'"))) %>% 
  mutate(Trait = factor(Trait, levels = c("'Plant'*' '*'Height'*' '*'(cm)'", "`SLA`*` `*(cm^2/g)", "'Dry'*' '*'Mass'*' '*'(g)'","'Leaf'*' '*'Area'*' '*(cm^2)",  "'Leaf'*' '*'Thickness'*' '*(mm)", "`LDMC`*` `*(g/g)",  "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')" ))) %>% 
  mutate(term = plyr::mapvalues(term, from = c("Treatment", "Site", "Treatment:Site"), to = c("T", "H", "TxH"))) %>% 
  filter(term != "Residuals") %>% 
  mutate(test = paste(term, ifelse(p.value < 0.05, "*", ifelse(p.value<0.1 & p.value > 0.05, "+", "")), sep = " ")) %>% 
  mutate(test = ifelse(grepl("\\*", test), test, ifelse(grepl("\\+", test), test, NA))) %>% 
  pivot_wider(id_cols = Trait, names_from = term, values_from = test) %>% 
  mutate(T = ifelse(is.na(T), "", T)) %>% 
  mutate(H = ifelse(is.na(H), "", H)) %>% 
  mutate(text = trimws(ifelse(!is.na(TxH), TxH, paste(T, H))))




trait_mean <- traitMean %>% ungroup() %>%  mutate(Trait = plyr::mapvalues(Trait, from = c("SLA_cm2_g", "LDMC", "Leaf_Area_cm2", "Leaf_Thickness_Ave_mm", "N_percent", "C_percent", "P_Ave", "CN_ratio", "dC13_percent", "dN15_percent", "Dry_Mass_g", "Plant_Height_cm"), to = c("`SLA`*` `*(cm^2/g)", "`LDMC`*` `*(g/g)", "'Leaf'*' '*'Area'*' '*(cm^2)", "'Leaf'*' '*'Thickness'*' '*(mm)", "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')", "'Dry'*' '*'Mass'*' '*'(g)'", "'Plant'*' '*'Height'*' '*'(cm)'"))) %>% 
  mutate(Trait = factor(Trait, levels = c("'Plant'*' '*'Height'*' '*'(cm)'", "`SLA`*` `*(cm^2/g)", "'Dry'*' '*'Mass'*' '*'(g)'","'Leaf'*' '*'Area'*' '*(cm^2)",  "'Leaf'*' '*'Thickness'*' '*(mm)", "`LDMC`*` `*(g/g)",  "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')" )))  %>% 
  filter(Year == "2015") %>% 
  filter(Year == 2015) %>% 
  #gather(key = key, value = value, -Site, -Year, -PlotID, -Trait, -Treatment, -Site_trt) %>%  #filter(trait == "Dry_Mass_g" | trait == "LDMC" | trait == "Leaf_Area_cm2" | trait == "Leaf_Thickness_Ave_mm" | trait == "Plant_Height_cm" | trait == "SLA_cm2_g") %>% #filter(key != "diff", key != "prop_diff") %>% 
  filter(PlotID != "CAS-4", PlotID != "CAS-9", PlotID != "CAS-10", PlotID != "CAS-6") %>% 
  group_by(Trait) %>% 
  mutate(y_max = max(mean), y_min = min(mean)) %>% 
  ggplot() +
  geom_boxplot(aes(x = Site, y = mean, fill = Treatment)) +
  scale_fill_manual(values = c("darkgray", "red")) +
  geom_blank(aes(y = y_max + 0.1*abs(y_max))) +
  ylab("CWM Trait Value") +
  xlab("Habitat Type") +
  theme_classic() +
  theme(text = element_text(size = 15),
        strip.background = element_blank(),
        legend.position = "top") +
  facet_wrap(~Trait, scales = "free", labeller = label_parsed) +
  geom_text(aes(label = text, x = 0, y = Inf, hjust = -0.15, vjust = 1), size = 3.5, color = "black",  data = anova_text_trait) 

tiff("plots/trait_mean.tiff", width = 9, height = 6, units = "in", res = 400)
trait_mean
dev.off()


traitMean <- traitMean %>% 
  mutate(itv_diff = mean-mean_noitv)

t_test_itv <- traitMean %>% 
  group_by(Trait, Site, Treatment) %>% 
  summarise(P = t.test(itv_diff, mu = 0)$p.value,
            Sig = ifelse(P < 0.05, "*", ifelse(P<0.1 & P > 0.05, "+", "")),
            MaxWidth = max(itv_diff))%>%
  ungroup() %>% 
  mutate(Trait = plyr::mapvalues(Trait, from = c("SLA_cm2_g", "LDMC", "Leaf_Area_cm2", "Leaf_Thickness_Ave_mm", "N_percent", "C_percent", "P_Ave", "CN_ratio", "dC13_percent", "dN15_percent", "Dry_Mass_g", "Plant_Height_cm"), to = c("`SLA`*` `*(cm^2/g)", "`LDMC`*` `*(g/g)", "'Leaf'*' '*'Area'*' '*(cm^2)", "'Leaf'*' '*'Thickness'*' '*(mm)", "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')", "'Dry'*' '*'Mass'*' '*'(g)'", "'Plant'*' '*'Height'*' '*'(cm)'"))) %>% 
  mutate(Trait = factor(Trait, levels = c("'Plant'*' '*'Height'*' '*'(cm)'", "`SLA`*` `*(cm^2/g)", "'Dry'*' '*'Mass'*' '*'(g)'","'Leaf'*' '*'Area'*' '*(cm^2)",  "'Leaf'*' '*'Thickness'*' '*(mm)", "`LDMC`*` `*(g/g)",  "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')" )))


itv_plot <- traitMean %>% mutate(Trait = plyr::mapvalues(Trait, from = c("SLA_cm2_g", "LDMC", "Leaf_Area_cm2", "Leaf_Thickness_Ave_mm", "N_percent", "C_percent", "P_Ave", "CN_ratio", "dC13_percent", "dN15_percent", "Dry_Mass_g", "Plant_Height_cm"), to = c("`SLA`*` `*(cm^2/g)", "`LDMC`*` `*(g/g)", "'Leaf'*' '*'Area'*' '*(cm^2)", "'Leaf'*' '*'Thickness'*' '*(mm)", "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')", "'Dry'*' '*'Mass'*' '*'(g)'", "'Plant'*' '*'Height'*' '*'(cm)'"))) %>% 
  mutate(Trait = factor(Trait, levels = c("'Plant'*' '*'Height'*' '*'(cm)'", "`SLA`*` `*(cm^2/g)", "'Dry'*' '*'Mass'*' '*'(g)'","'Leaf'*' '*'Area'*' '*(cm^2)",  "'Leaf'*' '*'Thickness'*' '*(mm)", "`LDMC`*` `*(g/g)",  "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')" ))) %>%
  filter(Year == 2015) %>% 
  ggplot() +
  geom_boxplot(aes(x = Site, y = itv_diff, fill = Treatment)) +
  geom_hline(aes(yintercept = 0)) +
  geom_blank(aes(x = Site, y = itv_diff + itv_diff*0.6)) + 
  scale_fill_manual(values = c("darkgray", "red")) +
  theme_classic() +
  theme(text = element_text(size = 15),
        strip.background = element_blank(),
        legend.position = "top") +
  facet_wrap(~Trait, scales = "free_y", labeller = label_parsed) +
  geom_text(aes(label = Sig, y = Inf, x = Site, group = Treatment, vjust = 1), position = position_dodge(0.75), data = t_test_itv, size = 4.5) +
  xlab("Habitat Type") +
  ylab("Mean Trait Value - Mean Trait Value (no ITV)")

tiff("plots/itv_plot.tiff", width = 8.75, height = 6, units = "in", res = 400)
itv_plot
dev.off()


#### plot 4: turnover vs intraspecific variation ####
source("ITEX_analyses/inter_intra_anova.R")

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
  mutate(trait = plyr::mapvalues(trait, from = c("SLA_cm2_g", "LDMC", "Leaf_Area_cm2", "Leaf_Thickness_Ave_mm", "N_percent", "C_percent", "P_Ave", "CN_ratio", "dC13_percent", "dN15_percent", "Dry_Mass_g", "Plant_Height_cm"), to = c("`SLA`*` `*(cm^2/g)", "`LDMC`*` `*(g/g)", "'Leaf'*' '*'Area'*' '*(cm^2)", "'Leaf'*' '*'Thickness'*' '*(mm)", "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')", "'Dry'*' '*'Mass'*' '*'(g)'", "'Plant'*' '*'Height'*' '*'(cm)'"))) %>% 
  mutate(trait = factor(trait, levels = c("'Plant'*' '*'Height'*' '*'(cm)'", "`SLA`*` `*(cm^2/g)", "'Dry'*' '*'Mass'*' '*'(g)'","'Leaf'*' '*'Area'*' '*(cm^2)",  "'Leaf'*' '*'Thickness'*' '*(mm)", "`LDMC`*` `*(g/g)",  "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')" )))

varpart_graph <- var_split_exp %>% 
  mutate(level = trimws(level)) %>% 
  filter(RelSumSq.Turnover < 999) %>% 
  rename(Turnover = RelSumSq.Turnover, Intraspecific = RelSumSq.Intraspec., Covariation = RelSumSq.Covariation, Total = RelSumSq.Total) %>% 
  mutate(level = plyr::mapvalues(level, from = c("Site", "Site:Treatment"), to = c("Habitat", "Habitat:Treatment"))) %>% 
  gather(key = variable, value = value, -trait, -level) %>% 
  filter(variable != "Covariation", level != "Total", variable != "Total") %>% 
  mutate(level = factor(level, levels = c("Habitat", "Treatment", "Habitat:Treatment", "Residuals"))) %>% 
  mutate(level = plyr::mapvalues(level, from = c("Habitat", "Treatment", "Habitat:Treatment", "Residuals"), to = c("H", "T", "HxT", "Resid"))) %>% 
  mutate(trait = plyr::mapvalues(trait, from = c("SLA_cm2_g", "LDMC", "Leaf_Area_cm2", "Leaf_Thickness_Ave_mm", "N_percent", "C_percent", "P_Ave", "CN_ratio", "dC13_percent", "dN15_percent", "Dry_Mass_g", "Plant_Height_cm"), to = c("`SLA`*` `*(cm^2/g)", "`LDMC`*` `*(g/g)", "'Leaf'*' '*'Area'*' '*(cm^2)", "'Leaf'*' '*'Thickness'*' '*(mm)", "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')", "'Dry'*' '*'Mass'*' '*'(g)'", "'Plant'*' '*'Height'*' '*'(cm)'"))) %>% 
  mutate(trait = factor(trait, levels = c("'Plant'*' '*'Height'*' '*'(cm)'", "`SLA`*` `*(cm^2/g)", "'Dry'*' '*'Mass'*' '*'(g)'","'Leaf'*' '*'Area'*' '*(cm^2)",  "'Leaf'*' '*'Thickness'*' '*(mm)", "`LDMC`*` `*(g/g)",  "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')" ))) %>% 
  ggplot() +
  geom_bar(aes(x = level, y = value, fill = variable), stat = "identity") +
  geom_point(aes(x = level, y  = value), data = var_split, size = 1) +
    facet_wrap(~trait, nrow = 3, labeller = label_parsed) +
  theme_classic() +
  theme(text = element_text(size = 15), legend.position = "top",
        strip.background = element_blank()) +
  xlab(NULL) +
  ylab("Proportion Variation Explained") +
  scale_fill_manual(values = c("blue", "darkorange"), name = "Source of Variation") +
  scale_x_discrete(drop = FALSE)

tiff(file = "plots/varpart_graph.tiff", width = 7.5, height = 9, units = "in", res = 400)
varpart_graph
dev.off()


#### Old code ####
#try doing var_split by site to isolate the treatment effect and look at differences between sites
var_split_site <- traitMean %>%
  group_by(Trait, Site) %>% 
  do(test = trait.flex.anova(~Treatment, mean, mean_noitv, data = .)) 

var_split_site_exp <- data.frame(RelSumSq.Turnover = 1000, RelSumSq.Intraspec. = 1000, RelSumSq.Covariation = 1000, RelSumSq.Total = 1000, trait = "E", site = "G", level = "F")

for(i in 1:nrow(var_split_site)){
  out <- as.data.frame(var_split_site$test[[i]][2])
  out$trait <- as.factor(rep(var_split_site[i,1], 3))
  out$site <- as.factor(rep(var_split_site[i,2], 3))
  out$level <- rownames(out)
  var_split_site_exp <- rbind(var_split_site_exp, out)
}

var_split_site_exp %>% 
  filter(RelSumSq.Turnover < 999) %>% 
  rename(Turnover = RelSumSq.Turnover, Intraspecific = RelSumSq.Intraspec., Covariation = RelSumSq.Covariation, Total = RelSumSq.Total) %>% 
  gather(key = variable, value = value, -trait, -level, -site) %>% 
  filter(variable != "Total", level != "Total") %>% 
  mutate(trait = plyr::mapvalues(trait, from = c("SLA_cm2_g", "LDMC", "Leaf_Area_cm2", "Leaf_Thickness_Ave_mm", "N_percent", "C_percent", "P_Ave", "CN_ratio", "dC13_percent", "dN15_percent", "Dry_Mass_g", "Plant_Height_cm"), to = c("SLA", "LDMC", "'Leaf'*' '*'Area'", "'Leaf'*' '*'Thickness'", "'%'*'N'", "'%'*'C'", "'%'*'P'", "'C'*':'*'N'", "paste(delta^13, 'C')", "paste(delta^15, 'N')", "'Dry'*' '*'Mass'*' '*'(g)'", "'Plant'*' '*'Height'*' '*'(cm)'"))) %>% 
  mutate(trait = factor(trait, levels = c("SLA", "LDMC", "'Leaf'*' '*'Area'", "'Leaf'*' '*'Thickness'", "'%'*'N'", "'%'*'C'", "'%'*'P'", "'C'*':'*'N'", "paste(delta^13, 'C')", "paste(delta^15, 'N')", "'Dry'*' '*'Mass'*' '*'(g)'", "'Plant'*' '*'Height'*' '*'(cm)'"))) %>% 
  ggplot(aes(x = level, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  facet_grid(trait~site, labeller = label_parsed) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90), text = element_text(size = 15), legend.position = "right") +
  xlab(NULL) +
  ylab("Proportion Variation Explained") +
  scale_fill_manual(values = c("blue", "darkorange","forestgreen"), name = "Source of Variation")
