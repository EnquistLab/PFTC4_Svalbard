#### COMMUNITY DATA ANALYSIS ####
source("ITEX_analyses/2_Import_data.R")


#### CALCULATE COMMUNITY RESPONSES #### 
CommResp <- CommunitySV_ITEX_2003_2015 %>% 
  group_by(Year, Site, Treatment, PlotID) %>%  
  summarise(n = n(),
            Richness = n, 
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
            totalShrub = sum(Abundance[FunctionalGroup %in% c("eshrub", "dshrub")])
  )


metaItex <- CommunitySV_ITEX_2003_2015 %>% 
  distinct(Site, Treatment, PlotID)

#### plot 1: change in community metrics ####

#multivariate community distances
comm_distances <- CommunitySV_ITEX_2003_2015 %>% 
  select(-FunctionalGroup, -Elevation_m, -Latitude_N, -Longitude_E, -Flag) %>% 
  filter(Year != 2009) %>% 
  spread(key = Taxon, value = Abundance, fill = 0) %>% 
  group_by(PlotID) %>% 
  do( data_frame(out = as.vector(vegdist(select(., -(Year:PlotID)), method = "bray")))) %>% 
  left_join(metaItex, by = "PlotID")  %>% 
  mutate(Site = factor(Site, levels = c("SB", "CH", "DH")))

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
  geom_text(aes(label = text, x = 0.5, y = Inf, hjust = 0, vjust = 2), size = 4, color = "black",  data = anova_text) +
  geom_text(aes(label = Sig, x = Site, y = -Inf, hjust = 0.5, vjust = 0, group = Treatment), size = 6, position = position_dodge(0.75),color = "black",  data = t_test)

jpeg("ITEX_analyses/output/Fig_1_metric_change.jpg", width = 8, height = 6, units = "in", res = 400)
metric_change
dev.off()

#### Figure S4 change in other community metrics ####
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

jpeg("ITEX_analyses/output/Fig_S4_metric_change_supp.jpg", width = 8, height = 4, units = "in", res = 400)
metric_change_supp
dev.off()


#### Figure S5. change by year ####

metric_time <- CommResp %>% 
  select(Year, Site, Treatment, Richness, Evenness, totalForb, totalShrub, totalGraminoid, propBryo, propLichen, PlotID) %>%
  rename("Forb\nAbundance" = totalForb, "Shrub\nAbundance" = totalShrub, "Graminoid\nAbundance" = totalGraminoid, "Bryo\nAbundance" = propBryo, "Lichen\nAbundance" = propLichen) %>% 
  gather(key = metric, value = value, -Year, -Site, -Treatment, -PlotID) %>% 
  group_by(metric) %>% mutate(max_val = max(value)) %>% ungroup() %>% 
  mutate(metric = factor(metric, levels = c("Richness", "Evenness", "Forb\nAbundance", "Graminoid\nAbundance", "Shrub\nAbundance", "Bryo\nAbundance", "Lichen\nAbundance"))) %>%
  mutate(Site = factor(Site, levels = c("SB", "CH", "DH"))) %>% 
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

jpeg("ITEX_analyses/output/Fig_S5_metric_time.jpg", width = 5, height = 9, units = "in", res = 400)
metric_time
dev.off()



#### NMDS ORDINATION ####
set.seed(32)

# SNOWBED (SB)
comm_fat_SB <- CommunitySV_ITEX_2003_2015 %>% 
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
comm_fat_CH <- CommunitySV_ITEX_2003_2015 %>% 
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
comm_fat_DH <- CommunitySV_ITEX_2003_2015 %>% 
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

# Make figure
  
 
plot_annotation <- tibble(Site = c("SB", "CH", "DH"),
                   label = c("Year*", "", "Year*"))
CommunityOrdination <- fNMDS %>% 
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

ggsave(CommunityOrdination, filename = "ITEX_analyses/output/Fig_S3_CommunityOrdination.jpeg", width = 8, height = 3.5, dpi = 300)



# Check if community composition changes in treatments and over time
fNMDS %>%
  nest(data = -Site) %>% 
  mutate(
    fit = map(data, ~ lm(NMDS1 ~ Treatment * Year, data = .x)),
    tidied = map(fit, tidy)
  ) %>% 
  unnest(tidied) %>% 
  filter(p.value < 0.05,
         term != "(Intercept)")
