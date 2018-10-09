#Note, you need to run much of the ImportITEX.R script and CommunityAnalysis.R script to get several of the important input dataframes

#### Create community response dataframe ####
load("community/cleaned_data/CommResp.Rdata")

#calculate distances between plot community metrics from 2003 to 2015
metric_plot_dist <- CommResp %>% 
  #filter(Year != 2009) %>% 
  gather(key = response, value = value, -Year, -Site, -Treatment, -PlotID) %>% 
  group_by(Treatment, PlotID, Site, response) %>% 
  mutate(dist = value - lag(value))%>% 
  left_join(soil_moisture2003, by = "PlotID") %>% 
  left_join(soil_moisture2004, by = "PlotID") %>% 
  filter(response == "Richness" | response == "Diversity" | response == "Evenness" | response == "sumAbundance" | response == "totalGraminoid" | response == "totalForb" | response == "totalShrub" | response == "propLichen" | response == "propBryo")

metric_plot_dist %>% 
  filter(Year != 2003) %>% 
  ggplot(aes(x = as.factor(Year), y = dist, color = Treatment.x, group = PlotID)) +
  geom_abline(slope = 0,intercept = 0) +
  geom_point() +
  geom_path() +
  scale_color_manual(values = c("black", "red")) +
  facet_grid(response ~ Site.x, scales = "free") +
  ylab("Change in Metric") +
  xlab("Year")

metric_plot_dist %>% 
  ggplot(aes(x = soil_moist, y = dist, color = Treatment.x)) +
  geom_abline(slope = 0,intercept = 0) +
  geom_point() +
  stat_smooth(method = "lm") +
  scale_color_manual(values = c("black", "red")) +
  facet_wrap(~response, scales = "free")

metric_plot_dist %>% 
  ggplot(aes(x = soil_moist, y = soil_moist2003)) +
  geom_point()


#### Add community ordination data ####
load("community/cleaned_data/fNMDS.Rdata")

CommunityOrdination <- ggplot(fNMDS, aes(x = NMDS1, y = NMDS2, group = PlotID, shape = Treatment, linetype = Treatment)) +
  geom_point(aes(size = ifelse(Year == min(as.numeric(Year)), "First", "Other"))) +
  geom_path() + 
  coord_equal() +
  scale_size_discrete(name = "Year", range = c(1.2, 2.5), limits = c("Other", "First"), breaks = c("First", "Other")) +
  scale_shape_manual(values = c(1, 16)) + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  labs(x = "NMDS axis 1", y = "NMDS axis 2") +
  facet_grid(~ Site) +
  theme_bw()

#calcuate distances between plot NMDS locations from 2003 to 2015
load("climate/data/soil_moisture2003.rdata")
load("climate/data/soil_moisture2004.rdata")
comm_plot_dist <- fNMDS %>%
  #filter(Year != 2009) %>% 
  group_by(Treatment, PlotID, Site) %>% 
  mutate(diff1 = NMDS1 - lag(NMDS1), diff2 = NMDS2 - lag(NMDS2)) %>% 
  mutate(dist = sqrt(diff1^2 + diff2^2)) %>% 
  left_join(soil_moisture2003, by = "PlotID") %>% 
  left_join(soil_moisture2004, by = "PlotID")

comm_plot_dist %>% filter(Year != 2003) %>% 
  ggplot(aes(x = as.factor(Year), y = dist, color = Treatment.x, group = PlotID)) +
  geom_abline(slope = 0,intercept = 0) +
  geom_point() +
  geom_path() +
  scale_color_manual(values = c("black", "red")) +
  facet_wrap(~Site.x) +
  ylab("Euclidian Distance") +
  xlab("Year")

comm_plot_dist %>% 
  ggplot(aes(x = soil_moist, y = dist, color = Site.x)) +
  geom_abline(slope = 0,intercept = 0) +
  geom_point(size = 4)

#### Community mean trait ordination ####
trait_ord <- traitMean %>% 
  spread(key = trait, value = mean)

trait_pca <- prcomp(trait_ord[,c(4:8)], center = T, scale. = T)
trait_pca_results <- cbind(trait_ord, scores(trait_pca))

trait_pca_results <- trait_pca_results %>% 
  group_by(PlotID, Site) %>% 
  mutate(diff1 = PC1 - lag(PC1), diff2 = PC2 - lag(PC2)) %>% 
  mutate(dist = sqrt(diff1^2 + diff2^2)) %>% 
  left_join(soil_moisture2003, by = "PlotID") %>% 
  left_join(soil_moisture2004, by = "PlotID") %>% 
  left_join(itex_treat)

trait_pca_results %>% filter(Year != 2003) %>% 
  ggplot(aes(x = as.factor(Year), y = dist, color = Treatment, group = PlotID)) +
  geom_abline(slope = 0,intercept = 0) +
  geom_point() +
  geom_path() +
  scale_color_manual(values = c("black", "red")) +
  facet_wrap(~Site.x) +
  ylab("Euclidian distance \n(trait space)") +
  xlab("Year")

TraitOrdination <- ggplot(trait_pca_results, aes(x = PC1, y = PC2, group = PlotID, shape = Treatment, linetype = Treatment)) +
  geom_point(aes(size = ifelse(Year == min(as.numeric(Year)), "First", "Other"))) +
  geom_path() + 
  coord_equal() +
  scale_size_discrete(name = "Year", range = c(1.2, 2.5), limits = c("Other", "First"), breaks = c("First", "Other")) +
  scale_shape_manual(values = c(1, 16)) + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  labs(x = "NMDS axis 1", y = "NMDS axis 2") +
  facet_grid(~ Site.x) +
  theme_bw()

#### CWM traits ####
traits_mean_ctl <- traitsITEX_SV_2018 %>% 
  select(Treatment, Taxon, Plant_Height_cm, Dry_Mass_g, Leaf_Area_cm2, Leaf_Thickness_Ave_mm, SLA_cm2_g, LDMC) %>% 
  gather(key = trait, value = value, -Treatment, -Taxon) %>% 
  filter(Treatment == "CTL") %>% 
  group_by(Taxon, trait) %>% 
  summarize(mean_trait = mean(value, na.rm = T))

sum_abundance <- CommunitySV_ITEX_2003_2015 %>% filter(Taxon != "equisetum scirpoides", Taxon != "equisetum arvense") %>% 
  group_by(PlotID, Year) %>% 
  summarize(sum_abundance = sum(Abundance, na.rm = T))

cwm_itex_ctl <- CommunitySV_ITEX_2003_2015 %>% filter(Taxon != "equisetum scirpoides", Taxon != "equisetum arvense") %>%
  left_join(sum_abundance) %>% 
  mutate(rel_abundance = Abundance/sum_abundance) %>% 
  left_join(traits_mean_ctl) %>% 
  group_by(PlotID, Year, trait) %>% 
  summarize(cwm_ctl = weighted.mean(mean_trait, rel_abundance, na.rm = T)) %>% 
  left_join(itex_treat) %>% 
  filter(!duplicated(cwm_ctl)) %>% 
  mutate(Site = substr(PlotID, 1, 3))

cwm_itex_ctl %>% filter(!is.na(trait)) %>% 
  ggplot(aes(x = as.factor(Year), y = cwm_ctl, fill = Treatment)) +
  geom_boxplot() +
  facet_grid(trait ~ Site, scales = "free") +
  scale_fill_manual(values = c("grey", "red"))

cwm_itex_ctl_dist <- cwm_itex_ctl %>%
  group_by(PlotID, trait) %>% 
  mutate(dist = cwm_ctl - lag(cwm_ctl))

cwm_itex <- cwm_itex_ctl %>%
  left_join(traitMean) %>% 
  rename(cwm_itv = mean) %>% 
  left_join(traitMean_noitv) %>% 
  rename(cwm_noitv = mean)

cas_abundance <- CommunitySV_ITEX_2003_2015 %>% 
  left_join(sum_abundance) %>% 
  mutate(rel_abundance = Abundance/sum_abundance) %>% 
  filter(Taxon == "cassiope tetragona") %>% 
  select(rel_abundance, PlotID, Year)

cwm_itex %>% filter(!is.na(trait)) %>% #left_join(cas_abundance) %>% 
  ggplot(aes(x = cwm_ctl, y = cwm_noitv, color = Site)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~trait, scales = "free")

traitsITEX_SV_2018 %>% filter(Site == "CAS") %>% 
  ggplot(aes(x = Dry_Mass_g, y = Leaf_Area_cm2, color = Taxon)) +
  geom_point(size = 2)

equisetum <- CommunitySV_ITEX_2003_2015 %>% 
  left_join(sum_abundance) %>% 
  mutate(rel_abundance = Abundance/sum_abundance) %>% 
  filter(Taxon == "equisetum scirpoides" | Taxon == "equisetum arvense")
