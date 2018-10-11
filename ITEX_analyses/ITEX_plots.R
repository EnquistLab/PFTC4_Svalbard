#Note, you need to run much of the ImportITEX.R script and CommunityAnalysis.R script to get several of the important input dataframes

#### Create community response dataframe ####
load("community/cleaned_data/CommResp.Rdata")
load(file = "community/cleaned_data/CommunitySV_ITEX_2003_2015.Rdata", verbose = TRUE)


metaItex <- CommunitySV_ITEX_2003_2015 %>% 
  distinct(Site, Treatment, PlotID)

#calculate distances between plot community metrics from 2003 to 2015
metric_plot_dist <- CommResp %>% 
  filter(Year != 2009) %>% 
  gather(key = response, value = value, -Year, -Site, -Treatment, -PlotID) %>% 
  group_by(Treatment, PlotID, Site, response) %>% 
  summarize(dist = diff(value))%>% 
  left_join(soil_moisture2003, by = "PlotID") %>% 
  left_join(soil_moisture2004, by = "PlotID") %>% 
  filter(response == "Richness" | response == "Diversity" | response == "Evenness" | response == "sumAbundance" | response == "totalGraminoid" | response == "totalForb" | response == "totalShrub" | response == "propLichen" | response == "propBryo") %>% 
  left_join(soil_moisture2004, by = "PlotID")

#Plot of change from beginning to end in community metrics by habitat type
metric_plot_dist %>% 
  #filter(Year != 2003) %>% 
  ggplot(aes(x = Treatment.x, y = dist, fill = Treatment.x)) +
  geom_abline(slope = 0,intercept = 0) +
  geom_boxplot() +
  scale_fill_manual(values = c("gray", "red")) +
  facet_wrap(~response, scales = "free") +
  ylab("Change in Metric") +
  xlab("Habitat Type") +
  theme(text = element_text(size = 20)) 

metric_plot_dist %>% 
  #filter(Year != 2003) %>% 
  ggplot(aes(x = Site.x, y = dist, fill = Treatment.x)) +
  geom_abline(slope = 0,intercept = 0) +
  geom_boxplot() +
  scale_fill_manual(values = c("gray", "red")) +
  facet_wrap(~response, scales = "free") +
  ylab("Change in Metric") +
  xlab("Habitat Type") +
  theme(text = element_text(size = 20)) 

metric_plot_dist %>% 
  #filter(Year != 2003) %>% 
  ggplot(aes(x = soil_moist2004.x, y = dist, color = Treatment.x)) +
  geom_abline(slope = 0,intercept = 0) +
  geom_point() +
  scale_color_manual(values = c("black", "red")) +
  facet_wrap(~response, scales = "free") +
  ylab("Change in Metric") +
  xlab("Moisture") +
  theme(text = element_text(size = 20)) 

#plot of change from beginning to end in community metrics by soil moisture
metric_plot_dist %>% 
  ggplot(aes(x = soil_moist, y = dist, color = Treatment.x)) +
  geom_abline(slope = 0,intercept = 0) +
  geom_point() +
  stat_smooth(method = "lm") +
  scale_color_manual(values = c("black", "red")) +
  facet_wrap(~response, scales = "free") +
  ylab("Change in Metric") +
  xlab("Soil Moisture") +
  theme(text = element_text(size = 20)) +
  scale_fill_manual(values = c("grey", "red"))


#### Add community ordination data ####
load("community/cleaned_data/fNMDS.Rdata")

fNMDS %>%
  filter(Site == "CAS") %>% 
  filter(Year != 2009) %>% 
  group_by(PlotID) %>% 
  summarize(diff1 = diff(NMDS1), diff2 = diff(NMDS2)) %>% 
  mutate(dist = sqrt(diff1^2 + diff2^2))

##CAS-5, CAS-10, CAS-9 do most extreme responses

#Auds NMDS plot
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
  filter(Year != 2009) %>% 
  group_by(Treatment, PlotID, Site) %>% 
  summarize(diff1 = diff(NMDS1), diff2 = diff(NMDS2)) %>% 
  mutate(dist = sqrt(diff1^2 + diff2^2)) %>% 
  left_join(soil_moisture2003, by = "PlotID") %>% 
  left_join(soil_moisture2004, by = "PlotID")

#plot of change in multivariate position based on community composition
comm_plot_dist %>% 
  ggplot(aes(x = Treatment.x, y = dist)) +
  geom_abline(slope = 0,intercept = 0) +
  geom_boxplot() +
  scale_fill_manual(values = c("gray", "red")) +
  ylab("Euclidian Distance") +
  xlab("Habitat Type") +
  theme(text = element_text(size = 20))

comm_plot_dist %>% 
  ggplot(aes(x = soil_moist, y = dist, color = Treatment.x)) +
  geom_abline(slope = 0,intercept = 0) +
  geom_point(size = 4)

#### Community mean trait ordination ####
traitMean <- readRDS(file = "traits/cleaned_data/community_weighted_means.RDS") %>% 
  as.tibble() %>% 
  mutate(mean = as.numeric(as.character(mean))) %>% 
  mutate(Year = as.numeric(as.character(Year)))

traitMean_noitv <- readRDS(file = "traits/cleaned_data/community_weighted_means_no_itv.RDS") %>% 
  as.tibble() %>% 
  mutate(mean = as.numeric(as.character(mean))) %>% 
  mutate(Year = as.numeric(as.character(Year)))

trait_ord <- traitMean %>% 
  spread(key = trait, value = mean)

trait_pca <- prcomp(trait_ord[,c(4:8)], center = T, scale. = T)
trait_pca_results <- cbind(trait_ord, scores(trait_pca)) %>% left_join(metaItex)

trait_pca_results1 <- trait_pca_results %>% 
  filter(Year != 2009) %>% 
  group_by(PlotID, Site) %>% 
  summarize(diff1 = diff(PC1), diff2 = diff(PC2)) %>% 
  mutate(dist = sqrt(diff1^2 + diff2^2)) %>% 
  left_join(soil_moisture2003, by = "PlotID") %>% 
  left_join(soil_moisture2004, by = "PlotID") %>% 
  left_join(metaItex)%>% 
  left_join(soil_moisture2004, by = "PlotID")

trait_pca_results1 %>% 
  ggplot(aes(x = soil_moist2004.x, y = dist, color = Treatment)) +
  geom_abline(slope = 0,intercept = 0) +
  geom_point() +
  scale_fill_manual(values = c("gray", "red")) +
  ylab("Euclidian distance \n(trait space)") +
  xlab("Habitat Type") +
  theme(text = element_text(size = 20))

TraitOrdination <- ggplot(trait_pca_results, aes(x = PC1, y = PC2, group = PlotID, shape = Treatment, linetype = Treatment)) +
  geom_point(aes(size = ifelse(Year == min(as.numeric(Year)), "First", "Other"))) +
  geom_path() + 
  coord_equal() +
  scale_size_discrete(name = "Year", range = c(1.2, 2.5), limits = c("Other", "First"), breaks = c("First", "Other")) +
  scale_shape_manual(values = c(1, 16)) + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  labs(x = "PC1", y = "PC2") +
  facet_wrap(~ Site) +
  theme_bw()

library(ggpubr)
ggarrange(CommunityOrdination, TraitOrdination, nrow = 2, common.legend = T)

#### Calculate vegetation and trait multivariate distances
comm_distances <- CommunitySV_ITEX_2003_2015 %>% 
  select(-Spp, -FunctionalGroup) %>% 
  filter(Year != 2009) %>% 
  spread(key = Taxon, value = Abundance, fill = 0) %>% 
  group_by(PlotID) %>% 
  do( data_frame(out = as.vector(vegdist(select(., -(Site:Year)), method = "bray")))) %>% 
  left_join(metaItex)

comm_dist <- comm_distances %>% 
  ggplot(aes(x = Site, y = out, fill = Treatment)) +
  geom_boxplot() +
  scale_fill_manual(values = c("gray", "red")) +
  ylab("Bray Curtis Distance") +
  xlab("Habitat Type") +
  ggtitle("Community Change") +
  theme(text = element_text(size = 20))

comm_test <- lm(out ~ Treatment, data = comm_distances)

# trait distances
trait_distances <- traitMean %>% 
  filter(Year != 2009) %>% 
  filter(trait != "Dry_Mass_Total_g", trait != "Wet_Mass_Total_g", trait != "wetSLA_cm2_per_g", trait != "Wet_mass_g") %>% 
  spread(key = trait, value = mean) %>% 
  group_by(PlotID) %>% 
  do( data_frame(out = as.vector(dist(select(., -(Site:Year)), method = "euclidian")))) %>% 
  left_join(metaItex)

trait_dist <- trait_distances %>% 
  ggplot(aes(x = Site, y = out, fill = Treatment)) +
  geom_boxplot() +
  scale_fill_manual(values = c("gray", "red")) +
  ylab("Euclidian Distance") +
  xlab("Habitat Type") +
  ggtitle("Trait Change") +
  theme(text = element_text(size = 20))

ggarrange(comm_dist, trait_dist, ncol = 2, common.legend = T)

trait_test <- lm(out ~ Treatment * Site, data = trait_distances)


#### CWM traits ####
load("traits/data/traitsITEX_SV_2018.Rdata")

traits_mean_ctl <- traitsITEX_SV_2018 %>% 
  select(Treatment, Taxon, Plant_Height_cm, Dry_Mass_g, Leaf_Area_cm2, Leaf_Thickness_Ave_mm, SLA_cm2_g, LDMC) %>% 
  gather(key = trait, value = value, -Treatment, -Taxon) %>% 
  filter(Treatment == "CTL") %>% 
  group_by(Taxon, trait) %>% 
  summarize(mean_trait = mean(value, na.rm = T))

#sum_abundance <- CommunitySV_ITEX_2003_2015 %>% filter(Taxon != "equisetum scirpoides", Taxon != "equisetum arvense") %>% 
#  group_by(PlotID, Year) %>% 
#  summarize(sum_abundance = sum(Abundance, na.rm = T))

cwm_itex_ctl <- CommunitySV_ITEX_2003_2015 %>% filter(Taxon != "equisetum scirpoides", Taxon != "equisetum arvense") %>%
  #left_join(sum_abundance) %>% 
  #mutate(rel_abundance = Abundance/sum_abundance) %>% 
  left_join(traits_mean_ctl) %>% 
  group_by(PlotID, Year, trait) %>% 
  summarize(cwm_ctl = weighted.mean(mean_trait, Abundance, na.rm = T)) %>% 
  left_join(metaItex, by = "PlotID") %>% 
  mutate(Site = substr(PlotID, 1, 3))

cwm_itex_ctl %>% filter(!is.na(trait)) %>% 
  ggplot(aes(x = as.factor(Year), y = cwm_ctl, fill = Treatment)) +
  geom_boxplot() +
  facet_grid(trait ~ Site, scales = "free") +
  scale_fill_manual(values = c("grey", "red"))

cwm_itex_ctl_dist <- cwm_itex_ctl %>%
  filter(Year != 2009) %>% 
  group_by(PlotID, trait) %>% 
  mutate(dist = cwm_ctl - lag(cwm_ctl))

# plot of change mean community functional traits
#NOTE: this is based on CWM without ITV right now!
cwm_itex_ctl_dist %>% filter(!is.na(trait)) %>% 
  ggplot(aes(x = Site, y = dist, fill = Treatment)) +
  geom_boxplot() +
  geom_abline(slope = 0, intercept = 0) +
  scale_fill_manual(values = c("gray", "red")) +
  ylab("Change in community mean trait") +
  xlab("Habitat Type") +
  theme(text = element_text(size = 20)) +
  facet_wrap(~trait, scales = "free")




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
  ggplot(aes(x = cwm_itv, y = cwm_noitv, color = Site)) +
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


#### Effect of ITV on mean community trait values ####
traitMean <- readRDS(file = "traits/cleaned_data/community_weighted_means.RDS") %>% 
  as.tibble() %>% 
  mutate(mean = as.numeric(as.character(mean))) %>% 
  mutate(Year = as.numeric(as.character(Year)))

traitMean_noitv <- readRDS(file = "traits/cleaned_data/community_weighted_means_no_itv.RDS") %>% 
  as.tibble() %>% 
  mutate(mean = as.numeric(as.character(mean)))%>% 
  mutate(Year = as.numeric(as.character(Year))) %>% 
  rename("mean_noitv" = "mean")

traitMean <- traitMean %>% 
  left_join(traitMean_noitv) %>% 
  mutate(diff = mean - mean_noitv) %>% 
  left_join(metaItex) %>% 
  mutate(prop_diff = diff/mean) %>% 
  gather(key = key, value = value, -Site, -Year, -PlotID, -trait, -Treatment) 

traitMean %>% filter(Year == 2015) %>% filter(trait == "Dry_Mass_g" | trait == "LDMC" | trait == "Leaf_Area_cm2" | trait == "Leaf_Thickness_Ave_mm" | trait == "Plant_Height_cm" | trait == "SLA_cm2_g") %>% filter(key != "diff", key != "prop_diff") %>% 
  filter(PlotID != "CAS-4", PlotID != "CAS-9", PlotID != "CAS-10", PlotID != "CAS-6") %>% 
  ggplot(aes(x = Site, y = value, fill = paste(Treatment, key))) +
  geom_boxplot() +
  geom_abline(slope = 0, intercept = 0) +
  scale_fill_manual(values = c("lightgray", "darkgray", "red", "darkred")) +
  ylab("CWM Trait Value") +
  xlab("Habitat Type") +
  theme(text = element_text(size = 20)) +
  facet_wrap(~trait, scales = "free")

library(cati)

test <- traitMean %>% filter(trait == "Leaf_Area_cm2") %>% filter(Year == 2015)

test <- traitMean %>% select(-mean_noitv, -diff, -prop_diff) %>% spread(key = trait, value = mean) %>% filter(Year == 2015)
  
aov_test <- trait.flex.anova(~Treatment*Site, mean, mean_noitv, data = test)
aov_test
plot(aov_test, use.percentage = T, legend.pos="none", main = "LeafArea")


decompCTRE(test, formula = ~Treatment*Site)

traitMean %>% filter(trait == "Plant_Height_cm", Year == "2015") %>% 
  ggplot(aes(x = Site, y = mean, fill = Treatment)) +
  geom_boxplot()



test <- traitsITEX_SV_2018 %>% 
  select(Treatment, Taxon, Plant_Height_cm, Dry_Mass_g, Leaf_Area_cm2, Leaf_Thickness_Ave_mm, SLA_cm2_g, LDMC) %>% 
  gather(key = trait, value = value, -Treatment, -Taxon) %>% 
  filter(Treatment == "CTL") %>% 
  filter(trait == "Leaf_Thickness_Ave_mm")


varpartLA <- varcomp(lme(value~1, random=~1|Taxon, data=test, na.action = na.omit), 1)
varpartSLA <- varcomp(lme(value~1, random=~1|Taxon, data=test, na.action = na.omit), 1)
varpartLDMC <- varcomp(lme(value~1, random=~1|Taxon, data=test, na.action = na.omit), 1)
varpartdm <- varcomp(lme(value~1, random=~1|Taxon, data=test, na.action = na.omit), 1)
varpartLT <- varcomp(lme(value~1, random=~1|Taxon, data=test, na.action = na.omit), 1)
varpartPH <- varcomp(lme(value~1, random=~1|Taxon, data=test, na.action = na.omit), 1)
