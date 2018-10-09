# LOAD DATA AND LIBRARIES

library("tidyverse")
library("vegan")
library("ggvegan")
library("broom")
library("lme4")

source("ImportITEX.R")


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
            propForb = sum(Abundance[FunctionalGroup %in% c("forbsv")])/sumAbundance,
            propShrub = sum(Abundance[FunctionalGroup %in% c("eshrub", "dshrub")])/sumAbundance,
            propEShrub = sum(Abundance[FunctionalGroup %in% c("eshrub")])/sumAbundance,
            propDShrub = sum(Abundance[FunctionalGroup %in% c("dshrub")])/sumAbundance,
            propLichen = sum(Abundance[FunctionalGroup %in% c("lichen")])/sumAbundance,
            propBryo = sum(Abundance[FunctionalGroup %in% c("moss", "liverwort")])/sumAbundance,
            totalVascular = sum(Abundance[FunctionalGroup %in% c("graminoid", "forbsv", "eshrub", "dshrub")]),
            totalGraminoid = sum(Abundance[FunctionalGroup %in% c("graminoid")]),
            totalForb = sum(Abundance[FunctionalGroup %in% c("forbsv")]),
            totalShrub = sum(Abundance[FunctionalGroup %in% c("eshrub", "dshrub")])
  )

CommResp %>% 
  gather(key = response, value = value, -Year, -Site, -Treatment, -PlotID) %>% 
  ggplot(aes(x = as.factor(Year), y = value, fill = Treatment)) +
  geom_boxplot() +
  scale_fill_manual(values = c("grey", "red")) +
  facet_grid(response ~ Site, scales = "free")


ModelRes <- CommResp %>% 
  mutate(nAbundance = sumAbundance) %>% 
  gather(key = response, value = value, -Year, -Site, -Treatment, -PlotID, -n, -nAbundance) %>% 
  filter(response %in% c("Richness", "totalVascular")) %>% 
  group_by(Site, response) %>% 
  do(fit.lmer5 = lmer(value ~ as.factor(Year) * Treatment + (1|PlotID), data = .),
     fit.lmer4 = lmer(value ~ as.factor(Year) + Treatment + (1|PlotID), data = .),
     fit.lmer3 = lmer(value ~ as.factor(Year) + (1|PlotID), data = .),
     fit.lmer2 = lmer(value ~ Treatment + (1|PlotID), data = .),
     fit.lmer1 = lmer(value ~ 1 + (1|PlotID), data = .))

tidy(ModelRes, fit.lmer5)

x <- CommResp %>% 
  mutate(nAbundance = sumAbundance) %>% 
  gather(key = response, value = value, -Year, -Site, -Treatment, -PlotID, -n, -nAbundance) %>% 
  filter(response %in% c("Evenness"))

fit.lmer5 = lmer(value ~ as.factor(Year) * Treatment + (1|PlotID), data = x)
fit.lmer4 = lmer(value ~ as.factor(Year) + Treatment + (1|PlotID), data = x)
fit.lmer3 = lmer(value ~ as.factor(Year) + (1|PlotID), data = x)
fit.lmer2 = lmer(value ~ Treatment + (1|PlotID), data = x)
fit.lmer1 = lmer(value ~ 1 + (1|PlotID), data = x)
modsel(list(fit.lmer1, fit.lmer2, fit.lmer3, fit.lmer4, fit.lmer5), 1000)


#function for QAICc. NB, phi is the scaling parameter from the quasi-family model. If using e.g. a poisson family, phi=1 and QAICc returns AICc, or AIC if QAICc=FALSE.
QAICc <- function(mod, scale, QAICc = TRUE) {
  ll <- as.numeric(logLik(mod))
  df <- attr(logLik(mod), "df")
  n <- length(resid(mod))
  if (QAICc)
    qaic = as.numeric(-2 * ll/scale + 2 * df + 2 * df * (df + 1)/(n - df - 1))
  else qaic = as.numeric(-2 * ll/scale + 2 * df)
  qaic
}


## code for model selection. First fit mod01, then run this code.
modsel <- function(mods,x){	
  phi=1
  dd <- data.frame(Model=1:length(mods), K=1, QAIC=1)
  for(j in 1:length(mods)){
    dd$K[j] = attr(logLik(mods[[j]]),"df")
    dd$QAIC[j] = QAICc(mods[[j]],phi)
  }
  dd$delta.i <- dd$QAIC - min(dd$QAIC)
  dd <- subset(dd,dd$delta.i<x)
  dd$re.lik <- round(exp(-0.5*dd$delta.i),3)
  sum.aic <- sum(exp(-0.5*dd$delta.i))
  wi <- numeric(0)
  for (i in 1:length(dd$Model)){wi[i] <- round(exp(-0.5*dd$delta.i[i])/sum.aic,3)}; dd$wi<-wi
  print(dds <- dd[order(dd$QAIC), ])
  assign("mstable",dd,envir=.GlobalEnv)
}


fit.glmer = lmer(value ~ as.factor(Year) * Treatment + (1|PlotID), data = ., family = "binomial", weights = nAbundance)
tidy(ModelRes, fit.glm) %>% 
  filter(response %in% c("Evenness", "propGraminoid", "propLichen", "propBryo")) %>% 
  filter(p.value < 0.05) %>% pn

#### MAKE ORDINATION ####
set.seed(32)

# BISTORTA
comm_fat_BIS <- CommunitySV_ITEX_2003_2015 %>% 
  select(-Taxon, -FunctionalGroup) %>% 
  arrange(Year) %>% 
  spread(key = Spp, value = Abundance, fill = 0) %>% 
  filter(Site == "BIS")

comm_fat_spp_BIS <- comm_fat_BIS %>% select(-(Site:Year))

NMDS_BIS <- metaMDS(comm_fat_spp_BIS, noshare = TRUE, try = 30)

fNMDS_BIS <- fortify(NMDS_BIS) %>% 
  filter(Score == "sites") %>% 
  bind_cols(comm_fat_BIS %>% select(Site:Year))

# CASSIOPE
comm_fat_CAS <- CommunitySV_ITEX_2003_2015 %>% 
  select(-Taxon, -FunctionalGroup) %>% 
  arrange(Year) %>% 
  spread(key = Spp, value = Abundance, fill = 0) %>% 
  filter(Site == "CAS")

comm_fat_spp_CAS <- comm_fat_CAS %>% select(-(Site:Year))

NMDS_CAS <- metaMDS(comm_fat_spp_CAS, noshare = TRUE, try = 100)

fNMDS_CAS <- fortify(NMDS_CAS) %>% 
  filter(Score == "sites") %>% 
  bind_cols(comm_fat_CAS %>% select(Site:Year))


# DRYAS
comm_fat_DRY <- CommunitySV_ITEX_2003_2015 %>% 
  select(-Taxon, -FunctionalGroup) %>% 
  arrange(Year) %>% 
  spread(key = Spp, value = Abundance, fill = 0) %>% 
  filter(Site == "DRY")

comm_fat_spp_DRY <- comm_fat_DRY %>% select(-(Site:Year))

NMDS_DRY <- metaMDS(comm_fat_spp_DRY, noshare = TRUE, try = 100)

fNMDS <- fortify(NMDS_DRY) %>% 
  filter(Score == "sites") %>% 
  bind_cols(comm_fat_DRY %>% select(Site:Year)) %>% 
  bind_rows(fNMDS_BIS, fNMDS_CAS)

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


comm_plot_dist %>% 
  ggplot(aes(x = soil_moist, y = dist, color = Treatment.x, shape = Site.x)) +
  geom_abline(slope = 0,intercept = 0) +
  geom_point(size = 4) +
  scale_color_manual(values = c("black", "red"))


comm_plot_dist %>% 
  ggplot(aes(x = soil_moist, y = dist, color = Site.x)) +
  geom_abline(slope = 0,intercept = 0) +
  geom_point(size = 4)

#calculate distances between plot community metrics from 2003 to 2015
metric_plot_dist <- CommResp %>% 
  filter(Year != 2009) %>% 
  gather(key = response, value = value, -Year, -Site, -Treatment, -PlotID) %>% 
  group_by(Treatment, PlotID, Site, response) %>% 
  summarize(dist = diff(value))%>% 
  left_join(soil_moisture2003, by = "PlotID") %>% 
  left_join(soil_moisture2004, by = "PlotID")

metric_plot_dist %>% 
  ggplot(aes(x = Site.x, y = dist, fill = Treatment.x)) +
  geom_abline(slope = 0,intercept = 0) +
  geom_boxplot() +
  scale_fill_manual(values = c("black", "red")) +
  facet_wrap(~response, scales = "free")

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



#### COMMUNITY WEIGHTED TRAIT MEANS ####
traitMean %>% 
  left_join(metaItex, by = c("Site", "PlotID")) %>% 
  #filter(trait == "wetSLA_cm2_per_g") %>% 
  ggplot(aes(x = Year, y = mean, fill = Treatment)) +
  geom_boxplot() +
  scale_fill_manual(values = c("grey", "red")) +
  labs(y = "Trait mean", x = "") +
  facet_grid(trait ~ Site, scales = "free_y")



#### ORDINATION FOR TRAITS ####
ItexHeight_2015 <- ItexHeight %>% 
  filter(Year == 2015) %>% 
  ungroup() %>% 
  select(-Year)

CommResp_2015 <- CommResp %>% 
  filter(Year == 2015) %>% 
  ungroup() %>% 
  select(-Year)

traitMean_2015 <- traitMean %>% 
  filter(Year == 2015) %>% 
  ungroup() %>% 
  select(-Year)


ITEX_all <- ITEX.fluxes.standard %>% 
  mutate(PlotID = gsub("L", "", PlotID)) %>% 
  left_join(ItexHeight_2015, by = c("ITEX_Site" = "Site", "PlotID", "Treatment")) %>% 
  left_join(CommResp_2015, by = c("ITEX_Site" = "Site", "PlotID", "Treatment")) %>% 
  left_join(traitMean_2015, by = c("ITEX_Site" = "Site", "PlotID")) %>% 
  spread(key = trait, value = mean)

set.seed(32)

ITEX_traits <- ITEX_all %>% 
  select(ITEX_Site , Treatment, PlotID, Area_cm2, mean_thickness_mm, Plant_height_cm, Wet_mass_g, wetSLA_cm2_per_g)

only_traits <- ITEX_traits %>% 
  select(Area_cm2, mean_thickness_mm, Plant_height_cm, Wet_mass_g, wetSLA_cm2_per_g)

pca <- rda(only_traits, scale = TRUE)

screeplot(pca, bstick = TRUE)

fNMDS <- fortify(pca, axes = 1:4) %>% 
  filter(Score == "sites") %>% 
  bind_cols(ITEX_traits)

ggplot(fNMDS, aes(x = PC1, y = PC2, group = PlotID, shape = Treatment, colour = ITEX_Site)) +
  geom_point(size = 3) +
  coord_equal() +
  scale_shape_manual(values = c(1, 16)) + 
  labs(x = "NMDS axis 1", y = "NMDS axis 2") +
  theme_bw()


### ACROSS YEARS
# BIS
ITEX_traits_Years_BIS <- traitMean %>% 
  spread(key = trait, value = mean) %>% 
  left_join(metaItex, by = c("Site", "PlotID")) %>% 
  filter(Site == "BIS")

only_traits_Years_BIS <- ITEX_traits_Years_BIS %>% 
  select(Area_cm2, mean_thickness_mm, Plant_height_cm, Wet_mass_g, wetSLA_cm2_per_g)

pca2_BIS <- rda(only_traits_Years_BIS, scale = TRUE)

screeplot(pca2_BIS, bstick = TRUE)

fNMDS2_BIS <- fortify(pca2_BIS, axes = 1:4) %>% 
  filter(Score == "sites") %>% 
  bind_cols(ITEX_traits_Years_BIS)

# CAS
ITEX_traits_Years_CAS <- traitMean %>% 
  spread(key = trait, value = mean) %>% 
  left_join(metaItex, by = c("Site", "PlotID")) %>% 
  filter(Site == "CAS")

only_traits_Years_CAS <- ITEX_traits_Years_CAS %>% 
  select(Area_cm2, mean_thickness_mm, Plant_height_cm, Wet_mass_g, wetSLA_cm2_per_g)

pca2_CAS <- rda(only_traits_Years_CAS, scale = TRUE)

screeplot(pca2_CAS, bstick = TRUE)

fNMDS2_CAS <- fortify(pca2_CAS, axes = 1:4) %>% 
  filter(Score == "sites") %>% 
  bind_cols(ITEX_traits_Years_CAS)


# DRY
ITEX_traits_Years_DRY <- traitMean %>% 
  spread(key = trait, value = mean) %>% 
  left_join(metaItex, by = c("Site", "PlotID")) %>% 
  filter(Site == "DRY")

only_traits_Years_DRY <- ITEX_traits_Years_DRY %>% 
  select(Area_cm2, mean_thickness_mm, Plant_height_cm, Wet_mass_g, wetSLA_cm2_per_g)

pca2_DRY <- rda(only_traits_Years_DRY, scale = TRUE)

screeplot(pca2_DRY, bstick = TRUE)

fNMDS2 <- fortify(pca2_DRY, axes = 1:4) %>% 
  filter(Score == "sites") %>% 
  bind_cols(ITEX_traits_Years_DRY) %>% 
  bind_rows(fNMDS2_BIS, fNMDS2_CAS) %>% 
  mutate(Year2 = as.numeric((Year)),
         Size = ifelse(Year2 == 1, "First", "Other"))


ggplot(fNMDS2, aes(x = PC1, y = PC2, group = PlotID, shape = Treatment, linetype = Treatment)) +
  geom_point(aes(size = Size)) +
  geom_path() +
  coord_equal() +
  scale_size_discrete(name = "Year", range = c(1.2, 2.5), limits = c("Other", "First"), breaks = c("First", "Other")) +
  scale_shape_manual(values = c(1, 16)) + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  labs(x = "PC1", y = "PC2") +
  facet_grid(~ Site) +
  theme_bw()


rds_DRY <- rda(only_traits_Years_DRY ~ Treatment * Year, data = ITEX_traits_Years_DRY, scale = TRUE)
rds_DRY
autoplot(rds_DRY)
