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
            totalVascular = sum(Abundance[FunctionalGroup %in% c("graminoid", "forbsv", "eshrub", "dshrub")])
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

NMDS_CAS <- metaMDS(comm_fat_spp_CAS, noshare = TRUE, try = 30)

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

NMDS_DRY <- metaMDS(comm_fat_spp_DRY, noshare = TRUE, try = 30)

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
ITEX_traits_Years <- traitMean %>% 
  spread(key = trait, value = mean) %>% 
  left_join(metaItex, by = c("Site", "PlotID"))

only_traits_Years <- ITEX_traits_Years %>% 
  select(Area_cm2, mean_thickness_mm, Plant_height_cm, Wet_mass_g, wetSLA_cm2_per_g)

pca2 <- rda(only_traits_Years, scale = TRUE)

screeplot(pca2, bstick = TRUE)

fNMDS <- fortify(pca, axes = 1:4) %>% 
  filter(Score == "sites") %>% 
  bind_cols(ITEX_traits_Years)

ggplot(fNMDS, aes(x = PC1, y = PC2, group = PlotID, shape = Treatment, colour = Site, linetype = Treatment)) +
  geom_point(aes(size = ifelse(Year == min(as.numeric(Year)), "First", "Other"))) +
  geom_path() +
  coord_equal() +
  scale_size_discrete(name = "Year", range = c(1.2, 2.5), limits = c("Other", "First"), breaks = c("First", "Other")) +
  scale_shape_manual(values = c(1, 16)) + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  labs(x = "PC1", y = "PC2") +
  theme_bw()

