# LOAD DATA AND LIBRARIES

library("tidyverse")
library("vegan")
library("ggvegan")
library("broom")
library("lme4")


load(file = "community/cleaned_data/CommunitySV_ITEX_2003_2015.Rdata", verbose = TRUE)
traitMean <- readRDS(file = "traits/cleaned_data/community_weighted_means.RDS") %>% 
  as.tibble() %>% 
  mutate(mean = as.numeric(as.character(mean)))


### Calcualte respones ### 
CommResp <- CommunitySV_ITEX_2003_2015 %>% 
  group_by(Year, Site, Treatment, PlotID) %>%  
  summarise(n = n(),
            Richness = n, 
            Diversity = diversity(Abundance), 
            N1 = exp(Diversity),
            Evenness = Diversity/log(Richness),
            sumAbundance = sum(Abundance),
            propGraminoid = sum(Abundance[FunctionalGroup %in% c("graminoid")])/sumAbundance,
            propLichen = sum(Abundance[FunctionalGroup %in% c("lichen")])/sumAbundance,
            propBryo = sum(Abundance[FunctionalGroup %in% c("moss", "liverwort")])/sumAbundance,
            totalVascular = sum(Abundance[FunctionalGroup %in% c("graminoid", "forbsv", "eshrub", "dshrub")])
            #vegetationHeight = mean(vegetationHeight)
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

# Ordination
comm_fat <- CommunitySV_ITEX_2003_2015 %>% 
  select(-Taxon, -FunctionalGroup) %>% 
  arrange(Year) %>% 
  spread(key = Spp, value = Abundance, fill = 0)

set.seed(32)

comm_fat_spp <- comm_fat %>% select(-(Site:Year))

NMDS <- metaMDS(comm_fat_spp, noshare = TRUE, try = 30)

fNMDS <- fortify(NMDS) %>% 
  filter(Score == "sites") %>% 
  bind_cols(comm_fat %>% select(Site:Year))

ggplot(fNMDS, aes(x = Dim1, y = Dim2, group = PlotID, shape = Treatment, colour = Site)) +
  geom_point(aes(size = ifelse(Year == min(as.numeric(Year)), "First", "Other"))) +
  geom_path() + 
  coord_equal() +
  scale_size_discrete(name = "Year", range = c(1, 2.5), limits = c("Other", "First"), breaks = c("First", "Other")) +
  scale_shape_manual(values = c(1, 16)) + 
  labs(x = "NMDS axis 1", y = "NMDS axis 2") +
  theme_bw()


setdiff(CommunitySV_ITEX_2003_2015$Taxon, traitsITEX_SV_2018$Taxon)
CommunitySV_ITEX_2003_2015 %>% 
  filter(FunctionalGroup %in% c("graminoid", "forbsv", "eshrub", "dshrub")) %>% 
  anti_join(traitsITEX_SV_2018, by = "Taxon") %>% distinct(Taxon)

load(file = "traits/cleaned_data/community_weighted_means.RDS")
#save(CommunitySV_ITEX_2003_2015, file = "community/data/CommunitySV_ITEX_2003_2015.Rdata")
metaItex <- CommunitySV_ITEX_2003_2015 %>% 
  distinct(Site, Treatment, PlotID)

traitMean %>% 
  left_join(metaItex, by = c("Site", "PlotID")) %>% 
  #filter(trait == "wetSLA_cm2_per_g") %>% 
  ggplot(aes(x = Year, y = mean, fill = Treatment)) +
  geom_boxplot() +
  scale_fill_manual(values = c("grey", "red")) +
  labs(y = "Trait mean", x = "") +
  facet_grid(trait ~ Site, scales = "free_y")


### CCA or RDA
library("vegan")
env <- traitMean %>% 
  left_join(metaItex, by = c("Site", "PlotID")) %>% 
  spread(key = trait, value = mean) %>% 
  filter(Site == "BIS") %>% 
  select(Treatment, Year)

trait <- traitMean %>% 
  left_join(metaItex, by = c("Site", "PlotID")) %>% 
  spread(key = trait, value = mean) %>% 
  filter(Site == "BIS") %>% 
  select(-Site, -Treatment, -PlotID, -Year)

ccaBIS <- cca(trait ~ Treatment * Year, data = env)
plot(ccaBIS, display = "cn")
anova(ccaBIS,  by = "axis", permu = 200)
