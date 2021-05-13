#### FLUX DATA ANALYSIS ####
source("ITEX_analyses/2_Import_data.R")

## process data
ITEX.data.pre.calcs <- ITEX.data.raw %>%
  mutate(
    Gradient = "1",
    BlockID = NA,
    Date = dmy(Date),     # changing Date from character to date format
    AirTemp = NA, # need iButton data!!
    NEE = -1* NEE  ## proper terminology
  ) %>%
  select(
    Country,
    Year,
    Gradient,
    BlockID,
    Project,
    Site,           ## changing variable names
    PlotID,
    Treatment,
    Date,
    Time = StartTime,
    Type = Cover,
    LNrsqd = rsqd,
    nee_lm = NEE,
    SoilTemp = ST_mean,
    SoilMoist = SM_mean,
    CanTemp = IR_mean,
    AirTemp,
    PAR = PAR_mean,
    Weather,
    Notes = comment
  )


# separate dfs for Dark and Light measurements  
ITEX_ER<-subset(ITEX.data.pre.calcs, Type== "D")        #& LNrsqd>=.8)
ITEX_NEE<-subset(ITEX.data.pre.calcs, Type== "L")    #& LNrsqd>=.8)

# to create new columns for specific fluxes in each df
ITEX_ER_mean <- ITEX_ER
ITEX_ER_mean$ER_ln <- ITEX_ER_mean$nee_lm  

ITEX_NEE_mean <- ITEX_NEE                           
ITEX_NEE_mean$NEE_ln <- ITEX_NEE_mean$nee_lm

### merge dfs by selected columns, then calculate GEP
ITEX_GEP_mean <- merge(ITEX_NEE_mean, ITEX_ER_mean, by = c("PlotID", "Date"))
ITEX_GEP_mean$GEP_ln <- ITEX_GEP_mean$nee_lm.x - ITEX_GEP_mean$nee_lm.y   # NEE - ER = GEP  linear

#### Last, select important columns to keep
ITEX.data.post.calcs <- as_tibble(ITEX_GEP_mean) %>%
  mutate(AirTemp = NA)%>%
  mutate(ITEX_Site = substr(PlotID, 1,3))%>%
  select(
    Country = Country.x,
    Gradient = Gradient.x,
    BlockID = BlockID.x,
    Project = Project.x,
    Year = Year.x,
    Site = Site.x,
    ITEX_Site = ITEX_Site,
    PlotID,
    Treatment = Treatment.x,
    Date,
    #Time = Time.x,
    NEE_ln,
    ER_ln,
    GEP_ln,
    SoilTemp = SoilTemp.x,
    SoilMoist = SoilMoist.x,
    AirTemp_Light = AirTemp,
    AirTemp_Dark = AirTemp,
    CanTemp_Light = CanTemp.x,
    CanTemp_Dark = CanTemp.y,
    PAR = PAR.x#,
    #Weather = Weather.x,
    #Notes = Notes.x
  )


## Changing NaNs to NAs! Needs to be done to assure no NaNs remain..
ITEX.data.post.calcs [ is.na(ITEX.data.post.calcs) ] <- NA

## Remove positive Reco values and negative GEP values
ITEX.data.post.calcs <- ITEX.data.post.calcs %>%
  filter(!is.na(NEE_ln) & GEP_ln > 0 & ER_ln < 0)


### FIGURES drafting ###
# points GEP PAR
#ggplot(ITEX.data.post.calcs, aes(x=PAR, y=GEP_ln, colour=ITEX_Site)) +    #colour=Type for at f? farve ogs? p? outliers
#        geom_point() 

#ggplot(ITEX.data.post.calcs, aes(x=SoilTemp, y=ER_ln, colour=ITEX_Site)) +    #colour=Type for at f? farve ogs? p? outliers
#  geom_point() 

#ggplot(ITEX.data.post.calcs, aes(x=SoilMoist, y=ER_ln, colour=ITEX_Site)) +    #colour=Type for at f? farve ogs? p? outliers
#  geom_point() 


####### Load plant community data #########
#Data with plant community, traits and fluxes. Do not use flux data, but plant data are updated and ready to use

### USE COMM.RESP from 2015 HERE !!!!!

ITEX_all$gram <- ITEX_all$sumAbundance * ITEX_all$propGraminoid
ITEX_all$forb <- ITEX_all$sumAbundance * ITEX_all$propForb
ITEX_all$bryo <- ITEX_all$sumAbundance * ITEX_all$propBryo
ITEX_all$lichen <- ITEX_all$sumAbundance * ITEX_all$propLichen
ITEX_all$shrub <- ITEX_all$sumAbundance * ITEX_all$propShrub
ITEX_all$eshrub <- ITEX_all$sumAbundance * ITEX_all$propEShrub
ITEX_all$dshrub <- ITEX_all$sumAbundance * ITEX_all$propDShrub

ITEX.plants <- ITEX_all %>%
  select(PlotID, Treatment, Richness, Diversity, Evenness, Height_cm, gram, forb, bryo, lichen, shrub, eshrub, dshrub, sumAbundance) %>%
  mutate(Biomass_Proxy = Height_cm * sumAbundance)

ITEX.data.post.calcs$PlotID <-  gsub("L", "", ITEX.data.post.calcs$PlotID)
ITEX.biomass.flux <- left_join(ITEX.data.post.calcs, ITEX.plants, by = c("PlotID", "Treatment"))

#### Standization of fluxes #####
## per Habitat = ITEX_site BIS, CAS, DRY
ITEX.fluxes.BIS<- ITEX.biomass.flux %>%  filter(ITEX_Site == "BIS" )
ITEX.fluxes.CAS<- ITEX.biomass.flux %>%  filter(ITEX_Site == "CAS" )
ITEX.fluxes.DRY<- ITEX.biomass.flux %>%  filter(ITEX_Site == "DRY" )

# fit exponential curve for respiration measurements per habitat (Loyd et al 1994)
fit.Reco_BIS<-nls((ER_ln~A*exp(-308.56/I(SoilTemp-227.13))), start=c(A=0 ), data=ITEX.fluxes.BIS)
fit.Reco_CAS<-nls((ER_ln~A*exp(-308.56/I(SoilTemp-227.13))), start=c(A=0 ), data=ITEX.fluxes.CAS)
fit.Reco_DRY<-nls((ER_ln~A*exp(-308.56/I(SoilTemp-227.13))), start=c(A=0 ), data=ITEX.fluxes.DRY)

#Recalculating Reco to 15C values taking in account heteroscedasticity 
ITEX.fluxes.BIS$Reco15<-ITEX.fluxes.BIS$ER_ln/fitted(fit.Reco_BIS)*(coef(fit.Reco_BIS)*exp(-308.56/I(288.15-227.13)))
ITEX.fluxes.CAS$Reco15<-ITEX.fluxes.CAS$ER_ln/fitted(fit.Reco_CAS)*(coef(fit.Reco_CAS)*exp(-308.56/I(288.15-227.13)))
ITEX.fluxes.DRY$Reco15<-ITEX.fluxes.DRY$ER_ln/fitted(fit.Reco_DRY)*(coef(fit.Reco_DRY)*exp(-308.56/I(288.15-227.13)))                


# fit rectangular hyperbolic saturation curve to GPP measurements (Thornley & Johnson, 1990)
fit.GPP_BIS<-nls((GEP_ln~ (A*B*PAR)/(A*PAR+B)), start=c(A=0.01, B=2), data=ITEX.fluxes.BIS)
fit.GPP_CAS<-nls((GEP_ln~ (A*B*PAR)/(A*PAR+B)), start=c(A=0.01, B=2), data=ITEX.fluxes.CAS)
fit.GPP_DRY<-nls((GEP_ln~ (A*B*PAR)/(A*PAR+B)), start=c(A=0.01, B=2), data=ITEX.fluxes.DRY)

#Recalculating GPP values taking in account heteroscedasticity 
ITEX.fluxes.BIS$GPP700<-ITEX.fluxes.BIS$GEP_ln/fitted(fit.GPP_BIS)*(((coef(fit.GPP_BIS)[1])*(coef(fit.GPP_BIS)[2])*1200)/((coef(fit.GPP_BIS)[1])*700+(coef(fit.GPP_BIS)[2])))
ITEX.fluxes.CAS$GPP700<-ITEX.fluxes.CAS$GEP_ln/fitted(fit.GPP_CAS)*(((coef(fit.GPP_CAS)[1])*(coef(fit.GPP_CAS)[2])*1200)/((coef(fit.GPP_CAS)[1])*700+(coef(fit.GPP_CAS)[2])))
ITEX.fluxes.DRY$GPP700<-ITEX.fluxes.DRY$GEP_ln/fitted(fit.GPP_DRY)*(((coef(fit.GPP_DRY)[1])*(coef(fit.GPP_DRY)[2])*1200)/((coef(fit.GPP_DRY)[1])*700+(coef(fit.GPP_DRY)[2])))

# combine all ITEX standardized data
ITEX.fluxes.finished.standard<- rbind(ITEX.fluxes.BIS, ITEX.fluxes.CAS, ITEX.fluxes.DRY)

#calculate mean flux per plot
ITEX.mean.fluxes<- ITEX.fluxes.finished.standard%>%
 group_by(PlotID, Treatment, ITEX_Site)%>%
 summarise_if(is.numeric, mean, na.rm = TRUE)

# save(ITEX.fluxes.finished.standard, file ="C:\\Users\\ial008\\Google Drive\\Team TraitTrain CarbonFlux\\Standard_ITEXFluxes.Rdata")
# ITEX <- ITEX.fluxes.finished.standard

#### REMOVING 4 CAS PLOTS BECAUSE OF FROST DAMAGE where cassiope died (CAS-4, 6, 9, 10). 
ITEX.mean.fluxes <- ITEX.mean.fluxes %>% 
  filter(!(PlotID == 'CAS-4' | PlotID == 'CAS-6' | PlotID == 'CAS-9' | PlotID == 'CAS-10'))%>%
  ungroup()


# Trait data
ITEX_CWM<- ItexTraitMeans%>%
  filter(Year==2015)%>%
  select(-X1, -mean_noitv)%>%
  spread(Trait, mean)%>%
  select(-dC13_percent, -dN15_percent, -Dry_Mass_g, -Year)%>%
  rename(CWM_C = C_percent, CWM_N = N_percent, CWM_CN= CN_ratio, CWM_P = P_Ave, CWM_LDMC = LDMC, CWM_LA = Leaf_Area_cm2, CWM_LT = Leaf_Thickness_Ave_mm, CWM_SLA= SLA_cm2_g, CWM_Height = Plant_Height_cm)

ITEX_CWMnoitv<- ItexTraitMeans%>%
  filter(Year==2015)%>%
  select(-X1, -mean)%>%
  spread(Trait, mean_noitv)%>%
  select(-dC13_percent, -dN15_percent, -Dry_Mass_g, -Year)%>%
  rename(noitv_C = C_percent, noitv_N = N_percent, noitv_CN= CN_ratio, noitv_P = P_Ave, noitv_LDMC = LDMC, noitv_LA = Leaf_Area_cm2, noitv_LT = Leaf_Thickness_Ave_mm, noitv_SLA= SLA_cm2_g, noitv_Height = Plant_Height_cm)

ITEXtraits <- left_join(ITEX_CWM, ITEX_CWMnoitv, by= c("PlotID", "Treatment", "Site"))
ITEX.mean.fluxes <- left_join(ITEX.mean.fluxes, ITEXtraits, by = c("PlotID", "Treatment"))


#Moss data
#moss <- readxl::read_xlsx('mossdepth_endalen2015.xlsx', sheet = 'average.moss')
# fix plotID
#moss <- moss %>% mutate(new = paste0(substr(habitat, 1, 4), plot))
#moss$PlotID <- moss$new #combine first four characters in habitat and the plot value (so PlotID is DRY-1, DRY-2 etc) 
#ITEX.mean.fluxes <- left_join(ITEX.mean.fluxes, moss, by='PlotID')


#### GRAPH of flux-means of sites ####

# run for file out initiation
#pdf("NEE_2019.pdf")
NEEplot <- ggplot (ITEX.mean.fluxes, aes(x=ITEX_Site, y=NEE_ln, fill = ITEX_Site)) +
  geom_boxplot() +
  scale_size_manual(values=c(3,3,3)) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    legend.position = "none")+
    #scale_color_manual(values = c("grey70", "red", "black"))+
    scale_fill_manual(values = c('gray70', 'red', 'white'))+
  # labs(x = ("Soil temperature (?C)"))+
  labs(x = ("Habitat"))+
  #ylab(bquote('PAR-Standardized GPP ('*mu~'mol' ~CO[2]~ m^-2~s^-1*')')) +
  ylab(bquote('NEE ('*mu~'mol' ~CO[2]~ m^-2~s^-1*')')) +
  stat_summary(geom = 'text', label = c('a', 'b', 'a'), 
               fun = max, 
               vjust = -1,
               size = 5) +
  ylim(-9, 0.5) 

#print(NEE)
#dev.off()


#### figure code for Reco ####
#pdf("Reco_2019.pdf")
Recoplot <- ggplot (ITEX.mean.fluxes, aes(x=ITEX_Site, y=ER_ln, fill = ITEX_Site)) +
  geom_boxplot() +
  scale_size_manual(values=c(3,3,3)) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    legend.position = "none")+
  #scale_color_manual(values = c("grey70", "red", "black"))+
  scale_fill_manual(values = c('gray70', 'red', 'white'))+
  # labs(x = ("Soil temperature (?C)"))+
  labs(x = ("Habitat"))+
  #ylab(bquote('PAR-Standardized GPP ('*mu~'mol' ~CO[2]~ m^-2~s^-1*')')) +
  ylab(bquote('Reco ('*mu~'mol' ~CO[2]~ m^-2~s^-1*')')) +
  stat_summary(geom = 'text', label = c('a', 'ab', 'b'), 
               fun = max, 
               vjust = -1,
               size = 5) +
  ylim(-9, 0.5) 

#print(Reco)
#dev.off()


#### figure code for GEP ####
#pdf("GEP_2019.pdf")
GEPplot <- ggplot (ITEX.mean.fluxes, aes(x=ITEX_Site, y=GPP700, fill = ITEX_Site)) +
  geom_boxplot() +
  scale_size_manual(values=c(3,3,3)) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    legend.position = "none")  +
  #scale_color_manual(values = c("grey70", "red", "black"))+
  scale_fill_manual(values = c('gray70', 'red', 'white'))+
  # labs(x = ("Soil temperature (?C)"))+
  labs(x = ("Habitat"))+
  ylab(bquote('PAR-Standardized GPP ('*mu~'mol' ~CO[2]~ m^-2~s^-1*')')) +
  #ylab(bquote('Reco ('*mu~'mol' ~CO[2]~ m^-2~s^-1*')')) +
  stat_summary(geom = 'text', label = c('a', 'ab', 'b'), 
               fun = max, 
               vjust = -1,
               size = 5) +
  ylim(-0.5, 9) 

#print(GEP)
#dev.off()


#######################################################################################
#### I DONT KNOW WHAT LETTERS APPLY TO THE NEE FLUXES... THAT HAS NOT BEEN SAVED ? ####
#### JUST USED INFO FROM THE MANUSCRIPT RESULTS SECTION - I ASSUME THAT IS CORRECT ####
#######################################################################################
  
# make some graph?? 
ggplot (ITEX.mean.fluxes, aes(x=sumAbundance, y=GPP700, colour = ITEX_Site)) +
    geom_point(aes(shape=Treatment, color=ITEX_Site, size=Treatment)) +
    scale_size_manual(values=c(3,3,3)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    scale_color_manual(values = c("grey70", "red", "black"))+
    # labs(x = ("Soil temperature (?C)"))+
    labs(x = ("Abundance Forbs (%)"))+
    #  ylab(bquote('PAR-Standardized GPP ('*mu~'mol' ~CO[2]~ m^-2~s^-1*')'))
    ylab(bquote('ER ('*mu~'mol' ~CO[2]~ m^-2~s^-1*')'))
  #ylab(bquote('PAR-Standardized NEE ('*mu~'mol' ~CO[2]~ m^-2~s^-1*')'))

 

#### New linear models #####
summary(lm (NEE_ln ~  Treatment, data = ITEX.mean.fluxes))
summary(lm (NEE_ln ~  ITEX_Site, data = ITEX.mean.fluxes))

summary(lm (GPP700 ~  Treatment, data = ITEX.mean.fluxes))# standardized GPP
summary(lm (GPP700 ~  ITEX_Site, data = ITEX.mean.fluxes))# standardized GPP

summary(lm (Reco15 ~  Treatment, data = ITEX.mean.fluxes))# standardized Reco
summary(lm (Reco15 ~  ITEX_Site, data = ITEX.mean.fluxes))# standardized Reco

### correlation matrix 
library(corrplot)
library(Hmisc)
# create correlation matrix for predictors Climate, Veg structure and traits 
cor_ITEX <- ITEX.mean.fluxes %>%
  ungroup()%>%
  dplyr::select(SoilTemp, SoilMoist, CanTemp_Light, Richness:lichen, eshrub, dshrub, CWM_C:CWM_SLA)

cor_ITEX <- rcorr(as.matrix(cor_ITEX))

cor_ITEX$r #the correlation matrix
cor_ITEX$n #the matrix of the number of observations used in analyzing each pair of variables
cor_ITEX$P #the p-values corresponding to the significance levels of correlations.

library(RColorBrewer)
par(mfrow=c(1,1))
brewer.pal(11, "PiYG")
brewer.pal(11, "PuOr")
col1 <- colorRampPalette(c("#40004B", "#762A83", "#9970AB", "#C2A5CF", "#E7D4E8", "#F7F7F7", "#D9F0D3", "#A6DBA0", "#5AAE61", "#1B7837", "#00441B"))
col2 <- colorRampPalette(c("#7F3B08", "#B35806", "#E08214", "#FDB863", "#FEE0B6", "#F7F7F7", "#D8DAEB", "#B2ABD2", "#8073AC", "#542788", "#2D004B"))
col3 <- colorRampPalette(c("#8E0152", "#C51B7D", "#DE77AE", "#F1B6DA", "#FDE0EF", "#F7F7F7", "#E6F5D0", "#B8E186", "#7FBC41", "#4D9221", "#276419"))

corrplot.mixed(cor_ITEX$r,  lower.col = "black", upper.col= col1(10), number.cex = .7, upper = "square", diag = "u", 
               tl.pos = "lt", tl.col = "black", p.mat = cor_ITEX$P, sig.level = 0.05,  insig = "blank")


###### USING NOT STANDARDIZED FLUXES!!!!!! #########
# Effect of single predictors on NEE. GPP and Reco
#### Effect size plots ####
library(lme4)

labels <- c(GPP700 = "GPP", Reco15 = "Reco", NEE_ln = "NEE")

#effect of single predictors on 
z1<-ITEX.mean.fluxes%>%
  ungroup()%>%
  dplyr::select(NEE_ln, ER_ln, GPP700, Site, Treatment, SoilTemp, SoilMoist, CanTemp_Light, Richness:lichen, eshrub, dshrub, CWM_C:noitv_SLA)%>%
  rename(NEE = NEE_ln, GPP = GPP700, Reco = ER_ln)%>%
  gather(key = pred, value = value, -c(NEE, GPP, Reco, Site, Treatment))%>%
  gather(key = response, value = Cflux, -c(Site, Treatment, pred, value))%>%
  group_by(response, pred)%>%
  mutate(value = scale(value))%>%
  do(tidy(lm(Cflux ~ value , data=. ))) %>%
  filter(!term == "(Intercept)") %>% 
  filter(!grepl("^sd_", term)) %>% 
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96)) %>%
  as.data.frame()

#postscript(file = "O:\\FunCab\\Manuscripts\\CO2trait\\figures\\Figure1.eps", width = 8.3, height = 5.8)
ggplot(z1, aes(x =pred, y = estimate, shape=response, fill= response, ymin = lower, ymax = upper)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "solid", color="grey") +
  #geom_vline(xintercept = c(8.5, 16.5, 22.5), linetype = "dotted", size =1) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_shape_manual(limits = c("NEE", "GPP", "Reco"), labels = c("NEE", "GPP", "Reco"), values = c(22, 24,21)) + 
  scale_fill_manual(limits = c("NEE", "GPP", "Reco"), labels = c("NEE", "GPP", "Reco"), values = c("red", "grey70", "black")) + 
  scale_x_discrete(limits=c( "noitv_CN", "noitv_N", "noitv_C", "noitv_P", "noitv_SLA", "noitv_LDMC", "noitv_LT", "noitv_LA", "noitv_Height", "CWM_CN", "CWM_N", "CWM_C", "CWM_P", "CWM_SLA", "CWM_LDMC", "CWM_LT", "CWM_LA", "CWM_Height", "Richness", "Evenness", "Diversity", "Height_cm", "gram", "forb", "bryo", "eshrub", "dshrub", "lichen", "CanTemp_Light","SoilTemp", "SoilMoist"), labels=c("noitv CN", "noitv N", "noitv C", "noitv P", "noitv SLA", "noitv LDMC", "noitv LT", "noitv LA", "noitv Height", "CWM CN", "CWM N", "CWM C", "CWM P", "CWM SLA", "CWM LDMC", "CWM LT", "CWM LA", "CWM Height", "Richness", "Evenness", "Diversity", "Plant Height", "Graminoid", "Forb", "Bryophyte", "Evergreen shrub", "Decidious shrub", "Lichen", "Canopy Temperature","Soil Temperature", "Soil Moisture"))+
  #labs(x= "             CWV traits                CWM traits             Veg structure      Climate")+
  facet_grid(~response)+ #, labeller=labeller(response = labels)
  theme(axis.title.x=element_text(size = 12), axis.text.x=element_text(size = 12), axis.text.y = element_text(size = 12), axis.title.y=element_blank(), strip.background = element_rect(colour="black", fill="white"), panel.background = element_rect(fill= "white"), panel.border = element_rect(colour = "black", fill=NA), strip.text.x = element_text(size=12, face="bold"),  axis.line = element_line(colour = "black"), legend.position = "none" )+
  coord_flip()
#dev.off()


#### difference between trait effects on Cflux when taking into account ITV or not
z2<-ITEX.mean.fluxes%>%
  ungroup()%>%
  dplyr::select(NEE_ln, ER_ln, GPP700, Site, Treatment, CWM_C:noitv_SLA)%>%
  rename(NEE = NEE_ln, GPP = GPP700, Reco = ER_ln)%>%
  group_by(Site, Treatment)%>%
  gather(key =Trait, value = Tvalue, CWM_C:noitv_SLA)%>%
  separate(Trait, c("ITV", "Trait"), sep = "_")%>%
  #gather(key = pred, value = value, -c(NEE, GPP, Reco, Site, Treatment, ITV))%>%
  gather(key = response, value = Cflux, -c(Site, Treatment,ITV, Trait, Tvalue))%>%
  group_by(response, Trait, ITV)%>%
  mutate(Tvalue = scale(Tvalue))%>%
  do(tidy(lm(Cflux ~ Tvalue , data=. ))) %>%   # not sure if Site should be in here
  filter(!term == "(Intercept)") %>% 
  filter(!grepl("^sd_", term)) %>% 
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96)) %>%
  as.data.frame()


#postscript(file = "O:\\FunCab\\Manuscripts\\CO2trait\\figures\\Figure1.eps", width = 8.3, height = 5.8)
ggplot(z2, aes(x =Trait, y = estimate, shape=response, col= ITV, ymin = lower, ymax = upper)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "solid", color="grey") +
  #geom_vline(xintercept = c(8.5, 16.5, 22.5), linetype = "dotted", size =1) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  #scale_shape_manual(labels = c("NEE", "GPP", "Reco"), values = c(22, 24,21)) + 
  #scale_fill_manual(labels = c("NEE", "GPP", "Reco"), values = c("red", "grey70", "black")) + 
  scale_x_discrete(limits=c( "Wvar_CN", "Wvar_N", "Wvar_C", "Wvar_VH", "Wvar_SLA", "Wvar_LDMC", "Wvar_Lth", "Wvar_LA", "CWM_CN", "CWM_N", "CWM_C", "CWM_VH", "CWM_SLA", "CWM_LDMC", "CWM_Lth", "CWM_LA", "Richness", "Evenness", "Diversity", "VegetationHeight", "Temp.C", "P.mm"), labels=c( "Wvar_CN" = "LCNvar", "Wvar_N" = "LNvar", "Wvar_C"= "LCvar", "Wvar_Height"= "Hvar", "Wvar_SLA" = "SLAvar", "Wvar_LDMC" = "LDMCvar", "Wvar_Lth" = "LTvar", "Wvar_LA" = "LAvar", "Wmean_CN" = "LCNmean", "Wmean_N" = "LNmean", "Wmean_C"= "LCmean", "Wmean_Height"= "Hmean", "Wmean_SLA" = "SLAmean", "Wmean_LDMC" = "LDMCmean", "Wmean_Lth" = "LTmean", "Wmean_LA" = "LAmean", "VegetationHeight" = "Vegetation Height", "Temp.C" = "Temperature", "P.mm" = "Precipitation"))+
  #labs(x= "             CWV traits                CWM traits             Veg structure      Climate")+
  facet_grid(~response)+ #, labeller=labeller(response = labels)
  theme(axis.title.x=element_text(size = 12), axis.text.x=element_text(size = 12), axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.title.y=element_text(size = 12), strip.background = element_rect(colour="black", fill="white"), panel.background = element_rect(fill= "white"), panel.border = element_rect(colour = "black", fill=NA), strip.text.x = element_text(size=12, face="bold"),  axis.line = element_line(colour = "black"))+
  coord_flip()
#dev.off()


#### MuMin Model selection + VIF ####
library(MASS)
library(MuMIn)

# GPP700 models:
# Environment
GPP.env<-summary(lm(GPP700 ~ CanTemp_Light, data = ITEX.mean.fluxes))

# community
GPP_com<- lm(GPP700 ~ Richness + Evenness + Diversity + Height_cm + gram + forb + bryo + eshrub + dshrub+ lichen, data = ITEX.mean.fluxes)
step <- stepAIC(GPP_com, direction="backward")
GPP.com<-summary(lm(GPP700 ~ Richness + Height_cm, data = ITEX.mean.fluxes))

# Traits
GPP_traitCWM<- lm(GPP700 ~ CWM_N + CWM_CN + CWM_P + CWM_SLA + CWM_LDMC + CWM_LT + CWM_LA + CWM_Height, data = ITEX.mean.fluxes)
step <- stepAIC(GPP_traitCWM, direction="backward")
GPP.CWM<-summary(lm(GPP700 ~  CWM_CN , data = ITEX.mean.fluxes)) 

# Traits No ITV, same predictors as with CWM to compare influence of ITV
GPP_traitNoITV<- lm(GPP700 ~ noitv_N + noitv_CN + noitv_P + noitv_SLA + noitv_LDMC + noitv_LT + noitv_LA + noitv_Height, data = ITEX.mean.fluxes)
step <- stepAIC(GPP_traitNoITV, direction="backward")
GPP.noitv<-summary(lm(GPP700 ~  noitv_CN , data = ITEX.mean.fluxes)) 

# combination
GPP.env.com<-summary(lm(GPP700 ~ Richness + Height_cm + CanTemp_Light, data = ITEX.mean.fluxes)) 
GPP.env.CWM<-summary(lm(GPP700 ~ CanTemp_Light + CWM_CN , data = ITEX.mean.fluxes)) 
GPP.com.CWM<-summary(lm(GPP700 ~ Richness + Height_cm + CWM_CN , data = ITEX.mean.fluxes)) 
GPP.env.com.CWM<-summary(lm(GPP700 ~ CanTemp_Light +Richness + Height_cm + CWM_CN , data = ITEX.mean.fluxes)) 

GPP.env.noitv<-summary(lm(GPP700 ~ CanTemp_Light + noitv_CN , data = ITEX.mean.fluxes))
GPP.com.noitv<-summary(lm(GPP700 ~ Richness + Height_cm + noitv_CN , data = ITEX.mean.fluxes))
GPP.env.com.noitv<-summary(lm(GPP700 ~ CanTemp_Light +Richness + Height_cm + noitv_CN , data = ITEX.mean.fluxes)) 

# Reco models:
# Environment
R.env<-summary(lm(ER_ln ~  CanTemp_Light, data = ITEX.mean.fluxes))

# community
R_com<- lm(ER_ln ~ Richness + Evenness + Diversity + Height_cm + gram + forb + bryo + eshrub + dshrub+ lichen, data = ITEX.mean.fluxes)
step <- stepAIC(R_com, direction="backward")
R.com<-summary( lm(ER_ln ~ Richness + Diversity + bryo , data = ITEX.mean.fluxes))

# traits 
R_traitCWM<- lm(ER_ln ~ CWM_N + CWM_CN + CWM_P + CWM_SLA + CWM_LDMC + CWM_LT + CWM_LA + CWM_Height, data = ITEX.mean.fluxes)
step <- stepAIC(R_traitCWM, direction="backward")
R.CWM<-summary(lm(ER_ln ~  CWM_CN + CWM_Height +CWM_LA, data = ITEX.mean.fluxes)) 

# Traits No ITV, same predictors as with CWM to compare influence of ITV
R_traitNoITV<- lm(ER_ln ~ noitv_N + noitv_CN + noitv_P + noitv_SLA + noitv_LDMC + noitv_LT + noitv_LA + noitv_Height, data = ITEX.mean.fluxes)
step <- stepAIC(R_traitNoITV, direction="backward")
R.noitv<-summary(lm(ER_ln ~  noitv_CN + noitv_Height + noitv_LA , data = ITEX.mean.fluxes))

R.env.com<-summary(lm(ER_ln ~  CanTemp_Light+ Richness + Diversity + bryo, data = ITEX.mean.fluxes)) 
R.env.CWM<-summary(lm(ER_ln ~  CanTemp_Light + CWM_CN + CWM_Height +CWM_LA, data = ITEX.mean.fluxes))
R.com.CWM<-summary(lm(ER_ln ~  Richness + Diversity + bryo + CWM_CN + CWM_Height +CWM_LA, data = ITEX.mean.fluxes))
R.env.com.CWM<-summary(lm(ER_ln ~  CanTemp_Light+ Richness + Diversity + bryo+ CWM_CN + CWM_Height +CWM_LA, data = ITEX.mean.fluxes))

R.env.noitv<-summary(lm(ER_ln ~  CanTemp_Light + noitv_CN + noitv_Height + noitv_LA, data = ITEX.mean.fluxes))
R.com.noitv<-summary(lm(ER_ln ~  Richness + Diversity + bryo + noitv_CN + noitv_Height + noitv_LA, data = ITEX.mean.fluxes)) 
R.env.com.noitv<-summary(lm(ER_ln ~  CanTemp_Light+ Richness + Diversity + bryo+ noitv_CN + noitv_Height + noitv_LA, data = ITEX.mean.fluxes))

# NEE models:
# Environment
NEE.env<-summary(lm(NEE_ln ~  SoilTemp, data = ITEX.mean.fluxes))
AICc(lm(NEE_ln ~  SoilTemp, data = ITEX.mean.fluxes))
step <- stepAIC(NEE.env, direction="backward")

# community
NEE_com<- lm(NEE_ln ~ Richness + Evenness + Diversity + Height_cm + gram + forb + bryo + eshrub + dshrub+ lichen, data = ITEX.mean.fluxes)
step <- stepAIC(NEE_com, direction="backward")
NEE.com<-summary( lm(NEE_ln ~ Height_cm + Evenness, data = ITEX.mean.fluxes))

# traits 
NEE_traitCWM<- lm(NEE_ln ~ CWM_N + CWM_CN + CWM_P + CWM_SLA + CWM_LDMC + CWM_LT + CWM_LA + CWM_Height, data = ITEX.mean.fluxes)
step <- stepAIC(NEE_traitCWM, direction="backward")
NEE.CWM<-summary(lm(NEE_ln ~  CWM_Height +CWM_LA, data = ITEX.mean.fluxes)) # N has biggest effect as single predictor

# Traits No ITV, same predictors as with CWM to compare influence of ITV
NEE_traitNoITV<- lm(NEE_ln ~ noitv_N + noitv_CN + noitv_P + noitv_SLA + noitv_LDMC + noitv_LT + noitv_LA + noitv_Height, data = ITEX.mean.fluxes)
NEE.noitv<-summary(lm(NEE_ln ~ noitv_Height + noitv_LA , data = ITEX.mean.fluxes)) # N has biggest effect as single predictor

NEE.env.com<-summary(lm(NEE_ln ~  SoilTemp+ Height_cm + Evenness, data = ITEX.mean.fluxes))
NEE.env.CWM<-summary(lm(NEE_ln ~  SoilTemp + CWM_Height +CWM_LA, data = ITEX.mean.fluxes))
NEE.com.CWM<-summary(lm(NEE_ln ~  Height_cm + Evenness+ CWM_Height +CWM_LA, data = ITEX.mean.fluxes))
NEE.env.com.CWM<-summary(lm(NEE_ln ~  SoilTemp+ Height_cm + Evenness + CWM_Height +CWM_LA, data = ITEX.mean.fluxes))

NEE.env.noitv<-summary(lm(NEE_ln ~  SoilTemp + noitv_Height + noitv_LA, data = ITEX.mean.fluxes))
NEE.com.noitv<-summary(lm(NEE_ln ~  Height_cm + Evenness + noitv_Height + noitv_LA, data = ITEX.mean.fluxes))
NEE.env.com.noitv<-summary(lm(NEE_ln ~  SoilTemp+ Height_cm + Evenness + noitv_Height + noitv_LA, data = ITEX.mean.fluxes))

CWM_ITV<- data.frame(Cflux = c("GPP", "Reco", "NEE"))
CWM_ITV$noITV <- c(GPP.noitv$adj.r.s, R.noitv$adj.r.s, NEE.noitv$adj.r.s)
CWM_ITV$ITV <- c(GPP.CWM$adj.r.s, R.CWM$adj.r.s, NEE.CWM$adj.r.s)
CWM_ITV$Cflux<- factor(CWM_ITV$Cflux, levels = c("GPP", "Reco", "NEE"))

p1<- CWM_ITV%>%
  mutate(ITV= ITV-noITV)%>%
  gather(key = ITV, value = Var.exp, noITV:ITV)%>%
  ggplot( aes(x=Cflux, y=Var.exp*100, fill=ITV))+
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values = c("grey20", "grey70"), limits = c("ITV", "noITV"),labels = c("intra", "inter"), name="Trait variation" )+
    ylab('Explained variance %') +
  scale_y_continuous(limits = c(-10, 70), breaks = seq(-10, 70, by = 10))+
  theme(axis.title.x=element_blank(), axis.text.x=element_text(size = 13), axis.title = element_text(size = 14), axis.text.y = element_text(size = 14), axis.title.y=element_text(size = 14),legend.title=element_text(size=13), legend.text=element_text(size=12),  panel.background = element_rect(fill= "white"), strip.text.x = element_text(size=12, face="bold"),  axis.line = element_line(colour = "black"), legend.position = c(0.20,0.95))
                

# Variance Decomposition
VarianceDecomp <- data_frame(Variables = c("Env", "Com", "Trait", "Env.Com", "Env.Trait", "Com.Trait", "Env.Com.Trait"))
VarianceDecomp$GPP<- c(GPP.env$adj.r.s, GPP.com$adj.r.s, GPP.CWM$adj.r.s, GPP.env.com$adj.r.s, GPP.env.CWM$adj.r.s, GPP.com.CWM$adj.r.s, GPP.env.com.CWM$adj.r.s) 
VarianceDecomp$NEE<-c(NEE.env$adj.r.s, NEE.com$adj.r.s,NEE.CWM$adj.r.s, NEE.env.com$adj.r.s, NEE.env.CWM$adj.r.s, NEE.com.CWM$adj.r.s, NEE.env.com.CWM$adj.r.s) 
VarianceDecomp$Reco<- c(R.env$adj.r.s, R.com$adj.r.s, R.CWM$adj.r.s, R.env.com$adj.r.s, R.env.CWM$adj.r.s, R.com.CWM$adj.r.s, R.env.com.CWM$adj.r.s) 

VarianceDecomp<- VarianceDecomp %>% remove_rownames %>% column_to_rownames(var="Variables")
VarianceDecomp<-as.data.frame(t(as.matrix(VarianceDecomp)))

VarianceDecomp<-VarianceDecomp%>%
   mutate(unique.Env = Env.Com.Trait - Com.Trait,
         unique.Com = Env.Com.Trait - Env.Trait,
         unique.Trait = Env.Com.Trait - Env.Com,
         combined.Env.Com = Env.Trait - Trait - unique.Env,
         combined.Env.Trait = Com.Trait - Com - unique.Trait,
         combined.Com.Trait = Env.Com - Env - unique.Com,
         combined.Env.Com.Trait =  Env.Com.Trait - (unique.Env + unique.Com + unique.Trait + combined.Env.Com + combined.Env.Trait + combined.Com.Trait),
         Cflux = c("GPP", "NEE", "Reco"))%>%
  gather(key = Variables, value = value, unique.Env:combined.Env.Com.Trait)%>%
  mutate(Variables = as.factor(Variables))

VarianceDecomp$Variables<- factor(VarianceDecomp$Variables, levels = c("unique.Trait", "unique.Com", "unique.Env", "combined.Com.Trait", "combined.Env.Trait", "combined.Env.Com", "combined.Env.Com.Trait"))
VarianceDecomp$Cflux<- factor(VarianceDecomp$Cflux, levels = c("GPP", "Reco", "NEE"))
  
VarianceDecomp2 <- data_frame(Variables = c("Env", "Com", "Trait", "Env.Com", "Env.Trait", "Com.Trait", "Env.Com.Trait"))
VarianceDecomp2$GPP<- c(GPP.env$adj.r.s, GPP.com$adj.r.s, GPP.noitv$adj.r.s, GPP.env.com$adj.r.s, GPP.env.noitv$adj.r.s, GPP.com.noitv$adj.r.s, GPP.env.com.noitv$adj.r.s) 
VarianceDecomp2$NEE<-c(NEE.env$adj.r.s, NEE.com$adj.r.s,NEE.noitv$adj.r.s, NEE.env.com$adj.r.s, NEE.env.noitv$adj.r.s, NEE.com.noitv$adj.r.s, NEE.env.com.noitv$adj.r.s) 
VarianceDecomp2$Reco<- c(R.env$adj.r.s, R.com$adj.r.s, R.noitv$adj.r.s, R.env.com$adj.r.s, R.env.noitv$adj.r.s, R.com.noitv$adj.r.s, R.env.com.noitv$adj.r.s) 

VarianceDecomp2<- VarianceDecomp2 %>% remove_rownames %>% column_to_rownames(var="Variables")
VarianceDecomp2<-as.data.frame(t(as.matrix(VarianceDecomp2)))

VarianceDecomp2<-VarianceDecomp2%>%
  mutate(unique.Env = Env.Com.Trait - Com.Trait,
         unique.Com = Env.Com.Trait - Env.Trait,
         unique.Trait = Env.Com.Trait - Env.Com,
         combined.Env.Com = Env.Trait - Trait - unique.Env,
         combined.Env.Trait = Com.Trait - Com - unique.Trait,
         combined.Com.Trait = Env.Com - Env - unique.Com,
         combined.Env.Com.Trait =  Env.Com.Trait - (unique.Env + unique.Com + unique.Trait + combined.Env.Com + combined.Env.Trait + combined.Com.Trait),
         Cflux = c("GPP", "NEE", "Reco"))%>%
  gather(key = Variables, value = value, unique.Env:combined.Env.Com.Trait)%>%
  mutate(Variables = as.factor(Variables))

VarianceDecomp2$Variables<- factor(VarianceDecomp2$Variables, levels = c("unique.Trait", "unique.Com", "unique.Env", "combined.Com.Trait", "combined.Env.Trait", "combined.Env.Com", "combined.Env.Com.Trait"))
VarianceDecomp2$Cflux<- factor(VarianceDecomp2$Cflux, levels = c("GPP", "Reco", "NEE"))

brewer.pal(n = 8, name = "Set2")
library(RColorBrewer)
p2<-ggplot(VarianceDecomp, aes(Cflux, value*100, fill=Variables))+
  geom_bar(position="stack", stat="identity")+
  geom_hline(yintercept=0)+
  scale_fill_manual(values = c("#FB9A99", "#FFFF99", "#A6CEE3", "#FDBF6F" , "#B2DF8A", "#CAB2D6", "#B3B3B3"), labels = c("Trait", "Taxonomy", "Environment", "Taxonomy & Trait", "Environment & Trait", "Environment & Taxonomy", "All" ))+
  #scale_fill_brewer(palette = "Dark2", labels = c("Trait", "Taxonomy", "Environment", "Taxonomy & Trait", "Environment & Trait", "Environment & Taxonomy", "All" ))+
  ylab('Explained variance %') +
  scale_y_continuous(limits = c(-10, 70), breaks = seq(-10, 70, by = 10))+
  theme(axis.title.x=element_blank(), axis.text.x=element_text(size = 13), axis.title = element_text(size = 14), axis.text.y = element_text(size = 14), axis.title.y=element_text(size = 14),legend.title=element_text(size=13), legend.text=element_text(size=12),  panel.background = element_rect(fill= "white"), strip.text.x = element_text(size=12, face="bold"),  axis.line = element_line(colour = "black"), legend.position = c(0.65,0.85))

p3<-ggplot(VarianceDecomp2, aes(Cflux, value*100, fill=Variables))+
  geom_bar(position="stack", stat="identity")+
  geom_hline(yintercept=0)+
  scale_fill_manual(values = c("#FB9A99", "#FFFF99", "#A6CEE3", "#FDBF6F" , "#B2DF8A", "#CAB2D6", "#B3B3B3"), labels = c("Trait", "Taxonomy", "Environment", "Taxonomy & Trait", "Environment & Trait", "Environment & Taxonomy", "All" ))+
  #scale_fill_brewer(palette = "Dark2")+
  ylab('Explained variance %') +
  scale_y_continuous(limits = c(-10, 70), breaks = seq(-10, 70, by = 10))+
  theme(axis.title.x=element_blank(), axis.text.x=element_text(size = 13), axis.title = element_text(size = 14), axis.text.y = element_text(size = 14), axis.title.y=element_text(size = 14),legend.title=element_text(size=13), legend.text=element_text(size=12),  panel.background = element_rect(fill= "white"), strip.text.x = element_text(size=12, face="bold"),  axis.line = element_line(colour = "black"), legend.position = "none")

library(cowplot)
plot_grid(p2, p3, p1, ncol=3, labels = c('A', 'B', "C"))

#################################################################


variation <- bind_rows(NEE = NEE, GPP700 = GEP, Reco = ER, .id = "Response") %>% 
  mutate(what = recode(what, ER_Environment = "Environment", NEE_Environment = "Environment", GEP_Environment = "Environment", NEE_Traits = "Traits", ER_Traits = "Traits", GEP_Traits = "Traits", GEP_FunGroup = "Community", ER_overlap = "Traits_Environment", NEE_overlap = "Traits_Environment", GEP_overlap_fgr_trt = "Community_Traits", GEP_overlap_env_trt = "Traits_Environment", GEP_overlap_env_fgr = "Community_Environment" )) %>% 
  mutate(covarience = grepl("-", what)) %>% 
  mutate( what = factor(what, levels = c("Traits_Environment", "Environment", "Traits", "Community", "Community_Traits", "Community_Environment" )))


#Dersom rekkef?lga p? legend blir forstyrra igjen, pr?v denne: 
variation$what <- factor(variation$what, levels = c("Environment", "Traits_Environment", "Traits", "Community_Traits", "Community_Environment"))

#variation$what <- factor(variation$what, levels =  rev(levels(variation$what)))
variation$Response <- factor(variation$Response, levels = c("Reco", "GPP700", "NEE"))

#### explained variation  PLOT####

#p <-
ggplot(variation [order(variation$what), ], aes(x = Response, y = explained, fill = what)) +
  geom_bar(stat='identity') +
  ylim(0,1) +
  coord_flip() +
  scale_fill_manual(name = "Variable Group", values=c('orange', 'darkorange1', 'dodgerblue3', 'forestgreen', 'chartreuse3')) +  #, 'sienna1', 'lightsteelblue4', 'lightsteelblue3', 'lightsteelblue1', 'royalblue3', 'lightskyblue')
  #scale_fill_brewer(palette = "Set1") +
  guides(fill = guide_legend(reverse = TRUE)) +
  #legend=c("Traits", "Traits_Environment", "Environment", "Community_Environment", "Community_Traits") +
  xlab('Flux component') +
  ylab('Explained variance') +
  #xlim(0,0.75) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


tiff("Explained Variance2.tif", width = 8, height = 4, units = 'in', res = 300)
p # Make plot
dev.off()






# Two groups on GEP
GEP_env_trt <- lm(GEP_ln ~ CanTemp_Light + N_percent, data = ITEX.mean.fluxes)  
GEP_env_fgr <- lm(GEP_ln ~ CanTemp_Light + shrub + forb, data = ITEX.mean.fluxes) 
GEP_trt_fgr <- lm(GEP_ln ~ N_percent + lichen, data = ITEX.mean.fluxes) 

# Explained variance...
# table with explained variance from each variable in the new full model + the overlabs

#Explained by only one group
GEP_env <- summary(fm.gep.env2)$adj.r.s
GEP_trt <- summary(fm.gep.trt2)$adj.r.s
GEP_fgr <- summary(fm.gep.fgr2)$adj.r.s
NEE_env <- summary(fm.nee.env2)$adj.r.s
NEE_trt <- summary(fm.nee.trt2)$adj.r.s
ER_env <- summary(fm.er.env2)$adj.r.s
ER_trt <- summary(fm.er.trt2)$adj.r.s

#Explained by two groups
GEP_env_trt2 <- summary(GEP_env_trt)$adj.r.s
GEP_env_fgr2 <- summary(GEP_env_fgr)$adj.r.s
GEP_trt_fgr2 <- summary(GEP_trt_fgr)$adj.r.s
NEE_total <- summary(NEE_full)$adj.r.s
ER_total <- summary(ER_full)$adj.r.s


