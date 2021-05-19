# Flux Analyses

# Join dark and light measurements, and calculate GPP

calc_GPP <- function(ITEX.data.pre.calcs){
  # Dark
  ITEX.data.post.calcs <- ITEX.data.pre.calcs %>% 
    filter(Type == "D") %>% 
    # Light
    inner_join(ITEX.data.pre.calcs %>% 
                filter(Type == "L"), by = c("PlotID", "Treatment", "Date"),
              suffix = c("_Dark", "_Light")) %>% 
    # NEE = Light and ER = Dark
    rename(NEE_ln = nee_lm_Light,
           ER_ln = nee_lm_Dark,
           SoilTemp = SoilTemp_Dark,
           SoilMoist = SoilMoist_Dark,
           PAR = PAR_Light) %>% 
    # GPP = NEE - ER
    mutate(GEP_ln = NEE_ln - ER_ln) %>% 
    # fix Site and PlotID
    mutate(Site = substr(PlotID, 1,3),
           Site = case_when(Site == "BIS" ~ "SB",
                            Site == "CAS" ~ "CH",
                            Site == "DRY" ~ "DH",
                            TRUE ~ Site),
           PlotID = gsub("L", "", PlotID),
           PlotID = str_replace(PlotID, "BIS", "SB"),
           PlotID = str_replace(PlotID, "CAS", "CH"),
           PlotID = str_replace(PlotID, "DRY", "DH")) %>% 
    # Reco and GPP700 missing in this data set. What are they?
    select(Date, Site, PlotID, Treatment, NEE_ln, ER_ln, GEP_ln, SoilTemp, SoilMoist, CanTemp_Light, CanTemp_Dark, PAR) %>% 
    ## Remove positive Reco values and negative GEP values (only 8 negative GEP values)
    filter(!is.na(NEE_ln) & GEP_ln > 0 & ER_ln < 0) %>% 
    # remove iced Cassiope plots
    filter(!PlotID %in% c("CH-4", "CH-6", "CH-9", "CH-10")) %>% 
    # Summarize values for multiple measurements (DH-3 and DH-5)
    group_by(Date, Site, PlotID, Treatment) %>% 
    summarise(NEE_ln = mean(NEE_ln),
              ER_ln = mean(ER_ln),
              GEP_ln = mean(GEP_ln),
              SoilTemp = mean(SoilTemp),
              SoilMoist = mean(SoilMoist),
              CanTemp_Light = mean(CanTemp_Light, na.rm = TRUE),
              CanTemp_Dark = mean(CanTemp_Dark, na.rm = TRUE),
              PAR = mean(PAR))
  
  return(ITEX.data.post.calcs)
}



####### Load plant community data #########
#Data with plant community, traits and fluxes. Do not use flux data, but plant data are updated and ready to use

#comm_resp <- CommResp
#height <- Height
standardize_fluxes <- function(comm_resp, height, ITEX.data.post.calcs){
  
  # Join sum abundance and height, and claclualte biomass proxy
  biomass <- comm_resp %>% 
    filter(Year == 2015) %>% 
    select(Year:PlotID, sumAbundance, Richness, Evenness, Diversity, propGraminoid:propBryo) %>% 
    mutate(Graminoid = sumAbundance * propGraminoid,
           Forb = sumAbundance * propForb,
           Bryophyte = sumAbundance * propBryo,
           Lichen = sumAbundance * propLichen,
           Shrub = sumAbundance * propShrub,
           Evergreen = sumAbundance * propEShrub,
           Decidious = sumAbundance * propDShrub) %>% 
    select(-c(propGraminoid:propDShrub)) %>% 
    left_join(height %>% 
                group_by(Site, Treatment, PlotID) %>% 
                summarise(Height_cm = mean(Value)), 
              by = c("Site", "Treatment", "PlotID")) %>% 
    mutate(Biomass_Proxy = Height_cm * sumAbundance)
  
  
  ITEX.biomass.flux <- ITEX.data.post.calcs %>% 
    left_join(biomass, by = c("Site", "PlotID", "Treatment"))
  
  
# # elegant solution
#   ITEX.biomass.flux %>% 
#     group_by(Site) %>% 
#     nest(data = -Site) %>% 
#     mutate(
#       
#       # fit exponential curve for respiration measurements per habitat (Loyd et al 1994)
#       fit.Reco = map(data, ~nls((ER_ln~A*exp(-308.56/I(SoilTemp-227.13))), start = c(A = 0), data = .x)),
#       
#       #Recalculating Reco to 15C values taking in account heteroscedasticity 
#       Reco15 = map(data, ~.x$ER_ln/fitted(fit.Reco)*(coef(fit.Reco)*exp(-308.56/I(288.15-227.13)))),
#       
#       # fit rectangular hyperbolic saturation curve to GPP measurements (Thornley & Johnson, 1990)
#       fit.GPP = map(data, ~nls((GEP_ln~ (A*B*PAR)/(A*PAR+B)), start = c(A = 0.01, B = 2), data = .x)),
#       
#       #Recalculating GPP values taking in account heteroscedasticity 
#       GPP700 = map(data, ~.x$GEP_ln/fitted(fit.GPP)*(((coef(fit.GPP)[1])*(coef(fit.GPP)[2])*1200)/((coef(fit.GPP)[1])*700 + (coef(fit.GPP)[2]))))
#       )
    
  
  #### Standization of fluxes #####
  ## per Habitat = ITEX_site BIS, CAS, DRY
  ITEX.fluxes.BIS<- ITEX.biomass.flux %>%  filter(Site == "SB" )
  ITEX.fluxes.CAS<- ITEX.biomass.flux %>%  filter(Site == "CH" )
  ITEX.fluxes.DRY<- ITEX.biomass.flux %>%  filter(Site == "DH" )
  
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
  ITEX.mean.fluxes <- ITEX.fluxes.finished.standard %>%
    group_by(PlotID, Treatment, Site) %>%
    summarise_if(is.numeric, mean, na.rm = TRUE)
  
  return(ITEX.mean.fluxes)
  
}



# Combine Flux and Trait data
#traitMean <- Trait_Mean
flux_and_trait <- function(traitMean, ITEX.mean.fluxes){

  ITEX.Trait_Fluxes <- traitMean %>%
    rename(ITV = mean,
           No_ITV = mean_noitv) %>%
    filter(!Trait %in% c("dC13_percent", "dN15_percent", "Dry_Mass_g")) %>%
    mutate(Trait = recode(Trait, C_percent = "C",
                          N_percent = "N",
                          CN_ratio = "CN",
                          P_Ave = "P",
                          Leaf_Area_cm2 = "LA",
                          Leaf_Thickness_mm = "LT",
                          SLA_cm2_g = "SLA",
                          Plant_Height_cm = "Height")) %>%
    pivot_wider(names_from = Trait, values_from = c(ITV, No_ITV)) %>%
    select(-Year, -Site_trt) %>%
    left_join(ITEX.mean.fluxes, by = c("Site", "PlotID", "Treatment"))
  
  return(ITEX.Trait_Fluxes)

}





#### difference between trait effects on Cflux when taking into account ITV or not
# z2 <- Flux_and_Traits %>%
#   select(Site, PlotID, Treatment, NEE_ln, ER_ln, GPP700, ITV_C:No_ITV_SLA) %>%
#   rename(NEE = NEE_ln, GPP = GPP700, Reco = ER_ln) %>%
#   group_by(Site, Treatment) %>%
#   gather(key = Trait, value = Tvalue, ITV_C:No_ITV_SLA) %>%
#   mutate(Trait = str_replace(Trait, "No_ITV", "NoITV")) %>% 
#   separate(Trait, c("ITV", "Trait"), sep = "_") %>%
#   gather(key = response, value = Cflux, -c(Site, PlotID, Treatment, ITV, Trait, Tvalue)) %>%
#   group_by(response, Trait, ITV) %>%
#   mutate(Tvalue = scale(Tvalue)[,1]) %>%
#   do(tidy(lm(Cflux ~ Tvalue , data=. ))) %>%   # not sure if Site should be in here
#   filter(!term == "(Intercept)") %>% 
#   mutate(lower = (estimate - std.error*1.96),
#          upper = (estimate + std.error*1.96))
# 
# 
# #postscript(file = "O:\\FunCab\\Manuscripts\\CO2trait\\figures\\Figure1.eps", width = 8.3, height = 5.8)
# ggplot(z2, aes(x =Trait, y = estimate, shape = response, 
#                col = ITV, ymin = lower, ymax = upper)) +
#   geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
#   geom_hline(yintercept = 0, linetype = "solid", color="grey") +
#   #geom_vline(xintercept = c(8.5, 16.5, 22.5), linetype = "dotted", size =1) +
#   geom_point(position = position_dodge(width = 0.5), size = 3) +
#   #scale_shape_manual(labels = c("NEE", "GPP", "Reco"), values = c(22, 24,21)) + 
#   #scale_fill_manual(labels = c("NEE", "GPP", "Reco"), values = c("red", "grey70", "black")) + 
#   #scale_x_discrete(limits=c( "Wvar_CN", "Wvar_N", "Wvar_C", "Wvar_VH", "Wvar_SLA", "Wvar_LDMC", "Wvar_Lth", "Wvar_LA", "CWM_CN", "CWM_N", "CWM_C", "CWM_VH", "CWM_SLA", "CWM_LDMC", "CWM_Lth", "CWM_LA", "Richness", "Evenness", "Diversity", "VegetationHeight", "Temp.C", "P.mm"), labels=c( "Wvar_CN" = "LCNvar", "Wvar_N" = "LNvar", "Wvar_C"= "LCvar", "Wvar_Height"= "Hvar", "Wvar_SLA" = "SLAvar", "Wvar_LDMC" = "LDMCvar", "Wvar_Lth" = "LTvar", "Wvar_LA" = "LAvar", "Wmean_CN" = "LCNmean", "Wmean_N" = "LNmean", "Wmean_C"= "LCmean", "Wmean_Height"= "Hmean", "Wmean_SLA" = "SLAmean", "Wmean_LDMC" = "LDMCmean", "Wmean_Lth" = "LTmean", "Wmean_LA" = "LAmean", "VegetationHeight" = "Vegetation Height", "Temp.C" = "Temperature", "P.mm" = "Precipitation"))+
#   #labs(x= "             CWV traits                CWM traits             Veg structure      Climate")+
#   facet_grid(~response)+ #, labeller=labeller(response = labels)
#   theme(axis.title.x=element_text(size = 12), axis.text.x=element_text(size = 12), axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.title.y=element_text(size = 12), strip.background = element_rect(colour="black", fill="white"), panel.background = element_rect(fill= "white"), panel.border = element_rect(colour = "black", fill=NA), strip.text.x = element_text(size=12, face="bold"),  axis.line = element_line(colour = "black"))+
#   coord_flip()




#### MuMin Model selection + VIF ####
# stepAIC function from MASS package, but should not load, messes with dplyr


#ITEX.Trait_Fluxes <- Flux_and_Traits


trait_model_selelction <- function(ITEX.Trait_Fluxes){
  
  ITEX.mean.fluxes <- ITEX.Trait_Fluxes %>% 
    rename(CWM_C = ITV_C,
           CWM_CN = ITV_CN, 
           CWM_LDMC = ITV_LDMC,
           CWM_LA = ITV_LA,
           CWM_LT = ITV_LT,
           CWM_N = ITV_N,
           CWM_P = ITV_P,
           CWM_Height = ITV_Height,
           CWM_SLA = ITV_SLA,
           
           noitv_C = No_ITV_C,
           noitv_CN = No_ITV_CN, 
           noitv_LDMC = No_ITV_LDMC,
           noitv_LA = No_ITV_LA,
           noitv_LT = No_ITV_LT,
           noitv_N = No_ITV_N,
           noitv_P = No_ITV_P,
           noitv_Height = No_ITV_Height,
           noitv_SLA = No_ITV_SLA,
           
           gram = Graminoid,
           forb = Forb,
           bryo = Bryophyte,
           eshrub = Evergreen,
           dshrub = Decidious,
           lichen = Lichen
    )
  
  GPP.CWM <- summary(lm(GPP700 ~  CWM_N , data = ITEX.mean.fluxes))
  GPP.noitv <- summary(lm(GPP700 ~  noitv_N, data = ITEX.mean.fluxes))
  
  R.CWM <- summary(lm(ER_ln ~  CWM_N + CWM_Height + CWM_LA, data = ITEX.mean.fluxes))
  R.noitv <- summary(lm(ER_ln ~  noitv_N + noitv_Height + noitv_LA, data = ITEX.mean.fluxes))
  
  NEE.CWM <- summary(lm(NEE_ln ~  CWM_Height + CWM_LA, data = ITEX.mean.fluxes))
  NEE.noitv <- summary(lm(NEE_ln ~ noitv_Height + noitv_LA, data = ITEX.mean.fluxes))
  
  # ITV vs no ITV
  CWM_ITV <- tibble(Cflux = c("GPP", "Reco", "NEE"),
                    noITV = c(GPP.noitv$adj.r.s, R.noitv$adj.r.s, NEE.noitv$adj.r.s),
                    ITV = c(GPP.CWM$adj.r.s, R.CWM$adj.r.s, NEE.CWM$adj.r.s)) %>%
    mutate(Cflux = factor(Cflux, levels = c("GPP", "Reco", "NEE")))
  
  return(CWM_ITV)
  
}

model_selection <- function(ITEX.Trait_Fluxes){
  
  # prep data
  ITEX.mean.fluxes <- ITEX.Trait_Fluxes %>% 
    rename(CWM_C = ITV_C,
           CWM_CN = ITV_CN, 
           CWM_LDMC = ITV_LDMC,
           CWM_LA = ITV_LA,
           CWM_LT = ITV_LT,
           CWM_N = ITV_N,
           CWM_P = ITV_P,
           CWM_Height = ITV_Height,
           CWM_SLA = ITV_SLA,
           
           noitv_C = No_ITV_C,
           noitv_CN = No_ITV_CN, 
           noitv_LDMC = No_ITV_LDMC,
           noitv_LA = No_ITV_LA,
           noitv_LT = No_ITV_LT,
           noitv_N = No_ITV_N,
           noitv_P = No_ITV_P,
           noitv_Height = No_ITV_Height,
           noitv_SLA = No_ITV_SLA,
           
           gram = Graminoid,
           forb = Forb,
           bryo = Bryophyte,
           eshrub = Evergreen,
           dshrub = Decidious,
           lichen = Lichen
    )
 
  
  #### GPP700 models ####
  # Environment
  GPP_env <- lm(GPP700 ~ CanTemp_Light, data = ITEX.mean.fluxes)
  step <- MASS::stepAIC(GPP_env, direction = "backward")
  GPP.env <- summary(lm(GPP700 ~ CanTemp_Light, data = ITEX.mean.fluxes))
  
  # Community
  GPP_com<- lm(GPP700 ~ Richness + Evenness + Diversity + Height_cm + gram + forb + bryo + eshrub + dshrub+ lichen, data = ITEX.mean.fluxes)
  step <- MASS::stepAIC(GPP_com, direction = "backward")
  GPP.com<-summary(lm(GPP700 ~ Richness + Height_cm, data = ITEX.mean.fluxes))
  
  # Traits
  GPP_traitCWM <- lm(GPP700 ~ CWM_N + CWM_CN + CWM_P + CWM_SLA + CWM_LDMC + CWM_LT + CWM_LA + CWM_Height, data = ITEX.mean.fluxes)
  step <- MASS::stepAIC(GPP_traitCWM, direction = "backward")
  GPP.CWM <- summary(lm(GPP700 ~  CWM_N , data = ITEX.mean.fluxes)) # N (CN before)
  
  # Traits No ITV, same predictors as with CWM to compare influence of ITV
  GPP_traitNoITV <- lm(GPP700 ~ noitv_N + noitv_CN + noitv_P + noitv_SLA + noitv_LDMC + noitv_LT + noitv_LA + noitv_Height, data = ITEX.mean.fluxes)
  step <- MASS::stepAIC(GPP_traitNoITV, direction = "backward")
  GPP.noitv <- summary(lm(GPP700 ~  noitv_N, data = ITEX.mean.fluxes)) # N, SLA, LT (CN before)
  
  # combination
  GPP.env.com <- summary(lm(GPP700 ~ Richness + Height_cm + CanTemp_Light, data = ITEX.mean.fluxes)) 
  GPP.env.CWM <- summary(lm(GPP700 ~ CanTemp_Light + CWM_N, data = ITEX.mean.fluxes)) 
  GPP.com.CWM <- summary(lm(GPP700 ~ Richness + Height_cm + CWM_N, data = ITEX.mean.fluxes)) 
  GPP.env.com.CWM <- summary(lm(GPP700 ~ CanTemp_Light + Richness + Height_cm + CWM_N, data = ITEX.mean.fluxes)) 
  
  GPP.env.noitv <- summary(lm(GPP700 ~ CanTemp_Light + noitv_N, data = ITEX.mean.fluxes))
  GPP.com.noitv <- summary(lm(GPP700 ~ Richness + Height_cm + noitv_N, data = ITEX.mean.fluxes))
  GPP.env.com.noitv <- summary(lm(GPP700 ~ CanTemp_Light + Richness + Height_cm + noitv_N, data = ITEX.mean.fluxes)) 
  
  
  #### Reco models ####
  # Environment
  R_env <- lm(ER_ln ~  CanTemp_Light, data = ITEX.mean.fluxes)
  step <- MASS::stepAIC(R_env, direction = "backward")
  R.env <- summary(lm(ER_ln ~  CanTemp_Light, data = ITEX.mean.fluxes))
  
  # Community
  R_com <- lm(ER_ln ~ Richness + Evenness + Diversity + Height_cm + gram + forb + bryo + eshrub + dshrub+ lichen, data = ITEX.mean.fluxes)
  step <- MASS::stepAIC(R_com, direction = "backward")
  R.com <- summary(lm(ER_ln ~ Richness + Diversity + bryo , data = ITEX.mean.fluxes))
  
  # Traits 
  R_CWM <- lm(ER_ln ~ CWM_N + CWM_CN + CWM_P + CWM_SLA + CWM_LDMC + CWM_LT + CWM_LA + CWM_Height, data = ITEX.mean.fluxes)
  step <- MASS::stepAIC(R_CWM, direction = "backward")
  R.CWM <- summary(lm(ER_ln ~  CWM_N + CWM_Height + CWM_LA, data = ITEX.mean.fluxes)) # (CN instead of N before)
  
  # Traits No ITV, same predictors as with CWM to compare influence of ITV
  R_NoITV <- lm(ER_ln ~ noitv_N + noitv_CN + noitv_P + noitv_SLA + noitv_LDMC + noitv_LT + noitv_LA + noitv_Height, data = ITEX.mean.fluxes)
  step <- MASS::stepAIC(R_NoITV, direction = "backward")
  R.noitv <- summary(lm(ER_ln ~  noitv_N + noitv_Height + noitv_LA, data = ITEX.mean.fluxes)) # (CN, Height and LA before)
  
  R.env.com <- summary(lm(ER_ln ~  CanTemp_Light + Richness + Diversity + bryo, data = ITEX.mean.fluxes)) 
  R.env.CWM <- summary(lm(ER_ln ~  CanTemp_Light + CWM_N + CWM_Height + CWM_LA, data = ITEX.mean.fluxes))
  R.com.CWM <- summary(lm(ER_ln ~  Richness + Diversity + bryo + CWM_N + CWM_Height + CWM_LA, data = ITEX.mean.fluxes))
  R.env.com.CWM <- summary(lm(ER_ln ~  CanTemp_Light + Richness + Diversity + bryo + CWM_N + CWM_Height + CWM_LA, data = ITEX.mean.fluxes))
  
  R.env.noitv <- summary(lm(ER_ln ~  CanTemp_Light + noitv_N + noitv_Height + noitv_LA, data = ITEX.mean.fluxes))
  R.com.noitv <- summary(lm(ER_ln ~  Richness + Diversity + bryo + noitv_N + noitv_Height + noitv_LA, data = ITEX.mean.fluxes)) 
  R.env.com.noitv <- summary(lm(ER_ln ~  CanTemp_Light+ Richness + Diversity + bryo + noitv_N + noitv_Height + noitv_LA, data = ITEX.mean.fluxes))
  
  #### NEE models ####
  # Environment
  NEE_env <- lm(NEE_ln ~  SoilTemp, data = ITEX.mean.fluxes)
  step <- MASS::stepAIC(NEE_env, direction = "backward")
  NEE.env <- summary(lm(NEE_ln ~  SoilTemp, data = ITEX.mean.fluxes))
  
  # Community
  NEE_com <- lm(NEE_ln ~ Richness + Evenness + Diversity + Height_cm + gram + forb + bryo + eshrub + dshrub + lichen, data = ITEX.mean.fluxes)
  step <- MASS::stepAIC(NEE_com, direction = "backward")
  NEE.com <- summary( lm(NEE_ln ~ Evenness, data = ITEX.mean.fluxes)) # (Height, Evenness before)
  
  # Traits 
  NEE_traitCWM <- lm(NEE_ln ~ CWM_N + CWM_CN + CWM_P + CWM_SLA + CWM_LDMC + CWM_LT + CWM_LA + CWM_Height, data = ITEX.mean.fluxes)
  step <- MASS::stepAIC(NEE_traitCWM, direction = "backward")
  NEE.CWM <- summary(lm(NEE_ln ~  CWM_Height + CWM_LA, data = ITEX.mean.fluxes)) # N has biggest effect as single predictor
  
  # Traits No ITV, same predictors as with CWM to compare influence of ITV
  NEE_traitNoITV <- lm(NEE_ln ~ noitv_N + noitv_CN + noitv_P + noitv_SLA + noitv_LDMC + noitv_LT + noitv_LA + noitv_Height, data = ITEX.mean.fluxes)
  step <- MASS::stepAIC(NEE_traitNoITV, direction = "backward")
  NEE.noitv <- summary(lm(NEE_ln ~ noitv_Height + noitv_LA, data = ITEX.mean.fluxes)) # N has biggest effect as single predictor
  # (Height and LA before)
  
  NEE.env.com <- summary(lm(NEE_ln ~  SoilTemp + Evenness, data = ITEX.mean.fluxes))
  NEE.env.CWM <- summary(lm(NEE_ln ~  SoilTemp + CWM_Height + CWM_LA, data = ITEX.mean.fluxes))
  NEE.com.CWM <- summary(lm(NEE_ln ~  Evenness + CWM_Height + CWM_LA, data = ITEX.mean.fluxes))
  NEE.env.com.CWM <- summary(lm(NEE_ln ~  SoilTemp + Evenness + CWM_Height + CWM_LA, data = ITEX.mean.fluxes))
  
  NEE.env.noitv <- summary(lm(NEE_ln ~  SoilTemp + noitv_Height + noitv_LA, data = ITEX.mean.fluxes))
  NEE.com.noitv <- summary(lm(NEE_ln ~  Evenness + noitv_Height + noitv_LA, data = ITEX.mean.fluxes))
  NEE.env.com.noitv <- summary(lm(NEE_ln ~  SoilTemp + Evenness + noitv_Height + noitv_LA, data = ITEX.mean.fluxes))
  
  
  # Variance Decomposition
  
  # ITV
  VarianceDecomp <- tibble(Variables = c("Env", "Com", "Trait", "Env.Com", "Env.Trait", "Com.Trait", "Env.Com.Trait"),
                           GPP = c(GPP.env$adj.r.s, GPP.com$adj.r.s, GPP.CWM$adj.r.s, GPP.env.com$adj.r.s, GPP.env.CWM$adj.r.s, GPP.com.CWM$adj.r.s, GPP.env.com.CWM$adj.r.s),
                           NEE = c(NEE.env$adj.r.s, NEE.com$adj.r.s, NEE.CWM$adj.r.s, NEE.env.com$adj.r.s, NEE.env.CWM$adj.r.s, NEE.com.CWM$adj.r.s, NEE.env.com.CWM$adj.r.s),
                           Reco = c(R.env$adj.r.s, R.com$adj.r.s, R.CWM$adj.r.s, R.env.com$adj.r.s, R.env.CWM$adj.r.s, R.com.CWM$adj.r.s, R.env.com.CWM$adj.r.s)) %>% 
    column_to_rownames(var = "Variables") %>% 
    t() %>% 
    as_tibble() %>% 
    mutate(unique.Env = Env.Com.Trait - Com.Trait,
           unique.Com = Env.Com.Trait - Env.Trait,
           unique.Trait = Env.Com.Trait - Env.Com,
           combined.Env.Com = Env.Trait - Trait - unique.Env,
           combined.Env.Trait = Com.Trait - Com - unique.Trait,
           combined.Com.Trait = Env.Com - Env - unique.Com,
           combined.Env.Com.Trait =  Env.Com.Trait - (unique.Env + unique.Com + unique.Trait + combined.Env.Com + combined.Env.Trait + combined.Com.Trait),
           Cflux = c("GPP", "NEE", "Reco")) %>%
    gather(key = Variables, value = value, unique.Env:combined.Env.Com.Trait) %>%
    select(Cflux, Variables, value) %>% 
    mutate(Variables = as.factor(Variables),
           Variables = factor(Variables, levels = c("unique.Env", "combined.Env.Com", "unique.Com", "combined.Env.Com.Trait", "combined.Com.Trait", "combined.Env.Trait", "unique.Trait")),
           Cflux = factor(Cflux, levels = c("GPP", "Reco", "NEE")))
  
  
  # no ITV
  VarianceDecomp2 <- tibble(Variables = c("Env", "Com", "Trait", "Env.Com", "Env.Trait", "Com.Trait", "Env.Com.Trait"),
                            GPP = c(GPP.env$adj.r.s, GPP.com$adj.r.s, GPP.noitv$adj.r.s, GPP.env.com$adj.r.s, GPP.env.noitv$adj.r.s, GPP.com.noitv$adj.r.s, GPP.env.com.noitv$adj.r.s),
                            NEE = c(NEE.env$adj.r.s, NEE.com$adj.r.s,NEE.noitv$adj.r.s, NEE.env.com$adj.r.s, NEE.env.noitv$adj.r.s, NEE.com.noitv$adj.r.s, NEE.env.com.noitv$adj.r.s),
                            Reco = c(R.env$adj.r.s, R.com$adj.r.s, R.noitv$adj.r.s, R.env.com$adj.r.s, R.env.noitv$adj.r.s, R.com.noitv$adj.r.s, R.env.com.noitv$adj.r.s)) %>% 
    column_to_rownames(var = "Variables") %>% 
    t() %>% 
    as_tibble() %>% 
    mutate(unique.Env = Env.Com.Trait - Com.Trait,
           unique.Com = Env.Com.Trait - Env.Trait,
           unique.Trait = Env.Com.Trait - Env.Com,
           combined.Env.Com = Env.Trait - Trait - unique.Env,
           combined.Env.Trait = Com.Trait - Com - unique.Trait,
           combined.Com.Trait = Env.Com - Env - unique.Com,
           combined.Env.Com.Trait =  Env.Com.Trait - (unique.Env + unique.Com + unique.Trait + combined.Env.Com + combined.Env.Trait + combined.Com.Trait),
           Cflux = c("GPP", "NEE", "Reco")) %>%
    gather(key = Variables, value = value, unique.Env:combined.Env.Com.Trait) %>%
    select(Cflux, Variables, value) %>% 
    mutate(Variables = as.factor(Variables),
           Variables = factor(Variables, levels = c("unique.Env", "combined.Env.Com", "unique.Com", "combined.Env.Com.Trait", "combined.Com.Trait", "combined.Env.Trait", "unique.Trait")),
           Cflux = factor(Cflux, levels = c("GPP", "Reco", "NEE")))
  
  
  model_selection_output <- bind_rows(ITV = VarianceDecomp,
            no_ITV = VarianceDecomp2,
            .id = "ITV")
  
  return(model_selection_output)
}








model_selection_results <- function(ITEX.Trait_Fluxes){
  
  # prep data
  ITEX.mean.fluxes <- ITEX.Trait_Fluxes %>% 
    rename(CWM_C = ITV_C,
           CWM_CN = ITV_CN, 
           CWM_LDMC = ITV_LDMC,
           CWM_LA = ITV_LA,
           CWM_LT = ITV_LT,
           CWM_N = ITV_N,
           CWM_P = ITV_P,
           CWM_Height = ITV_Height,
           CWM_SLA = ITV_SLA,
           
           noitv_C = No_ITV_C,
           noitv_CN = No_ITV_CN, 
           noitv_LDMC = No_ITV_LDMC,
           noitv_LA = No_ITV_LA,
           noitv_LT = No_ITV_LT,
           noitv_N = No_ITV_N,
           noitv_P = No_ITV_P,
           noitv_Height = No_ITV_Height,
           noitv_SLA = No_ITV_SLA,
           
           gram = Graminoid,
           forb = Forb,
           bryo = Bryophyte,
           eshrub = Evergreen,
           dshrub = Decidious,
           lichen = Lichen
    )
  
  
  #### GPP700 models ####
  # Environment
  GPP.env <- (lm(GPP700 ~ CanTemp_Light, data = ITEX.mean.fluxes))
  
  # Community
  GPP.com <- (lm(GPP700 ~ Richness + Height_cm, data = ITEX.mean.fluxes))
  
  # Traits
  GPP.CWM <- (lm(GPP700 ~  CWM_N , data = ITEX.mean.fluxes)) # N (CN before)
  
  # Traits No ITV, same predictors as with CWM to compare influence of ITV
  GPP.noitv <- (lm(GPP700 ~  noitv_N, data = ITEX.mean.fluxes))
  
  
  #### Reco models ####
  # Environment
  R.env <- summary(lm(ER_ln ~  CanTemp_Light, data = ITEX.mean.fluxes))
  
  # Community
  R.com <- summary(lm(ER_ln ~ Richness + Diversity + bryo , data = ITEX.mean.fluxes))
  
  # Traits 
  R.CWM <- summary(lm(ER_ln ~  CWM_N + CWM_Height + CWM_LA, data = ITEX.mean.fluxes)) # (CN instead of N before)
  
  # Traits No ITV, same predictors as with CWM to compare influence of ITV
  R.noitv <- summary(lm(ER_ln ~  noitv_N + noitv_Height + noitv_LA, data = ITEX.mean.fluxes))
  
  #### NEE models ####
  # Environment
  NEE.env <- summary(lm(NEE_ln ~  SoilTemp, data = ITEX.mean.fluxes))
  
  # Community
  NEE.com <- summary( lm(NEE_ln ~ Evenness, data = ITEX.mean.fluxes)) # (Height, Evenness before)
  
  # Traits 
  NEE.CWM <- summary(lm(NEE_ln ~  CWM_Height + CWM_LA, data = ITEX.mean.fluxes)) # N has biggest effect as single predictor
  
  # Traits No ITV, same predictors as with CWM to compare influence of ITV
  NEE.noitv <- summary(lm(NEE_ln ~ noitv_Height + noitv_LA, data = ITEX.mean.fluxes)) # N has biggest effect as single predictor
  
  
  tidy_model <- bind_rows(GPP_env = tidy(GPP.env),
            GPP_comm = tidy(GPP.com),
            GPP_ITV = tidy(GPP.CWM),
            GPP_NoITV = tidy(GPP.noitv),
            
            R_env = tidy(R.env),
            R_comm = tidy(R.com),
            R_ITV = tidy(R.CWM),
            R_NoITV = tidy(R.noitv),
            
            NEE_env = tidy(NEE.env),
            NEE_comm = tidy(NEE.com),
            NEE_ITV = tidy(NEE.CWM),
            NEE_NoITV = tidy(NEE.noitv),
            .id = "Flux_Model")
  
  
  glance_model <- bind_rows(GPP_env = glance(GPP.env),
            GPP_comm = glance(GPP.com),
            GPP_ITV = glance(GPP.CWM),
            GPP_NoITV = glance(GPP.noitv),
            
            R_env = glance(R.env),
            R_comm = glance(R.com),
            R_ITV = glance(R.CWM),
            R_NoITV = glance(R.noitv),
            
            NEE_env = glance(NEE.env),
            NEE_comm = glance(NEE.com),
            NEE_ITV = glance(NEE.CWM),
            NEE_NoITV = glance(NEE.noitv),
            .id = "Flux_Model")
  
  results_model_selection <- tidy_model %>% 
    left_join(glance_model, by = "Flux_Model", suffix = c(".tidy", ".glance")) %>% 
    separate(col = Flux_Model, into = c("Flux", "Model"), sep = "_") %>% 
    mutate(term = case_when(term == "(Intercept)" ~ "Intercept",
                            term == "CanTemp_Light" ~ "Canopy temperature",
                            term == "Height_cm" ~ "Canopy height",
                            term == "SoilTemp" ~ "Soil temperature",
                            term %in% c("CWM_N", "noitv_N") ~ "N",
                            term %in% c("CWM_Height", "noitv_Height") ~ "Plant height",
                            term %in% c("CWM_LA", "noitv_LA") ~ "Leaf area",
                            TRUE ~ term),
           Model = case_when(Model == "env" ~ "Environment",
                             Model == "comm" ~ "Community taxonomic structure",
                             Model == "ITV" ~ "ITV traits",
                             Model == "NoITV" ~ "No ITV traits",
                             TRUE ~ Model)) %>% 
    select(Flux, Model, Variable = term, R2adj = adj.r.squared, df, estimate, SE = std.error, p = p.value.tidy) %>% 
    mutate(R2adj = round(R2adj, 3),
           estimate = round(estimate, 3),
           SE = round(SE, 3),
           p = round(p, 3),
           p = case_when(p < 0.001 ~ paste("<0.001", "***"),
                                 p < 0.01 ~ paste(p, "**"),
                                 p < 0.05 ~ paste(p, "*"),
                                 p >= 0.05 ~ paste(p, ""))) %>% 
    rename("P value" = p)
  
  return(results_model_selection)
}

