###Generate CWM bootstrapping results
library(tidyverse)
#load community data and make it skinny and remove equisetum
CommunitySV_ITEX_2003_2015 <- read.csv("community/cleaned_data/ITEX_Svalbard_2003_2015_Community_cleaned.csv")

comm <- CommunitySV_ITEX_2003_2015 %>%
  filter(FunctionalGroup != "lichen", FunctionalGroup != "moss", FunctionalGroup != "liverwort", FunctionalGroup != "fungi") %>% 
  select(-Spp, -FunctionalGroup) %>% 
  filter(Taxon != "equisetum arvense", Taxon != "equisetum scirpoides") %>% 
  filter(Year == 2015) %>% 
  mutate(Site_trt = paste0(Site, Treatment))

#load trait data and make it skinny
#load("traits/data/traitsITEX_SV_2018.Rdata")

traitsITEX_SV_2018 <- read.csv("traits/data/PFTC4_Svalbard_2018_ITEX.csv")

trait.pca <- traitsITEX_SV_2018 %>% 
  select(-Country, -Year, -Project, -Latitude_N, -Longitude_E, -Elevation_m, -Genus, -Species, -ID, -Date, -Individual_nr, -Wet_Mass_g, -Wet_Mass_Total_g, -Dry_Mass_Total_g, -Leaf_Area_Total_cm2, -Leaf_Thickness_1_mm, -Leaf_Thickness_2_mm, -Leaf_Thickness_3_mm,  -NrLeaves, -Bulk_nr_leaves, -NumberLeavesScan, -Comment, -Data_entered_by, -Flag, -Batch, -CoeffVarP_Corrected, -N_replications, -Flag_corrected, -filename, -Samples_Nr, -Row, -Column, -Remark_CN, -sdP_Corrected) 

#pca_res <- prcomp(trait.pca %>% select(-Taxon, -Site, -PlotID, -Treatment) %>% na.omit() , center = T, scale. = T)
  
  
trait <- trait.pca %>% 
  gather(key = Trait, value = Value, -PlotID, -Site, -Taxon, -Treatment) %>%
  mutate(Site_trt = paste0(Site, Treatment)) %>% 
  filter(!is.na(Value))

library(nlme)
library(ape)

var_res <- data.frame()
for(i in unique(trait$Trait)){
v <- varcomp(lme(Value~1, random=~1|Taxon, data=trait %>% filter(Trait == i), na.action = na.omit), 1)[c(1,2)] 

v$trait <- i

v <- unlist(v)

var_res <- bind_rows(var_res, v)

}


write.table(var_res, file = "varpart_results.csv")

trait.null <- trait.pca %>% 
  gather(key = Trait, value = Value, -PlotID, -Site, -Taxon, -Treatment) %>% 
  filter(Treatment == "CTL") %>% 
  group_by(Taxon, Trait) %>% 
  summarize(Value = mean(as.numeric(Value), na.rm = T)) %>% 
  right_join(trait.pca) %>% 
  select(Taxon:PlotID) %>%  
mutate(Site_trt = paste0(Site, Treatment)) %>% 
  filter(!is.na(Value))

library(traitstrap)

trait_imp <- trait_impute(comm = comm, 
                          traits = trait, 
                          scale_hierarchy = c("Site", "Site_trt", "PlotID"),
                          global = F,
                          taxon_col = "Taxon",
                          trait_col = "Trait",
                          value_col = "Value",
                          abundance_col = "Abundance"
                          )

trait_imp_null <- trait_impute(comm = comm, 
                          traits = trait.null, 
                          scale_hierarchy = c("Site", "Site_trt", "PlotID"),
                          global = F,
                          taxon_col = "Taxon",
                          trait_col = "Trait",
                          value_col = "Value",
                          abundance_col = "Abundance"
)


trait_imp %>% 
  #filter(Trait == "C_percent") %>% 
  autoplot(.) +
  theme(axis.text.x = element_text(angle = 90))

trait_imp_null %>% 
  filter(Trait == "C_percent") %>% 
  autoplot(.) +
  theme(axis.text.x = element_text(angle = 90))

CWM <- trait_np_bootstrap(trait_imp, nrep = 100, sample_size = 200)
CWM_notiv <- trait_np_bootstrap(trait_imp_null, nrep = 100, sample_size = 200)

CWM_mean <- trait_summarise_boot_moments(CWM) %>% 
  select(Site:mean) 

CWM_notiv_mean <- trait_summarise_boot_moments(CWM_notiv) %>% 
  select(Site:mean) %>% 
  rename("mean_noitv" = "mean")

traitMean1 <- CWM_mean %>% 
  left_join(CWM_notiv_mean) %>% 
  select(-n) %>% 
  mutate(Year = 2015)

save(traitMean, file = "traits/cleaned_data/traitMean.RData")

# CWM1 <- test_b(trait_imp, nrep = 100, sample_size = 200)
# 
# test_b <- function (imputed_traits, nrep = 100, sample_size = 200) 
# {
#   attrib <- attr(imputed_traits, "attrib")
#   value_col <- attrib$value_col
#   bootstrapMoments <- map_df(1:nrep, ~{
#     sample_n(imputed_traits, size = sample_size, replace = TRUE, 
#              weight = weight) %>% summarise_at(.vars = vars(one_of(value_col)), 
#                                                .funs = list(mean = mean, variance = var, skewness = skewness, kurtosis = kurtosis), na.rm = T)
#   }, .id = "n")
#   attr(bootstrapMoments, "attrib") <- attrib
#   class(bootstrapMoments) <- class(bootstrapMoments)[!class(bootstrapMoments) == 
#                                                        "imputed_trait"]
#   return(bootstrapMoments)
# }


#dat <- list(community = comm, trait_trans = trait)
#dat.null <- list(community = comm, trait_trans = trait.null)

#ITEX_CWM <- CWM_Bootstrapping(dat, nrep = 200, samplesize = 100)
#ITEX_CWM_noitv <- CWM_Bootstrapping(dat.null, nrep = 200, samplesize = 100)

#traitMean_noitv <- ITEX_CWM_noitv %>% 
#   group_by(PlotID, Trait, Year) %>% 
#   summarize(mean_noitv = mean(Mean))
# 
# traitMean <- ITEX_CWM %>% 
#   group_by(PlotID, Trait, Year) %>% 
#   summarize(mean = mean(Mean)) %>% 
#   left_join(traitMean_noitv) %>% 
#   left_join(metaItex)




plot(traitMean1$mean, traitMean$mean)
table(traitMean1$PlotID)
table(traitMean$PlotID)
unique(traitMean$Trait)
unique(traitMean1$Trait)
