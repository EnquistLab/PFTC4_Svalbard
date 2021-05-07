#load data
source("ITEX_analyses/2_Import_data.R")

comm <- CommunitySV_ITEX_2003_2015 %>%
  filter(FunctionalGroup != "lichen", FunctionalGroup != "moss", FunctionalGroup != "liverwort", FunctionalGroup != "fungi") %>% 
  select(-FunctionalGroup) %>% 
  filter(Taxon != "equisetum arvense", Taxon != "equisetum scirpoides") %>% 
  filter(Year == 2015) %>% 
  mutate(Site_trt = paste0(Site, Treatment))

#load trait data and make it skinny
#load("traits/data/traitsITEX_SV_2018.Rdata")

trait <- Svalbard_2018_ITEX_Traits %>% 
  select(Treatment, Site, PlotID, Taxon, Trait, Value) %>% 
  filter(!is.na(Value)) %>% 
  mutate(Site_trt = paste0(Site, Treatment)) %>%  
  mutate(Site = factor(Site, levels = c("SB", "CH", "DH"))) %>% 
  filter(Trait != "Wet_Mass_g")


trait.null <- Svalbard_2018_ITEX_Traits %>% 
  select(Treatment, Site, PlotID, Taxon, Trait, Value) %>%   
  filter(!is.na(Value)) %>% 
  filter(Treatment == "CTL") %>% 
  filter(Trait != "Wet_Mass_g") %>% 
  group_by(Taxon, Trait) %>% 
  summarize(Value = mean(as.numeric(Value), na.rm = T)) %>% 
  right_join(trait, by = c("Taxon", "Trait")) %>% 
  select(-Value.y, "Value" = Value.x) %>%  
  mutate(Site = factor(Site, levels = c("SB", "CH", "DH"))) 

set.seed(2525)
trait_imp <- trait_impute(comm = comm, 
                          traits = trait, 
                          scale_hierarchy = c("Site", "Site_trt", "PlotID"),
                          global = F,
                          taxon_col = "Taxon",
                          trait_col = "Trait",
                          value_col = "Value",
                          abundance_col = "Abundance",
                          min_n_in_sample = 2
                          )

trait_imp_null <- trait_impute(comm = comm, 
                          traits = trait.null, 
                          scale_hierarchy = c("Site", "Site_trt", "PlotID"),
                          global = F,
                          taxon_col = "Taxon",
                          trait_col = "Trait",
                          value_col = "Value",
                          abundance_col = "Abundance",
                          min_n_in_sample = 2
)


trait_imp %>% 
  #filter(Trait == "C_percent") %>% 
  autoplot(.) +
  theme(axis.text.x = element_text(angle = 90))

trait_imp_null %>% 
  autoplot(.) +
  theme(axis.text.x = element_text(angle = 90))

CWM <- trait_np_bootstrap(trait_imp, nrep = 100, sample_size = 200)
CWM_notiv <- trait_np_bootstrap(trait_imp_null, nrep = 100, sample_size = 200)

CWM_mean <- trait_summarise_boot_moments(CWM) %>% 
  select(Site:mean) 

CWM_notiv_mean <- trait_summarise_boot_moments(CWM_notiv) %>% 
  select(Site:mean) %>% 
  rename("mean_noitv" = "mean")

traitMean4 <- CWM_mean %>% 
  left_join(CWM_notiv_mean) %>% 
  select(-n) %>% 
  mutate(Year = 2015)

load("traits/cleaned_data/traitMean.rdata")



test <- traitMean4 %>% 
  left_join(traitMean, by = c("PlotID", "Trait"))

plot(test$mean.x, test$mean.y)
abline(0,1)
plot(test$mean_noitv.x, test$mean_noitv.y)



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
