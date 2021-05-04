#### COMMUNITY WEIGHTED MEANS ####

# Libraries
# install.packages("devtools")
#devtools::install_github("richardjtelford/traitstrap")
library("traitstrap")
library("tidyverse")

comm <- read_csv(file = "community/cleaned_data/ITEX_Svalbard_2003_2015_Community_cleaned.csv", col_names = TRUE)
traits <- read_csv(file = "traits/cleaned_Data/PFTC4_Svalbard_2018_ITEX.csv", col_names = TRUE)

# Transform data sets
comm <- comm %>% 
  filter(Year == 2015) %>% 
  select(-Spp, -FunctionalGroup) %>% 
  select(-Year)

traits <- traits %>% 
  # remove 3 Betula nana ind.
  filter(Taxon != "betula nana") %>% 
  # select important columns
  select(Site, Treatment, PlotID, Taxon, Individual_nr:LDMC, P_percent, C_percent:dC13_permil) %>% #, ID, Flag) %>% 
  # Make long data frame
  pivot_longer(cols = c(Individual_nr:dC13_permil), names_to = "Trait", values_to = "Value") %>% 
  filter(!is.na(Value)) %>% 
  # log transform growth traits
  mutate(Value = ifelse(Trait %in% c("Plant_Height_cm", "Wet_Mass_g", "Dry_Mass_g", "Leaf_Area_cm2", "Leaf_Thickness_Ave_mm"), suppressWarnings(log(Value)), Value),
       Trait = recode(Trait, "Plant_Height_cm" = "Plant_Height_cm_log", "Wet_Mass_g" = "Wet_Mass_g_log", "Dry_Mass_g" = "Dry_Mass_g_log", "Leaf_Area_cm2" = "Leaf_Area_cm2_log", "Leaf_Thickness_Ave_mm" = "Leaf_Thickness_Ave_mm_log"))


# Impute missing traits
ImputedTraits_15 <- trait_impute(comm = comm,
             traits = traits, 
             scale_hierarchy = c("Site", "Treatment", "PlotID"), 
             taxon_col = "Taxon", 
             trait_col = "Trait", 
             value_col = "Value", 
             abundance_col = "Abundance")

# Coverage Plot
autoplot(ImputedTraits_15)

# Boostrapping
BootstrapedTraits <- trait_np_bootstrap(imputed_traits = ImputedTraits_15, 
                   nrep = 100, sample_size = 200)

SumMoments <- SummariseBootMoments(BootstrapMoments = BootstrapedTraits)


