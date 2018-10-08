# LOAD LIBRARIES
library("readxl")
library("tidyverse")
library("lubridate")
#devtools::install_github("gustavobio/tpldata")
#devtools::install_github("gustavobio/tpl")
library("tpl")
library("taxize")
library("googlesheets")

# Source ITEX (for Site-Elevation comninations)
#source(file = "community/ImportITEX.R")

pn <- . %>% print(n = Inf)


#### COORDINATES ####
coords <- read_excel(path = "Coordinates.xlsx", col_names = TRUE)



#### IMPORT TRAIT DATA ####

# Download data from google sheet

# Check which tables you have access to
gs_ls()
# which google sheets do you have access to?
trait <- gs_title("LeafTrait_Svalbard")
# list worksheets
gs_ws_ls(trait)
#download data
traits <- gs_read(ss = trait, ws = "Tabellenblatt1") %>% as.tibble() %>% select(-Dry_mass_g)

#import datasheet with dry masses entered
trait_dry_mass <- gs_title("LeafTrait_Svalbard (1)")
gs_ws_ls(trait_dry_mass)
traits_dry_mass <- gs_read(ss = trait_dry_mass, ws = "Tabellenblatt1") %>% 
  as.tibble() %>% 
# select(ID, Dry_mass_g) %>% 
  mutate(Dry_mass_g = gsub(",", "\\.", Dry_mass_g)) %>% 
  mutate(Dry_mass_g = as.numeric(Dry_mass_g))


#### DATA CHECKING ####
# Check LeafID
# Load trait IDs
load("traits/Rdatagathering/envelope_codes.Rdata", verbose = TRUE)
setdiff(traits$ID, all_codes$hashcode)


 # Check values
unique(traits$Day)
unique(traits$Site)
unique(traits$Elevation)

# Check combinations of Site and Elevation
table(traits$Site, traits$Elevation)

# Species and Genus
traits %>% distinct(Genus) %>% arrange(Genus) %>% pn
traits %>% distinct(Species) %>% arrange(Species) %>% pn
traits %>% 
  mutate(Taxon = paste(Genus, Species, sep = " ")) %>% 
  arrange(Taxon) %>% distinct(Taxon) %>% pn

# Plot name
unique(traits$Plot)
traits %>% filter(is.na(Plot)) %>% filter(Project != "Sean", Project != "M") # 15 without plot
table(traits$Plot, traits$Site)
traits %>% 
  filter(Site == "X") %>% 
  distinct(Plot, Elevation) %>% 
  arrange(Elevation, Plot) %>% pn

# Check all the itex plot names and habitat comninations
traits %>% 
  filter(Site == "X") %>% 
  anti_join(itex.codes, by = c("Elevation" = "Habitat", "Plot")) %>% 
  select(ID, Elevation, Genus, Species, Plot, Individual_nr, Remark)
itex.codes %>% pn


## FIX THE BUGS ##
# What was fixed and not fixed
# Cannot fix
# 3x stellaria longipes: plot and elev missing
# CCK4783 equisetum arvense: plot number missing, cannot fix
# CDW3071: It says Plot D or E; unlikely to be any of those, because there are already 3 of each plot, cannot fix
# CVP1261: It says Plot D or E; But no cover in D or E. Could be F, cannot fix
# CDX1924: remark is D or E; Already 3 E, and no cover in D, cannot fix
# AWN7480: no Plot on envelope. Could be 1-CTL or 5-OTC as those only have 2 ind. Cannot fix.
#AYU6804 oxyria digyna: Plot says 4, cannot fix

# Fixed
# BHG2119, BJH1430, BJG7192: assumed that these are Plot F, because cover is higher. Could also be B, but cover in B is only 0.5
# CAV5132: assume it is Plot A (only 2 ind. from Elevation 1 and Plot A)
# ADH9312 luzula nivalis: assumed C, Plot = 3, which is C and C ahs only 2 ind. Ind nr fits
# ALI6553: assumed Plot 5-OTC because only 2 ind form this plot and LeafID fits
# AJL5589: change from OTC to CTL. 5-OTC does not exist and only 2 leaves from 5-CTL
# ALO7062: Plot name was 2 and changed to 2-CTL


traits <- traits %>% 
  # Fix leafID
  mutate(ID = recode(ID, "CIG85099" = "CIG8509",
                     "CHF3’094" = "CHF3094")) %>% 
  
  mutate(ID = gsub("BMT5974", "BTM5974", ID),#
         ID = gsub("AZL0848", "BZL0848", ID),
         ID = gsub("BHS6740", "BHS6704", ID), #
         ID = gsub("BAH7471", "BAH7571", ID),
         ID = gsub("BDP43249", "BDP4329", ID),#
         ID = gsub("BUS0605", "BSU0605", ID),#
         ID = gsub("ADX3335", "ADK3335", ID),#
         ID = gsub("BTK0192", "BTK0182", ID),#
         ID = gsub("BJG192", "BJG7192", ID),
         ID = gsub("ACV501", "ACV0501", ID),#
         ID = gsub("AJJ5071", "AJJ5061", ID),#
         ID = gsub("AXX2436", "AAX2436", ID),
         ID = gsub("AWW8078", "AAW8078", ID),
         ID = gsub("AYP7268", "AYP7286", ID),
         ID = gsub("AQQ9548", "AQQ9458", ID),
         ID = gsub("CHF3’094", "CHF3094", ID),#
         ID = gsub("CIG850", "CIG8509", ID),#
         ID = gsub("CMP9385", "CMP9835", ID),
         ID = gsub("CIG85099", "CIG8509", ID)) %>% 
  
  #mutate(Date = dmy(paste(Day, "07-2018", sep = "-"))) %>% 
  mutate(Site = ifelse(Site == "x", "X", Site)) %>% 
  mutate(Site = ifelse(ID == "BWS2352", "X", Site)) %>% 
  mutate(Elevation = toupper(Elevation)) %>% 
  mutate(Elevation = ifelse(Elevation %in% c("3 OR 4", "3 - 4"), "3-4", Elevation)) %>% 
  mutate(Elevation = ifelse(ID == "BSE3271", 2, Elevation)) %>% 
  mutate(Project = ifelse(Project == "X", "T", Project)) %>% 
  
  # Correct species names
  # Genus
  mutate(Genus = tolower(Genus), Species = tolower(Species)) %>% 
  mutate(Genus = ifelse(Genus == "oxyra", "oxyria", Genus),
         Genus = ifelse(Genus == "bistota", "bistorta", Genus),
         Genus = ifelse(Genus == "equisteum", "equisetum", Genus),
         Genus = ifelse(Genus == "michranthes", "micranthes", Genus),
         Genus = ifelse(Genus == "ceratium", "cerastium", Genus),
         Genus = ifelse(Genus == "saxifraga/micranthus", "micranthes", Genus),
         Genus = ifelse(Genus == "stelleria", "stellaria", Genus),
         Genus = ifelse(Genus == "stelleria", "stellaria", Genus),
         Genus = ifelse(Genus == "racomitrium", "niphotrichum", Genus), # fixed because of tpl
         Genus = ifelse(Genus == "sanonia", "sanionia", Genus)) %>% 
  
  # Species
  mutate(Species = ifelse(Genus == "alopecurus", "ovatus", Species),
         Species = ifelse(Genus == "bistorta", "vivipara", Species),
         Species = ifelse(Genus == "cassiope", "tetragona", Species),
         Species = ifelse(Genus == "cerastium", "arcticum", Species),
         Species = ifelse(Genus == "equisteum", "equisetum", Species),
         ### poas
         Species = ifelse(Genus == "festuca" & Species == "vivipara", "viviparoidea", Species),
         Species = ifelse(Genus == "festuca" & Species %in% c("rubra_ssp_richardsonii", "rubra_spp_richardsonii"), "rubra", Species),
         Species = ifelse(Genus == "poa" & Species %in% c("arcitca", "arctcia", "arctica_var_vivipara", "arctica/pratensis vivipara"), "arctica", Species),
         Species = ifelse(Genus == "poa" & Species == "arctica/pratensis", "arctica", Species),
         Species = ifelse(Genus == "poa" & Species == "pratensis ssp. alpigena", "pratensis", Species),
         Species = ifelse(Genus == "ranunculus", "sulphureus", Species),
         Species = ifelse(Genus == "salix", "polaris", Species),
         Species = ifelse(Genus == "saxifraga" & Species == "herculus", "hirculus", Species),
         Species = ifelse(Genus == "saxifraga" & Species == "cernus", "cernua", Species),
         Species = ifelse(Genus == "saxifraga" & Species == "sernua", "cernua", Species),
         Species = ifelse(Genus == "saxifraga" & Species == "cerua", "cernua", Species),
         Species = ifelse(Genus == "saxifraga" & Species == "oppostifolia", "oppositifolia", Species),
         Species = ifelse(Genus == "aulacomnium" & Species == "turgidium", "turgidum", Species),
         Species = ifelse(Genus == "sanionia" & Species == "uni", "uncinata", Species)) %>% 
  
  # Fix wrong species
  mutate(Genus = ifelse(ID == "BAR1151", "salix", Genus)) %>% # typed in wrong, right on envelop
  mutate(Species = ifelse(ID == "BAR1151", "polaris", Species)) %>% 
  
  # Fix and check plot names
  mutate(Plot = gsub("_", "-", Plot)) %>% 
  mutate(Plot = ifelse(Plot == "L-1-CLT", "L-1-CTL", Plot)) %>% 
  mutate(Plot = ifelse(ID == "BDJ7423", "L-8-CTL", Plot)) %>% 
  mutate(Plot = ifelse(ID == "BTL8005", "L-1-CTL", Plot)) %>% 
  mutate(Plot = ifelse(ID %in% c("BHG2119", "BJH1430", "BJG7192"), "F", Plot)) %>% 
  mutate(Remark = ifelse(ID %in% c("BHG2119", "BJH1430", "BJG7192"), "assumed this is Plot F", Remark)) %>% 
  mutate(Plot = ifelse(ID == "CAV5132", "A", Plot)) %>% 
  mutate(Remark = ifelse(ID == "CAV5132", "assumed Plot A; Ind_nr_missing; height_missing", Remark)) %>%
  mutate(Plot = ifelse(ID == "ADH9312", "C", Plot)) %>% 
  mutate(Remark = ifelse(ID == "ADH9312", "assumed Plot C", Remark)) %>%
  mutate(Plot = ifelse(ID == "ALI6553", "L-5-OTC", Plot)) %>% 
  mutate(Remark = ifelse(ID == "ALI6553", "assumed Plot 5-OTC", Remark)) %>%
  mutate(Plot = ifelse(ID == "AJL5589", "L-5-CTL", Plot)) %>% 
  mutate(Remark = ifelse(ID == "AJL5589", "changed to Plot 5-CTL", Remark)) %>%
  mutate(Plot = ifelse(ID == "ALO7062", "L-2-CTL", Plot)) %>% 
  mutate(Remark = ifelse(ID == "ALO7062", "was 2 and changed to 2-CTL", Remark)) %>%
  mutate(Plot = ifelse(ID == "BVN8783", "L-5-CTL", Plot)) %>% # fix wrong name L5-CTL
  
  # Project
  mutate(Project = ifelse(ID == "AHE5823", "T", Project)) %>% 
  
  # Remove L- from Plot name for ITEX plants
  mutate(Plot = ifelse(Site == "X", substr(Plot, 3, nchar(Plot)), Plot)) %>% 
  
  mutate(Taxon = paste(Genus, Species, sep = " "))

#fix dry mass trait dataset
traits_dry_mass <- traits_dry_mass %>% 
  # Fix leafID
  mutate(ID = recode(ID, "CIG85099" = "CIG8509",
                     "CHF3’094" = "CHF3094")) %>% 
  
  mutate(ID = gsub("BMT5974", "BTM5974", ID),#
         ID = gsub("AZL0848", "BZL0848", ID),
         ID = gsub("BHS6740", "BHS6704", ID), #
         ID = gsub("BAH7471", "BAH7571", ID),
         ID = gsub("BDP43249", "BDP4329", ID),#
         ID = gsub("BUS0605", "BSU0605", ID),#
         ID = gsub("ADX3335", "ADK3335", ID),#
         ID = gsub("BTK0192", "BTK0182", ID),#
         ID = gsub("BJG192", "BJG7192", ID),
         ID = gsub("ACV501", "ACV0501", ID),#
         ID = gsub("AJJ5071", "AJJ5061", ID),#
         ID = gsub("AXX2436", "AAX2436", ID),
         ID = gsub("AWW8078", "AAW8078", ID),
         ID = gsub("AYP7268", "AYP7286", ID),
         ID = gsub("AQQ9548", "AQQ9458", ID),
         ID = gsub("CHF3’094", "CHF3094", ID),#
         ID = gsub("CIG850", "CIG8509", ID),#
         ID = gsub("CMP9385", "CMP9835", ID),
         ID = gsub("CIG85099", "CIG8509", ID)) %>% 
  
  #mutate(Date = dmy(paste(Day, "07-2018", sep = "-"))) %>% 
  mutate(Site = ifelse(Site == "x", "X", Site)) %>% 
  mutate(Site = ifelse(ID == "BWS2352", "X", Site)) %>% 
  mutate(Elevation = toupper(Elevation)) %>% 
  mutate(Elevation = ifelse(Elevation %in% c("3 OR 4", "3 - 4"), "3-4", Elevation)) %>% 
  mutate(Elevation = ifelse(ID == "BSE3271", 2, Elevation)) %>% 
  mutate(Project = ifelse(Project == "X", "T", Project)) %>% 
  
  # Correct species names
  # Genus
  mutate(Genus = tolower(Genus), Species = tolower(Species)) %>% 
  mutate(Genus = ifelse(Genus == "oxyra", "oxyria", Genus),
         Genus = ifelse(Genus == "bistota", "bistorta", Genus),
         Genus = ifelse(Genus == "equisteum", "equisetum", Genus),
         Genus = ifelse(Genus == "michranthes", "micranthes", Genus),
         Genus = ifelse(Genus == "ceratium", "cerastium", Genus),
         Genus = ifelse(Genus == "saxifraga/micranthus", "micranthes", Genus),
         Genus = ifelse(Genus == "stelleria", "stellaria", Genus),
         Genus = ifelse(Genus == "stelleria", "stellaria", Genus),
         Genus = ifelse(Genus == "racomitrium", "niphotrichum", Genus), # fixed because of tpl
         Genus = ifelse(Genus == "sanonia", "sanionia", Genus)) %>% 
  
  # Species
  mutate(Species = ifelse(Genus == "alopecurus", "ovatus", Species),
         Species = ifelse(Genus == "bistorta", "vivipara", Species),
         Species = ifelse(Genus == "cassiope", "tetragona", Species),
         Species = ifelse(Genus == "cerastium", "arcticum", Species),
         Species = ifelse(Genus == "equisteum", "equisetum", Species),
         ### poas
         Species = ifelse(Genus == "festuca" & Species == "vivipara", "viviparoidea", Species),
         Species = ifelse(Genus == "festuca" & Species %in% c("rubra_ssp_richardsonii", "rubra_spp_richardsonii"), "rubra", Species),
         Species = ifelse(Genus == "poa" & Species %in% c("arcitca", "arctcia", "arctica_var_vivipara", "arctica/pratensis vivipara"), "arctica", Species),
         Species = ifelse(Genus == "poa" & Species == "arctica/pratensis", "arctica", Species),
         Species = ifelse(Genus == "poa" & Species == "pratensis ssp. alpigena", "pratensis", Species),
         Species = ifelse(Genus == "ranunculus", "sulphureus", Species),
         Species = ifelse(Genus == "salix", "polaris", Species),
         Species = ifelse(Genus == "saxifraga" & Species == "herculus", "hirculus", Species),
         Species = ifelse(Genus == "saxifraga" & Species == "cernus", "cernua", Species),
         Species = ifelse(Genus == "saxifraga" & Species == "sernua", "cernua", Species),
         Species = ifelse(Genus == "saxifraga" & Species == "cerua", "cernua", Species),
         Species = ifelse(Genus == "saxifraga" & Species == "oppostifolia", "oppositifolia", Species),
         Species = ifelse(Genus == "aulacomnium" & Species == "turgidium", "turgidum", Species),
         Species = ifelse(Genus == "sanionia" & Species == "uni", "uncinata", Species)) %>% 
  
  # Fix wrong species
  mutate(Genus = ifelse(ID == "BAR1151", "salix", Genus)) %>% # typed in wrong, right on envelop
  mutate(Species = ifelse(ID == "BAR1151", "polaris", Species)) %>% 
  
  # Fix and check plot names
  mutate(Plot = gsub("_", "-", Plot)) %>% 
  mutate(Plot = ifelse(Plot == "L-1-CLT", "L-1-CTL", Plot)) %>% 
  mutate(Plot = ifelse(ID == "BDJ7423", "L-8-CTL", Plot)) %>% 
  mutate(Plot = ifelse(ID == "BTL8005", "L-1-CTL", Plot)) %>% 
  mutate(Plot = ifelse(ID %in% c("BHG2119", "BJH1430", "BJG7192"), "F", Plot)) %>% 
  mutate(Remark = ifelse(ID %in% c("BHG2119", "BJH1430", "BJG7192"), "assumed this is Plot F", Remark)) %>% 
  mutate(Plot = ifelse(ID == "CAV5132", "A", Plot)) %>% 
  mutate(Remark = ifelse(ID == "CAV5132", "assumed Plot A; Ind_nr_missing; height_missing", Remark)) %>%
  mutate(Plot = ifelse(ID == "ADH9312", "C", Plot)) %>% 
  mutate(Remark = ifelse(ID == "ADH9312", "assumed Plot C", Remark)) %>%
  mutate(Plot = ifelse(ID == "ALI6553", "L-5-OTC", Plot)) %>% 
  mutate(Remark = ifelse(ID == "ALI6553", "assumed Plot 5-OTC", Remark)) %>%
  mutate(Plot = ifelse(ID == "AJL5589", "L-5-CTL", Plot)) %>% 
  mutate(Remark = ifelse(ID == "AJL5589", "changed to Plot 5-CTL", Remark)) %>%
  mutate(Plot = ifelse(ID == "ALO7062", "L-2-CTL", Plot)) %>% 
  mutate(Remark = ifelse(ID == "ALO7062", "was 2 and changed to 2-CTL", Remark)) %>%
  mutate(Plot = ifelse(ID == "BVN8783", "L-5-CTL", Plot)) %>% # fix wrong name L5-CTL
  
  # Project
  mutate(Project = ifelse(ID == "AHE5823", "T", Project)) %>% 
  
  # Remove L- from Plot name for ITEX plants
  mutate(Plot = ifelse(Site == "X", substr(Plot, 3, nchar(Plot)), Plot)) %>% 
  
  mutate(Taxon = paste(Genus, Species, sep = " "))


#join original trait datasheet with the dry mass datasheet
traits_dry_mass1 <- traits_dry_mass %>% select(ID, Dry_mass_g)
traits <- traits %>% left_join(traits_dry_mass1)

#### LEAF AREA ####
load("traits/data/LeafArea2018.Rdata", verbose = TRUE)

## Check Leaf IDs ##
setdiff(LeafArea2018$ID, all_codes$hashcode)
# only Unknown

# check how many ID do not join with trait data
setdiff(LeafArea2018$ID, traits$ID)
# Do not fit with any traits and cannot find out which leaf it is:
#"ATH9996" "BVK6301"  "CRZ2953" "Unknown"

traits2018 %>% 
  anti_join(LeafArea2018, by = "ID") %>% 
  filter(Project != "M") %>% 
  select(ID, Day, Site, Elevation, Genus, Species, Plot, Bulk_nr_leaves, Remark) %>% arrange(ID) %>% pn


# Join Area and Traits
traits2018 <- traits %>% 
  left_join(LeafArea2018, by = "ID") %>% 
  mutate(Bulk_nr_leaves = as.numeric(Bulk_nr_leaves)) %>% 
  mutate(NrLeaves = ifelse(is.na(Bulk_nr_leaves), NumberLeavesScan, Bulk_nr_leaves)) %>% 
  
  # Mark 24 leaves with missing area
  # For some leaf area checkbox was not checked
  # AEB3831 was leaf missing
  mutate(Remark = ifelse(ID %in% c("AWL5310", "BZU8768", "BIE8420", "CUP3093", "BST1760", "BUF9439", "AIO2428", "AVW5412", "AIP2629", "ANW0434", "AEC8296", "AFO1112", "AJI6590", "ALW3077", "AUJ7139", "AUL1863", "AVE0287", "AWU0779", "BJC4868", "BWF1270", "BWZ2813", "CAF5903"), "Missing_leaf_area", Remark)) %>% 
  mutate(Remark = ifelse(ID %in% c("BKO8767"), paste(Remark, "Missing_leaf_area", sep = "; "), Remark))



### Calculate Traits and fix wrong trait values
traitsSV2018 <- traits2018 %>% 
  # Make variables consistent with China and Peru
  rename(Leaf_Area_cm2 = Area_cm2, Dry_Mass_g = Dry_mass_g, Wet_Mass_g = Wet_mass_g, Plant_Height_cm = Plant_height_cm, Leaf_Thickness_1_mm = Leaf_thickness_1_mm, Leaf_Thickness_2_mm = Leaf_thickness_2_mm, Leaf_Thickness_3_mm = Leaf_thickness_3_mm) %>% 
  
  # Fix wrong trait values
  mutate(Leaf_Thickness_3_mm = ifelse(ID == "AWH9637", 0.288, Leaf_Thickness_3_mm),
         Leaf_Thickness_2_mm = ifelse(ID == "CLG6203", 0.49, Leaf_Thickness_2_mm),
         Leaf_Thickness_2_mm = ifelse(ID == "CTH9016", 0.276, Leaf_Thickness_2_mm)) %>% 
  mutate(Wet_Mass_g = ifelse(ID == "AGL0566", 0.0661, Wet_Mass_g),
         Wet_Mass_g = ifelse(ID == "AMG1572", 0.0955, Wet_Mass_g),
         Wet_Mass_g = ifelse(ID == "BJO2182", 0.0712, Wet_Mass_g),
         Wet_Mass_g = ifelse(ID == "AGG1992", 0.055, Wet_Mass_g),
         Wet_Mass_g = ifelse(ID == "AZF1475", 0.0966, Wet_Mass_g),
         Wet_Mass_g = ifelse(ID == "BOM3760", 0.0479, Wet_Mass_g),
         Wet_Mass_g = ifelse(ID == "ASL0771", 0.0884, Wet_Mass_g),
         Wet_Mass_g = ifelse(ID == "APX3945", 0.0351, Wet_Mass_g),
         Wet_Mass_g = ifelse(ID == "ADC9307", 0.067, Wet_Mass_g),
         Remark = ifelse(ID == "ADC9307", "Wet mass 0.607g is clearly wrong; assume it must be 0.067", Remark),
         Wet_Mass_g = ifelse(ID == "BWU3122", 0.0655, Wet_Mass_g),
         Remark = ifelse(ID == "BWU3122", "Wet mass 0.6655g is clearly wrong; assume it must be 0.0655", Remark),
         Wet_Mass_g = ifelse(ID == "BLI0527", 0.053, Wet_Mass_g),
         Remark = ifelse(ID == "BLI0527", paste(Remark, "Wet mass 0.539g is clearly wrong; assume it must be 0.053", sep = "; "), Remark),
         Wet_Mass_g = ifelse(ID == "BSC0608", 0.0402, Wet_Mass_g),
         Remark = ifelse(ID == "BSC0608", "Wet mass 0.4032g is clearly wrong; assume it must be 0.0402", Remark),
         Wet_Mass_g = ifelse(ID == "AWB8528", 0.0296, Wet_Mass_g),
         Remark = ifelse(ID == "AWB8528", "Wet mass 0.296g is clearly wrong; assume it must be 0.0296", Remark),
         Wet_Mass_g = ifelse(ID == "BHV0134", 0.102, Wet_Mass_g),
         Remark = ifelse(ID == "BHV0134", "Wet mass 1.002g is clearly wrong; assume it must be 0.102", Remark),
         Wet_Mass_g = ifelse(ID == "BRD0870", 0.0653, Wet_Mass_g),
         Remark = ifelse(ID == "BRD0870", "Wet mass 0.653g is clearly wrong; assume it must be 0.0653", Remark),
         Wet_Mass_g = ifelse(ID == "CCD9457", 0.0985, Wet_Mass_g),
         Remark = ifelse(ID == "CCD9457", "Wet mass 0.985g is clearly wrong; assume it must be 0.0985", Remark)) %>% 
  mutate(Remark = ifelse(ID %in% c("AHC5738", "AHA7548", "AGY5197", "BTZ1755"), "tiny_leaf", Remark)) %>% 
  
  # Replace Area and Thickness with AQL4331, because leaf was mixed after scannig, deltet AQL4331, because leaf was lost
  mutate(Leaf_Area_cm2 = ifelse(ID == "AEB3831", 2.804, Leaf_Area_cm2),
         Leaf_Thickness_1_mm = ifelse(ID == "AEB3831", 0.0161, Leaf_Thickness_1_mm),
         Leaf_Thickness_2_mm = ifelse(ID == "AEB3831", 0.236, Leaf_Thickness_2_mm),
         Leaf_Thickness_3_mm = ifelse(ID == "AEB3831", 0.194, Leaf_Thickness_3_mm)) %>% 
  filter(ID != "AQL4331") %>% 
  
  # fix wrong leaf numbers
  mutate(NrLeaves = ifelse(ID == "BCX3331", 14, NrLeaves),
         NrLeaves = ifelse(ID == "BOB9666", 2, NrLeaves),
         NrLeaves = ifelse(ID == "ADD9443", 6, NrLeaves),
         NrLeaves = ifelse(ID == "BMN6819", 1, NrLeaves),
         NrLeaves = ifelse(ID == "BSL6524", 2, NrLeaves),
         NrLeaves = ifelse(ID == "CST3822", 3, NrLeaves),
         NrLeaves = ifelse(ID == "BNW8155", 4, NrLeaves),
         NrLeaves = ifelse(ID == "ANO7848", 7, NrLeaves),
         NrLeaves = ifelse(ID == "BXF4662", 8, NrLeaves)) %>% 
  
  # Equisetum does not need leaf number = > Leaf nr = 1 because of calculations below
  mutate(NrLeaves = ifelse(Genus == "equisetum", 1, NrLeaves)) %>% 
  
  # Calculate values on the leaf level (mostly bulk samples)
  rename(Wet_Mass_Total_g = Wet_Mass_g,
         Dry_Mass_Total_g = Dry_Mass_g,
         Leaf_Area_Total_cm2 = Leaf_Area_cm2) %>% 
  mutate(Wet_Mass_g = Wet_Mass_Total_g / NrLeaves,
         Dry_Mass_g = Dry_Mass_Total_g / NrLeaves,
         Leaf_Area_cm2 = Leaf_Area_Total_cm2 / NrLeaves) %>% 
  
  # Calculate SLA, LMDC
  mutate(Leaf_Thickness_Ave_mm = rowMeans(select(., matches("Leaf_Thickness_\\d_mm")), na.rm = TRUE),
         SLA_cm2_g = Leaf_Area_cm2 / Dry_Mass_g,
         LDMC = Dry_Mass_g / Wet_Mass_g,
         # Measures for the mosses
         Length_1_cm = is.numeric(Length_1_cm),
         Length_2_cm = is.numeric(Length_2_cm),
         Length_3_cm = is.numeric(Length_3_cm),
         GreenLength_1_cm = is.numeric(GreenLength_1_cm),
         GreenLength_2_cm = is.numeric(GreenLength_2_cm),
         GreenLength_3_cm = is.numeric(GreenLength_3_cm),
         Length_Ave_Moss_cm = (Length_1_cm + Length_2_cm + Length_3_cm)/3,
         GreenLength_Ave_Moss_cm = (GreenLength_1_cm + GreenLength_2_cm + GreenLength_3_cm)/3) %>% 
         #Length_Ave_Moss_cm = rowMeans(select(., matches("Length_\\d_cm")), na.rm = TRUE)),
         #GreenLength_Ave_Moss_cm = rowMeans(select(., matches("GreenLength_\\d_cm")), na.rm = TRUE)) %>% 
  
  # Make data speak to other PFTC data
  rename(Gradient = Site, Site = Elevation, PlotID = Plot, Data_entered_by = Person_data_entered, Comment = Remark) %>% 
  # join Coords and Elevation
  # ...
    mutate(Country = "SV",
         Year = 2018,
         Treatment = "C",
         Date = ymd(paste(Year, 7, Day, sep = "-")),
         Project = ifelse(Gradient == "X", "X", Project)) %>%
  
  ### ADD ELEVATION; LATITUDE; LONGITUDE
  left_join(coords, by = c("Project", "Treatment", "Site")) %>% 
  
  select(Country, Year, Project, Treatment, Latitude_N, Longitude_E, Elevation_m, Site, Gradient, PlotID, Taxon, Genus, Species, ID, Date, Individual_nr, Plant_Height_cm, Wet_Mass_g, Dry_Mass_g, Leaf_Thickness_Ave_mm, Leaf_Area_cm2, SLA_cm2_g, LDMC, Wet_Mass_Total_g, Dry_Mass_Total_g, Leaf_Area_Total_cm2, Leaf_Thickness_1_mm, Leaf_Thickness_2_mm, Leaf_Thickness_3_mm, Length_Ave_Moss_cm, GreenLength_Ave_Moss_cm, Length_1_cm, Length_2_cm, Length_3_cm, GreenLength_1_cm, GreenLength_2_cm, GreenLength_3_cm, NrLeaves, Bulk_nr_leaves, NumberLeavesScan, Comment, Data_entered_by) %>% 
  mutate(Taxon = ifelse(Taxon == "micranthes hieracifolia", "micranthes hieraciifolia", Taxon),
         Species = ifelse(Species == "hieracifolia", "hieraciifolia", Species))
  
  
checkTraitNames <- tpl.get(unique(traitsSV2018$Taxon))
unique(checkTraitNames$note)
checkTraitNames %>% 
  filter(note == "replaced synonym|family not in APG")

tnrsCheck <- tnrs(query = unique(traitsSV2018$Taxon), source = "iPlant_TNRS")
head(tnrsCheck)

# replaced synonym
#1     Persicaria vivipara replaced synonym      bistorta vivipara
#2 Alopecurus magellanicus replaced synonym      alopecurus ovatus
#3       Saxifraga nivalis replaced synonym     micranthes nivalis
#4   Calamagrostis stricta replaced synonym calamagrostis neglecta, tnrs: Deyeuxia pooides
#5         Draba oblongata replaced synonym          draba arctica
#6 Saxifraga hieraciifolia replaced synonym micranthes hieraciifolia




### Divid into separate data sets
# Gradients, Sean and mosses
traitsGradients_SV_2018 <- traitsSV2018 %>% 
  filter(Project %in% c("T", "Sean", "M"))
save(traitsGradients_SV_2018, file = "traits/data/traitsGradients_SV_2018.Rdata")


# ITEX
traitsITEX_SV_2018 <- traitsSV2018 %>%
  filter(Project == "X") %>% 
  #select(-Length_Ave_Moss_cm, -GreenLength_Ave_Moss_cm, -Length_1_cm, -Length_2_cm, -Length_3_cm, -GreenLength_1_cm, -GreenLength_2_cm, -GreenLength_3_cm, -Gradient) %>% 
  mutate(Treatment = substr(PlotID, str_length(PlotID)-2, str_length(PlotID)),
         PlotID = paste(Treatment, sub("\\-.*$","", PlotID), sep = "-"))
save(traitsITEX_SV_2018, file = "traits/data/traitsITEX_SV_2018.Rdata")
  

# Saxy
traitsSAXY_SV_2018 <- traitsSV2018 %>%
  filter(Project == "Saxy") %>% 
  select(-Length_Ave_Moss_cm, -GreenLength_Ave_Moss_cm, -Length_1_cm, -Length_2_cm, -Length_3_cm, -GreenLength_1_cm, -GreenLength_2_cm, -GreenLength_3_cm, -Gradient) %>% 
  mutate(Site = substr(PlotID, 1, 2))
save(traitsSAXY_SV_2018, file = "traits/data/traitsSAXY_SV_2018.Rdata")



# counts
dim(traits2018) # 1693
traits2018 %>% distinct(Taxon) # 44 species
traits2018 %>% group_by(Taxon) %>% count() %>% pn


#### COMMUNITY DATA ####

# Check which tables you have access to
gs_ls()
# which google sheets do you have access to?
comm <- gs_title("dataPTT4communities")
# list worksheets
gs_ws_ls(comm)
#download data
communityRaw <- gs_read(ss = comm, ws = "DATA") %>% as.tibble()

metaCommunitySV_2018 <- communityRaw %>%
  select(Day, Site, Elevation, Plot, MedianHeight_cm, Max_height_cm, Vascular, Bryophytes, Lichen_soil, Lichen_rock, Rock, BareGround, BioCrust, Litter, Weather, PlotSize_cm2, Aspect, Slope_percent, Notes, Entered_by, Collected_by) %>% 
  filter(!is.na(Site)) %>% # remove empty line
  mutate(Country = "SV",
         Year = 2018,
         Elevation = as.character(Elevation)) %>% 
  rename(Gradient = Site, Site = Elevation, PlotID = Plot)
save(metaCommunitySV_2018, file = "community/data/metaCommunitySV_2018.Rdata")

# Extra Draba data from Finns
drabas <- read_xlsx(path = "community/data/DRABAS_2.xlsx")
drabas <- drabas %>% 
  gather(key = Taxon2, value = occurence, -site, -transect, -plot) %>% 
  filter(occurence > 0) %>% 
  rename(Site = site, Elevation = transect, Plot = plot) %>% 
  select(-occurence) %>% 
  #mutate(Entered_by = "Julia", Collected_by = "Julia") %>% 
  mutate(Taxon2 = ifelse(Taxon2 == "No Drabas", "Cerastium arcticum", Taxon2))
  #mutate(Cover_Fertile = "0.1_1")
  

communitySV_2018 <- communityRaw %>%
  select(-GPS_Nr, -Lat_N, -Long_E, -Scat_species, -MedianHeight_cm, -Max_height_cm, -Vascular, -Bryophytes, -Lichen_soil, -Lichen_rock, -Rock, -BareGround, -BioCrust, -Litter, -Weather, -Elevation_m, -PlotSize_cm2, -Aspect, -Slope_percent, -GPSUnitAccuracy) %>% 
  gather(key = Taxon, value = Cover_Fertile, -Entered_by, -Collected_by, -Day, -Site, -Elevation, -Plot, -Notes) %>% 
  filter(!is.na(Cover_Fertile)) %>% 
  separate(col = Cover_Fertile, into = c("Cover", "Fertile"), sep = "_") %>% 
  
  
  # Rename species
  mutate(Taxon = ifelse(Taxon == "poa alpigena vivipara", "poa arctica_x_pratensis", Taxon),
         Taxon = ifelse(Taxon == "cochleria groenlandica", "cochlearia groenlandica", Taxon),
         Taxon = ifelse(Taxon == "micranthes hieracifolia", "micranthes hieraciifolia", Taxon)) %>% 
  
  # Replace Draba sp1 and sp2 with Julias Draba list
  left_join(drabas, by = c("Site", "Elevation", "Plot")) %>% 
  mutate(Taxon = ifelse(Taxon %in% c("Draba sp1", "Draba sp2", "Draba nivalis", "Draba oxycarpa"), Taxon2, Taxon)) %>% 
  
  mutate(Taxon = tolower(Taxon)) %>% 
  mutate(Elevation = as.character(Elevation)) %>% 
  mutate(Country = "SV",
         Year = 2018,
         Project = "T",
         Treatment = Site,
         Gradient = Site,
         Site = Elevation,
         PlotID = Plot
         ) %>% 
  left_join(coords, by = c("Project", "Treatment", "Site")) %>% 
  select(Country, Year, Project, Latitude_N, Longitude_E, Elevation_m, Site, Gradient, PlotID, Taxon, Cover, Fertile, Notes, Collected_by, Entered_by)

save(communitySV_2018, file = "community/data/communitySV_2018.Rdata")
    

checkCommNames <- tpl.get(unique(communityRaw1$Taxon))
checkCommNames %>% 
  filter(note == "was misspelled|replaced synonym") %>% select(name, original.search)



# Check taxon not in traits or comm.
setdiff(communitySV_2018$Taxon, traitsSV2018$Taxon)
setdiff(traitsSV2018$Taxon, communitySV_2018$Taxon)
