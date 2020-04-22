#### DOWNLOAD RAW TRAIT DATA FROM OSF ####
source(file = "rawdataCleaning/Download_Raw_Data.R")


# LOAD LIBRARIES
library("readxl")
library("tidyverse")
library("lubridate")
#devtools::install_github("gustavobio/tpldata")
#devtools::install_github("gustavobio/tpl")
library("tpl")
library("taxize")
library("googlesheets4")


pn <- . %>% print(n = Inf)

# source import of chemical data
source("rawdataCleaning/R/ImportChemicalTraits.R")

# Source ITEX (for Site-Elevation comninations)
#source(file = "community/ImportITEX.R")
### Read in BIEN and TTT traits
#BienTTT <- read_rds(path = "traits/data/BIEN_and_TTT_traits_for_ITEX_species.rds")


#### READ IN DATA SETS ####
coords <- read_excel(path = "traits/cleaned_Data/PFTC4_Svalbard_Coordinates.xlsx", col_names = TRUE)
traits_in <- read_csv(file = "traits/data/PFTC4_Svalbard_2018_LeafTrait_with_DM.csv")
LeafArea2018 <- read_csv(file = "traits/data/PFTC4_Svalbard_2018_Raw_LeafArea.csv", col_names = TRUE)


########################################################################
#### DATA CHECKING ####
#check which dry masses are missing from leaves from the sites
#ITEX - 14
# missing_dry_itex <- traits %>% filter(Site == "X") %>% filter(is.na(Dry_mass_g))
# #Control gradient - 5
# missing_dry_control <- traits %>% filter(Site == "C") %>% filter(is.na(Dry_mass_g))
# #Birdcliff gradient - 4
# missing_dry_birdcliff <- traits %>% filter(Site == "B") %>% filter(is.na(Dry_mass_g))

# Check LeafID
# Load trait IDs
# load("traits/Rdatagathering/envelope_codes.Rdata", verbose = TRUE)
# setdiff(traits$ID, all_codes$hashcode)
# 
# 
#  # Check values
# unique(traits$Day)
# unique(traits$Site)
# unique(traits$Elevation)
# 
# # Check combinations of Site and Elevation
# table(traits$Site, traits$Elevation)
# 
# # Species and Genus
# traits %>% distinct(Genus) %>% arrange(Genus) %>% pn
# traits %>% distinct(Species) %>% arrange(Species) %>% pn
# traits %>% 
#   mutate(Taxon = paste(Genus, Species, sep = " ")) %>% 
#   arrange(Taxon) %>% distinct(Taxon) %>% pn
# 
# # Plot name
# unique(traits$Plot)
# traits %>% filter(is.na(Plot)) %>% filter(Project != "Sean", Project != "M") # 15 without plot
# table(traits$Plot, traits$Site)
# traits %>% 
#   filter(Site == "X") %>% 
#   distinct(Plot, Elevation) %>% 
#   arrange(Elevation, Plot) %>% pn
# 
# # Check all the itex plot names and habitat comninations
# traits %>% 
#   filter(Site == "X") %>% 
#   anti_join(itex.codes, by = c("Elevation" = "Habitat", "Plot")) %>% 
#   select(ID, Elevation, Genus, Species, Plot, Individual_nr, Remark)
# itex.codes %>% pn
########################################################################



## FIX THE BUGS ##
# What was fixed and not fixed
# Cannot fix
# 3x stellaria longipes: plot and elev missing
# CCK4783 equisetum arvense: plot number missing, cannot fix
# CDX1924: remark is D or E; Already 3 E, and no cover in D, cannot fix
# AWN7480: no Plot on envelope. Could be 1-CTL or 5-OTC as those only have 2 ind. Cannot fix.
# AYU6804 oxyria digyna: Plot says 4, cannot fix

# Fixed
# BHG2119, BJH1430, BJG7192: assumed that these are Plot F, because cover is higher. Could also be B, but cover in B is only 0.5
# CAV5132: assume it is Plot A (only 2 ind. from Elevation 1 and Plot A)
# ADH9312 luzula nivalis: assumed C, Plot = 3, which is C and C ahs only 2 ind. Ind nr fits
# ALI6553: assumed Plot 5-OTC because only 2 ind form this plot and LeafID fits
# AJL5589: change from OTC to CTL. 5-OTC does not exist and only 2 leaves from 5-CTL
# ALO7062: Plot name was 2 and changed to 2-CTL
# CDW3071: It says Plot D or E; unlikely to be any of those, because there are already 3 of each plot, made it Plot C, only possibility
# CVP1261: It says Plot D or E; But no cover in D or E. Could be F, so assumed it to be F
####################################################################################################

#### CLEAN DATA ####
traits <- traits_in %>% 
  # Fix leafID
  mutate(ID = recode(ID, "CIG85099" = "CIG8509",
                     "CHF3’094" = "CHF3094")) %>% 
  
  mutate(ID = gsub("BMT5974", "BTM5974", ID),
         ID = gsub("AZL0848", "BZL0848", ID),
         ID = gsub("BHS6740", "BHS6704", ID),
         ID = gsub("BAH7471", "BAH7571", ID),
         ID = gsub("BDP43249", "BDP4329", ID),
         ID = gsub("BUS0605", "BSU0605", ID),
         ID = gsub("ADX3335", "ADK3335", ID),
         ID = gsub("BTK0192", "BTK0182", ID),
         ID = gsub("BJG192", "BJG7192", ID),
         ID = gsub("ACV501", "ACV0501", ID),
         ID = gsub("AJJ5071", "AJJ5061", ID),
         ID = gsub("AXX2436", "AAX2436", ID),
         ID = gsub("AWW8078", "AAW8078", ID),
         ID = gsub("AYP7268", "AYP7286", ID),
         ID = gsub("AQQ9548", "AQQ9458", ID),
         ID = gsub("CHF3’094", "CHF3094", ID),
         ID = gsub("CIG850", "CIG8509", ID),
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
         Species = ifelse(Genus == "sanionia" & Species == "uni", "uncinata", Species),
         Species = ifelse(Genus == "micranthes" & Species == "hieracifolia", "hieraciifolia", Species)) %>% 
  
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
  mutate(Plot = ifelse(ID == "CDW3071", "C", Plot),
         Remark = ifelse(ID == "CDW3071", "assumed Plot C", Remark),
         Plot = ifelse(ID == "CVP1261", "F", Plot),
         Remark = ifelse(ID == "CVP1261", "assumed Plot F", Remark)) %>%
  
  # Project
  mutate(Project = ifelse(ID == "AHE5823", "T", Project)) %>% 
  
  # Remove L- from Plot name for ITEX plants
  mutate(Plot = ifelse(Site == "X", substr(Plot, 3, nchar(Plot)), Plot)) %>% 
  
  mutate(Taxon = paste(Genus, Species, sep = " "))



#### JOIN TRAITS AND LEAF AREA DATA ####

## Check if Leaf IDs are valid ##
#setdiff(LeafArea2018$ID, all_codes$hashcode)
# only Unknown

# check how many ID do not join with trait data
#setdiff(LeafArea2018$ID, traits$ID)
# Do not fit with any traits and cannot find out which leaf it is:
#"ATH9996", "Unknown"

# 24 leaves that have no scan
# traits %>% 
#   anti_join(LeafArea2018, by = "ID") %>% 
#   filter(Project != "M") %>% 
#   select(ID, Day, Site, Elevation, Genus, Species, Plot, Bulk_nr_leaves, Remark) %>% arrange(ID) %>% pn


# Join Area and Traits
traits2018 <- traits %>% 
  left_join(LeafArea2018, by = "ID") %>% 
  mutate(Bulk_nr_leaves = as.numeric(Bulk_nr_leaves)) %>% # NA's introduced here, because characters (bulk)
  mutate(NrLeaves = ifelse(is.na(Bulk_nr_leaves), NumberLeavesScan, Bulk_nr_leaves)) %>% 

  # Mark 24 leaves with missing area
  # For some leaf area checkbox was not checked
  # AEB3831 was leaf missing
  mutate(Remark = ifelse(ID %in% c("AWL5310", "BZU8768", "BIE8420", "CUP3093", "BST1760", "BUF9439", "AIO2428", "AVW5412", "AIP2629", "ANW0434", "AEC8296", "AFO1112", "AJI6590", "ALW3077", "AUJ7139", "AUL1863", "AVE0287", "AWU0779", "BJC4868", "BWF1270", "BWZ2813", "CAF5903"), "Missing_leaf_area", Remark)) %>% 
  mutate(Remark = ifelse(ID %in% c("BKO8767"), paste(Remark, "Missing_leaf_area", sep = "_"), Remark))



#### CALCULATE SLA, LDMC AND FIX MORE STUFF ####
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
         Wet_Mass_g = ifelse(ID == "AWZ0728", 0.0725, Wet_Mass_g),
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
  
  # Fix Dry_Mass
  mutate(Dry_Mass_g = ifelse(ID == "AUP7141", 0.011, Dry_Mass_g),
         Dry_Mass_g = ifelse(ID == "AGD0525", 0.0066, Dry_Mass_g),
         Dry_Mass_g = ifelse(ID == "AKI3166", 0.012, Dry_Mass_g),
         Dry_Mass_g = ifelse(ID == "AEA5561", 0.023, Dry_Mass_g),) %>% 
  
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
  
  # Fix BMN6819
  # Only one leaf in the bag; wet weight probably 6 leaves, dry weight will be only one leaf
  # Nees multiplying by 6, becaue leaf nr has been changed to 1
  mutate(Wet_Mass_g = if_else(ID == "BMN6819", Wet_Mass_g / 6, Wet_Mass_g)) %>% 

  # CMP9835
  mutate(Leaf_Thickness_1_mm = if_else(ID == "CMP9835", NA_real_, Leaf_Thickness_1_mm),
         Leaf_Thickness_2_mm = if_else(ID == "CMP9835", NA_real_, Leaf_Thickness_2_mm),
         Leaf_Thickness_3_mm = if_else(ID == "CMP9835", NA_real_, Leaf_Thickness_3_mm),
         Length_1_cm = if_else(ID == "CMP9835", 4.5, Length_1_cm),
         Length_2_cm = if_else(ID == "CMP9835", 2.7, Length_2_cm),
         Length_3_cm = if_else(ID == "CMP9835", 3.4, Length_3_cm)) %>% 
  
  # Calculate SLA, LMDC
  mutate(Leaf_Thickness_Ave_mm = rowMeans(select(., matches("Leaf_Thickness_\\d_mm")), na.rm = TRUE),
         SLA_cm2_g = Leaf_Area_cm2 / Dry_Mass_g,
         LDMC = Dry_Mass_g / Wet_Mass_g) %>% 

  # Measures for the mosses
  mutate(Length_Ave_Moss_cm = (Length_1_cm + Length_2_cm + Length_3_cm)/3,
         GreenLength_Ave_Moss_cm = (GreenLength_1_cm + GreenLength_2_cm + GreenLength_3_cm)/3) %>% 
        # should probably use something like this
         #Length_Ave_Moss_cm = rowMeans(select(., matches("Length_\\d_cm")), na.rm = TRUE)),
         #GreenLength_Ave_Moss_cm = rowMeans(select(., matches("GreenLength_\\d_cm")), na.rm = TRUE)) %>% 
  
  # Flags and filter unrealistic trait values
  mutate(Dry_Mass_g = ifelse(SLA_cm2_g > 500, NA_real_, Dry_Mass_g),
         Wet_Mass_g = ifelse(SLA_cm2_g > 500, NA_real_, Wet_Mass_g),
         Leaf_Area_cm2 = ifelse(SLA_cm2_g > 500, NA_real_, Leaf_Area_cm2),
         Dry_Mass_Total_g = ifelse(SLA_cm2_g > 500, NA_real_, Dry_Mass_Total_g),
         Wet_Mass_Total_g = ifelse(SLA_cm2_g > 500, NA_real_, Wet_Mass_Total_g),
         Leaf_Area_Total_cm2 = ifelse(SLA_cm2_g > 500, NA_real_, Leaf_Area_Total_cm2),
         Flag = ifelse(SLA_cm2_g > 500, "SLA_>_500", NA_character_),
         SLA_cm2_g = ifelse(SLA_cm2_g > 500, NA_real_, SLA_cm2_g),
         
         Dry_Mass_g = ifelse(LDMC > 1, NA_real_, Dry_Mass_g),
         Wet_Mass_g = ifelse(LDMC > 1, NA_real_, Wet_Mass_g),
         Dry_Mass_Total_g = ifelse(LDMC > 1, NA_real_, Dry_Mass_Total_g),
         Wet_Mass_Total_g = ifelse(LDMC > 1, NA_real_, Wet_Mass_Total_g),
         Flag = ifelse(LDMC > 1, "LDMC_>_1", NA_character_),
         LDMC = ifelse(LDMC > 1, NA_real_, LDMC)) %>% 
  mutate(Flag = if_else(ID == "CCP4302", "Area cut a tiny bit", NA_character_)) %>% 
  mutate(Flag = if_else(ID %in% c("ADM0955", "AMD6577", "AME5937", "AMF5763", "AMH0895", "AOE8815", "AOT9797", "ASF0636", "ASF0636", "BLS7300", "BUR2769", "BUZ1775"), "Thickness_measure_wrong_remeasured_dry_leaf_might_be_too_small_value", NA_character_)) %>% 
  # Make data speak to other PFTC data
  rename(Gradient = Site, Site = Elevation, PlotID = Plot, Data_entered_by = Person_data_entered, Comment = Remark) %>% 
  mutate(Country = "SV",
         Year = 2018,
         Treatment = Gradient,
         Date = ymd(paste(Year, 7, Day, sep = "-"))) %>% # makes one NA, because Day was NA
  
  # remove 2 species where PlotID is unknown
  filter(!(is.na(PlotID) & Site %in% c("CAS", "BIS"))) %>% 
  # change one sp where site name was wrong (3)
  mutate(Site = if_else(Gradient == "X" & Site == "3", "DRY", Site)) %>% 
  
  # Impossible to fix leaves (missing Site and/or PlotID)
  filter(!ID %in% c("CDX1924", "AYU6804", "CUN0289", "CUO1118", "CUR9333")) %>% 
  
  # Fix PlotID and Treatment for ITEX
  mutate(Treatment = if_else(Gradient == "X", substr(PlotID, str_length(PlotID)-2, str_length(PlotID)), Treatment),
         PlotID = if_else(Project == "X", paste(Site, sub("\\-.*$","", PlotID), sep = "-"), PlotID)) %>% 
  # fix AKZ0354
  mutate(PlotID = if_else(ID == "AKZ0354", "8-OTC", PlotID)) %>% 

  ### ADD ELEVATION; LATITUDE; LONGITUDE
  left_join(coords, by = c("Project", "Treatment", "Site")) %>% 
  
  select(Country, Year, Project, Treatment, Latitude_N, Longitude_E, Elevation_m, Site, Gradient, PlotID, Taxon, Genus, Species, ID, Date, Individual_nr, Plant_Height_cm, Wet_Mass_g, Dry_Mass_g, Leaf_Thickness_Ave_mm, Leaf_Area_cm2, SLA_cm2_g, LDMC, Wet_Mass_Total_g, Dry_Mass_Total_g, Leaf_Area_Total_cm2, Leaf_Thickness_1_mm, Leaf_Thickness_2_mm, Leaf_Thickness_3_mm, Flag, Length_Ave_Moss_cm, GreenLength_Ave_Moss_cm, Length_1_cm, Length_2_cm, Length_3_cm, GreenLength_1_cm, GreenLength_2_cm, GreenLength_3_cm, NrLeaves, Bulk_nr_leaves, NumberLeavesScan, Comment, Data_entered_by)



### Add missing Individual_Nr for ITEX and gradients
IndNr <- traitsSV2018 %>% 
  filter(Gradient %in% c("B", "C", "X"),
         Taxon != "betula nana") %>% 
  distinct(Treatment, Site, PlotID, Taxon, Individual_nr) %>% 
  arrange(Treatment, Site, PlotID, Taxon, Individual_nr) %>% 
  group_by(Treatment, Site, PlotID, Taxon) %>% 
  mutate(n = as.double(1:n())) %>% 
  mutate(newIndNr = coalesce(Individual_nr, n)) %>% 
  # Sometimes Ind_nr does not start with 1, Ind_nr NA, 2, then the method does not work. Solution if 2x same Ind_nr, then new + 1 (dirty trick but works :-)
  ungroup() %>% 
  group_by(Treatment, Site, PlotID, Taxon, newIndNr) %>% 
  mutate(nx = 1:n()) %>% 
  mutate(newIndNr = if_else(nx == 2, newIndNr + 1, newIndNr)) %>% 
  ungroup() %>% 
  # Check again
  # group_by(Treatment, Site, PlotID, Taxon, new) %>% 
  # mutate(nx2 = n()) %>% 
  # filter(nx2 > 1) %>% 
  select(-n, -nx)
  
# Replace Ind
traitsSV2018 <- traitsSV2018 %>% 
  left_join(IndNr, by = c("Treatment", "Site", "PlotID", "Taxon", "Individual_nr")) %>% 
  mutate(Individual_nr = if_else(Gradient %in% c("B", "C", "X"), newIndNr, Individual_nr)) %>% 
  select(-newIndNr)


#### CHECK TRAIT NAMES WITH TNRS ####
# checkTraitNames <- tpl.get(unique(traitsSV2018$Taxon))
# unique(checkTraitNames$note)
# checkTraitNames %>% 
#   filter(note == "replaced synonym|family not in APG")
# 
# tnrsCheck <- tnrs(query = unique(traitsSV2018$Taxon), source = "iPlant_TNRS")
# head(tnrsCheck)

# replaced synonym
#1     Persicaria vivipara replaced synonym      bistorta vivipara
#2 Alopecurus magellanicus replaced synonym      alopecurus ovatus
#3       Saxifraga nivalis replaced synonym     micranthes nivalis
#4   Calamagrostis stricta replaced synonym calamagrostis neglecta, tnrs: Deyeuxia pooides
#5         Draba oblongata replaced synonym          draba arctica
#6 Saxifraga hieraciifolia replaced synonym micranthes hieraciifolia


# Check trait values
# traitsSV2018 %>%
#   ggplot(aes(x = log(Dry_Mass_g), y = log(Wet_Mass_g), colour = LDMC > 1)) +
#   geom_point()
# 
# traitsSV2018 %>%
#   ggplot(aes(x = log(Dry_Mass_g), y = log(Leaf_Area_cm2), colour = SLA_cm2_g > 500)) +
#   geom_point()

# traitsSV2018 %>%
#   ggplot(aes(x = Leaf_Thickness_1_mm, y = Leaf_Thickness_3_mm)) +
#   geom_point()

# These IDs are in the cnp but not trait data:
# AQL4331 was deleted, because leaf was lost, obviously not
# DBO4590, DEG1493, DEJ2799, DFE9019


#### JOIN CNP AND TRAITS DATA ####
traitsSV2018 <- traitsSV2018 %>% 
  left_join(cnp_data, by = c("ID", "Country"))



#### DIVID DATA INTO GRADIEN AND ITEX ####
# Gradients
traitsGradients_SV_2018 <- traitsSV2018 %>% 
  filter(Project %in% c("T"))
write_csv(traitsGradients_SV_2018, path = "traits/cleaned_Data/PFTC4_Svalbard_2018_TraitsGradients.csv", col_names = TRUE)

# ITEX
# 3 ind of Betula nana in the data, no Site, PlotID info etc.
traitsITEX_SV_2018 <- traitsSV2018 %>%
  filter(Gradient == "X") %>% 
  select(-Length_Ave_Moss_cm, -GreenLength_Ave_Moss_cm, -Length_1_cm, -Length_2_cm, -Length_3_cm, -GreenLength_1_cm, -GreenLength_2_cm, -GreenLength_3_cm, -Gradient)
write_csv(traitsITEX_SV_2018, path = "traits/cleaned_Data/PFTC4_Svalbard_2018_ITEX.csv", col_names = TRUE)
  
# Mosses
traitsGradients_Bryophytes_SV_2018 <- traitsSV2018 %>% 
  filter(Project %in% c("M")) 
write_csv(traitsGradients_Bryophytes_SV_2018, path = "traits/cleaned_Data/PFTC4_Svalbard_2018_Bryo_TraitsGradients.csv", col_names = TRUE)

# # Saxy
traitsSAXY_SV_2018 <- traitsSV2018 %>%
  filter(Project == "Saxy") %>%
  select(-Length_Ave_Moss_cm, -GreenLength_Ave_Moss_cm, -Length_1_cm, -Length_2_cm, -Length_3_cm, -GreenLength_1_cm, -GreenLength_2_cm, -GreenLength_3_cm, -Gradient) %>%
  mutate(Site = substr(PlotID, 1, 2))
write_csv(traitsSAXY_SV_2018, path = "traits/cleaned_Data/PFTC4_Svalbard_Traits_2018_Saxy.csv", col_names = TRUE)


# counts
dim(traits2018) # 1696
traits2018 %>% distinct(Taxon) # 45 species
traits2018 %>% group_by(Taxon) %>% count() %>% pn
