# LOAD LIBRARIES
library("readxl")
library("tidyverse")
library("lubridate")
#devtools::install_github("gustavobio/tpldata")
#devtools::install_github("gustavobio/tpl")
#library("tpl")
library("googlesheets")

# Source ITEX (for Site-Elevation comninations)
source(file = "community/ImportITEX.R")

pn <- . %>% print(n = Inf)


#### IMPORT TRAIT DATA ####

# Download data from google sheet

# Check which tables you have access to
gs_ls()
# which google sheets do you have access to?
trait <- gs_title("LeafTrait_Svalbard")
# list worksheets
gs_ws_ls(trait)
#download data
traits <- gs_read(ss = trait, ws = "Tabellenblatt1") %>% as.tibble()


## DATA CHECKING ##
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
         ID = gsub("CMP9385", "CMP9835", ID)) %>% 
  
  # Fix Project, Site, elevation etc.
  mutate(Habitat = ifelse(Site == "X", Elevation, NA)) %>%
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
         Species = ifelse(Genus == "micranthes" & Species == "hieraciifolia", "hieracifolia", Species),
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
  
  # Remove L- from Plot name for ITEX plants
  mutate(Plot = ifelse(Site == "X", substr(Plot, 3, nchar(Plot)), Plot)) %>% 
  
  mutate(Taxon = paste(Genus, Species, sep = " "))



#### LEAF AREA ####
load("traits/data/LeafArea2018.Rdata", verbose = TRUE)

# Check Leaf IDs
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


# Join
traits2018 <- traits %>% 
  left_join(LeafArea2018, by = "ID") %>% 
  mutate(Bulk_nr_leaves = as.numeric(Bulk_nr_leaves)) %>% 
  mutate(NrLeaves = ifelse(is.na(Bulk_nr_leaves), NumberLeavesScan, Bulk_nr_leaves)) %>% 
  
  # Mark 24 leaves with missing area
  # For some leaf area checkbox was not checked
  # AEB3831 was leaf missing
  mutate(Remark = ifelse(ID %in% c("AWL5310", "BZU8768", "BIE8420", "CUP3093", "BST1760", "BUF9439", "AIO2428", "AVW5412", "AIP2629", "ANW0434", "AEC8296", "AFO1112", "AJI6590", "ALW3077", "AUJ7139", "AUL1863", "AVE0287", "AWU0779", "BJC4868", "BWF1270", "BWZ2813", "CAF5903"), "Missing_leaf_area", Remark)) %>% 
  mutate(Remark = ifelse(ID %in% c("BKO8767"), paste(Remark, "Missing_leaf_area", sep = "; "), Remark))

save(traits2018, file = "traits/data/traits_SV_2018.Rdata")

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

metaCommunity <- communityRaw %>%
  select(Day, Site, Elevation, Plot, MedianHeight_cm, Max_height_cm, Vascular, Bryophytes, Lichen_soil, Lichen_rock, Rock, BareGround, BioCrust, Litter, Weather, Elevation_m, PlotSize_cm2, Aspect, Slope_percent, Notes, Entered_by, Collected_by)


# Extra Draba data from Finns
drabas <- read_xlsx(path = "community/data/DRABAS_2.xlsx")
drabas <- drabas %>% 
  gather(key = Taxon, value = occurence, -site, -transect, -plot) %>% 
  filter(occurence > 0) %>% 
  rename(Site = site, Elevation = transect, Plot = plot) %>% 
  select(-occurence) %>% 
  mutate(Entered_by = "Julia", Collected_by = "Julia") %>% 
  mutate(Cover_Fertile = "0.1_1")
  

communityRaw1 <- communityRaw %>%
  select(-GPS_Nr, -Lat_N, -Long_E, -Scat_species, -MedianHeight_cm, -Max_height_cm, -Vascular, -Bryophytes, -Lichen_soil, -Lichen_rock, -Rock, -BareGround, -BioCrust, -Litter, -Weather, -Elevation_m, -PlotSize_cm2, -Aspect, -Slope_percent, -GPSUnitAccuracy) %>% 
  gather(key = Taxon, value = Cover_Fertile, -Entered_by, -Collected_by, -Day, -Site, -Elevation, -Plot, -Notes) %>% 
  filter(!is.na(Cover_Fertile)) %>% 
  
  
  # Rename species
  mutate(Taxon = ifelse(Taxon == "poa alpigena vivipara", "poa arctica_x_pratensis", Taxon)) %>% 
  
  # Remove Draba sp1 and sp2
  filter(!Taxon %in% c("Draba sp1", "Draba sp2")) %>% 
  bind_rows(drabas) %>% 
  mutate(Taxon = ifelse(Site == "B" & Elevation == 1 & Plot == "E", "Cerastium sp", Taxon)) %>% 
  # !!! THIS PLOT HAS CERASTIUM ARCTICA, IS NO DRABA ARCTICA? THEN REMOVE THIS LINE
  mutate(Taxon = ifelse(Site == "C" & Elevation == 5 & Plot == "D", "Cerastium sp", Taxon)) %>% 
  filter(Taxon != "No Drabas") %>% 
  mutate(Taxon = tolower(Taxon)) %>% 
  mutate(Elevation = as.character(Elevation))
    

# Check
setdiff(communityRaw1$Taxon, traits2018$Taxon)
setdiff(traits2018$Taxon, communityRaw1$Taxon)

### need to check how many do not join traits and community.
### and how to join community and itex etc.


traits %>% 
  filter(!Project %in% c("P", "M", "Sean"), Site != "X") %>% 
  anti_join(community.raw1, by = c("Site", "Elevation", "Plot", "Taxon")) %>% 
  filter(!is.na(Elevation)) %>% 
  filter(!is.na(Plot)) %>% 
  select(Site, Elevation, Plot, Taxon) %>% pn

### Join trait and community data
# used left_join because we are interested in the traits for now, but species with low cover and no trait data get lost!
TraitsCommGradients <- traits2018 %>% 
  filter(!Project %in% c("P", "M", "Sean"), Site != "X") %>% 
  left_join(community.raw1, by = c("Site", "Elevation", "Plot", "Taxon"))
save(TraitsCommGradients, file = "traits/data/TraitsCommGradients.Rdata")

TraitsAllProjects <- traits2018
save(TraitsAllProjects, file = "traits/data/TraitsAllProjects.Rdata")


### ITEX
TraitsCommITEX <- traits2018 %>% 
  filter(Site == "X") %>% 
  left_join(itex1, by = c("Elevation", "Plot", "Taxon"))
save(TraitsCommITEX, file = "traits/data/TraitsCommITEX.Rdata")



setdiff(traits1$Taxon, itex1$Taxon)
