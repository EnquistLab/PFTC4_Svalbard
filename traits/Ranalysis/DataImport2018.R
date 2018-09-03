# LOAD LIBRARIES
library("readxl")
library("tidyverse")
library("lubridate")
#devtools::install_github("gustavobio/tpldata")
#devtools::install_github("gustavobio/tpl")
#library("tpl")
library("googlesheets")

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


# !!!!
#### THINGS TO FIX
# Empty Plot, Ind_nr
# ITEX check Elevation and Plot combinations
# Plot data
# CHECK IF THERE ARE DOUBLE SCANS!!!

# no species names: AAW8078, AAT5935

# check! Strange Plot nr. ABA8118, ATE5693, AGP2467




#### TRAIT, LEAF AREA AND SPECIES COMMUNITY ####
# get ITEX codes
source(file = "community/ImportITEX.R")

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
  
  # Fix Site, elevation etc.
  mutate(Habitat = ifelse(Site == "X", Elevation, NA)) %>%
  #mutate(Date = dmy(paste(Day, "07-2018", sep = "-"))) %>% 
  mutate(Site = ifelse(Site == "x", "X", Site)) %>% 
  mutate(Site = ifelse(ID == "BWS2352", "X", Site)) %>% 
  mutate(Elevation = toupper(Elevation)) %>% 
  mutate(Elevation = ifelse(Elevation %in% c("3 OR 4", "3 - 4"), "3-4", Elevation)) %>% 
  mutate(Elevation = ifelse(ID == "BSE3271", 2, Elevation)) %>% 
  
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
  mutate(Genus = ifelse(ID == "BAR1151", "salix", Genus)) %>% # written wrong from envelop
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




#### Join with leaf area
load("traits/data/LeafArea2018.Rdata", verbose = TRUE)

# check
traits %>% 
  anti_join(LeafArea2018, by = "ID") %>% 
  filter(Project != "M") %>% 
  select(ID, Site, Elevation, Genus, Species, Plot, Remark)
  distinct(Site, Elevation, Plot)

# Join
traits2018 <- traits %>% 
  left_join(LeafArea2018, by = "ID") %>% 
  mutate(Bulk_nr_leaves = as.numeric(Bulk_nr_leaves)) %>% 
  mutate(NrLeaves = ifelse(is.na(Bulk_nr_leaves), NumberLeavesScan, Bulk_nr_leaves))
  
# Leaf nr NA: CBR1720, ATO4822, AKW9471
# Check how many times leaf nr differ

#
dim(traits2018) # 1690
traits2018 %>% distinct(Taxon) # 37 species
traits2018 %>% group_by(Taxon) %>% count() %>% pn


#### IMPORT COMMUNITY DATA

# Check which tables you have access to
gs_ls()
# which google sheets do you have access to?
comm <- gs_title("dataPTT4communities")
# list worksheets
gs_ws_ls(comm)
#download data
community.raw <- gs_read(ss = comm, ws = "DATA") %>% as.tibble()

community.raw1 <- community.raw %>% 
  gather(key = Taxon, value = Cover_Fertile, -Entered_by, -Collected_by, -Day, -Site, -Elevation, -Plot, -GPS_Nr, -Lat_N, -Long_E, -Scat_species, -MedianHeight_cm, -Max_height_cm, -Vascular, -Bryophytes, -Lichen_soil, -Lichen_rock, -Rock, -BareGround, -BioCrust, -Litter, -Weather, -Elevation_m, -PlotSize_cm2, -Aspect, -Slope_percent, -GPSUnitAccuracy, -Notes) %>% 
  filter(!is.na(Cover_Fertile)) %>% 
  mutate(Taxon = tolower(Taxon)) %>% 
  mutate(Taxon = ifelse(Taxon == "poa alpigena vivipara", "poa arctica_x_pratensis", Taxon)) %>% 
  mutate(Elevation = as.character(Elevation))

# Check
setdiff(community.raw1$Taxon, traits$Taxon)

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



 
 
  # remove double scan (other leaf is BDJ1110, where ID is also wrong)
  filter(ID != "VJD1110") %>% 
  
  
  
  # Sum areas for each ID
  group_by(ID) %>% 
  summarise(Area_cm2 = sum(LeafArea)) %>% 

  # replace LeafArea with NA - empty or corrupted scan
  mutate(Area_cm2 = ifelse(ID == "AUB2092", NA, Area_cm2)) %>% 
  add_row(ID = "BMB7274", Area_cm2 = NA) %>% 
  add_row(ID = "EHP2066", Area_cm2 = NA) %>% 
  add_row(ID = "FDF1809", Area_cm2 = NA)



#### LEAF TRAITS ####
files <- dir(path = "traits/data/", pattern = "\\.xlsx$", full.names = TRUE)
traits.raw <- files[grepl("^(?!~)", basename(files), perl = TRUE)] %>% 
  set_names(basename(.)) %>% 
  map_df(read_excel, col_types = c("text", "date", "text", "numeric", rep("text", 4), rep("numeric", 8), "text", "numeric", "text"), .id = "file")

save(traits.raw, file = "traits/data/traits.raw.Rdata")

# Merge traits and LeafArea  and clean data
traits <- traits.raw %>% 
  ### FIX WROND LEAF ID's
  mutate(ID = gsub("WSY0063", "ESY0063", ID),
         ID = gsub("EEA7841", "EAA7841", ID),
         ID = gsub("EOP2126", "EOP2116", ID),
         ID = gsub("ELN57788", "ELN5788", ID),
         ID = gsub("EIH0038", "EHI0038", ID),
         ID = gsub("EFD9867", "EFP9867", ID),
         ID = gsub("EKB8895", "EBK8895", ID),
         ID = gsub("CJO6932", "COJ6932", ID),
         ID = gsub("DJV0218", "DJV0318", ID),
         ID = gsub("DMM5647", "DMM5645", ID),
         ID = gsub("DGE6973", "DGE6927", ID),
         ID = gsub("DCB", "DCB6902", ID),
         ID = gsub("DHG7363", "DHG7383", ID),
         ID = gsub("CMO0287", "CMO2587", ID),
         ID = gsub("CZV0169", "CZV0159", ID),
         ID = gsub("ELB6979", "ELB6970", ID),
         ID = gsub("FAG3940", "FAG3950", ID),
         ID = gsub("CQ06175", "CQO6175", ID),
         ID = gsub("DQO712", "DQO7122", ID),
         ID = gsub("CZZ7222", "DQO7122", ID),
         ID = gsub("BDG9657", "BGD9657", ID),
         ID = gsub("DBE080", "DBE0880", ID),
         ID = gsub("CHK3202", "CHK3203", ID),
         ID = gsub("CFX6171", "CFX6172", ID),
         ID = gsub("BQT3166", "BQT1366", ID),
         ID = gsub("BLM4402", "BML4402", ID),
         ID = gsub("ALD", "ALD3826", ID),
         ID = gsub("GZM1878", "GMZ1878", ID),
         ID = gsub("ARR112", "ARR1112", ID),
         ID = gsub("GWV45096", "GWV4596", ID),
         ID = gsub("AWR2030", "AWR2040", ID),
         ID = gsub("DDC220", "DDC2200", ID),
         ID = gsub("AVE4872", "AVE4827", ID),
         ID = gsub("BQG5351", "BQH5351", ID),
         ID = gsub("EXP4335", "EXK4335", ID), # maybe UEX4335?
         ID = gsub("CTG2458", "CTG2468", ID),
         ID = gsub("AKB5462", "AKB5492", ID),
         ID = gsub("HKL5161", "HKL5191", ID)) %>% 
  filter(!is.na(ID)) %>% # remove 46 lines that have ID as NA, empty rows
  
  ### JOIN TRAITS WITH LEAF AREA
  left_join(LeafArea2018, by = "ID") %>% 
  
  ### FIX WRONG DATA
  # Dates
  mutate(Date = ifelse(Date == "2018-05-15", "2018-03-15", Date)) %>% 
  
  # Site name
  mutate(Site = plyr::mapvalues(Site, c("AJC", "PIL", "WAY", "ACJ", "TRE", "QUE", "Wayqecha", "Way", "QYE"), c("ACJ", "PIL", "WAY", "ACJ", "TRE", "QUE", "WAY", "WAY", "QUE"))) %>% 
  
  # Fix elevation (WAY:3100m; ACJ:3475m; PIL:3675m ; TRE:3715m; QUE:3888m)
  mutate(Elevation = ifelse(Site == "PIL" & Elevation %in% c(2675, 3400, 3600, 3647, 3650, 3670, 3700), 3675, Elevation)) %>% #probably add 3475, but it's ACJ elevation
  mutate(Elevation = ifelse(Site == "ACJ" & Elevation %in% c(3400, 3457, 3465, 3467, 3474, 3487, 3567, 3600, 3440), 3475, Elevation)) %>% 
  mutate(Elevation = ifelse(Site == "TRE" & Elevation %in% c(3700, 3701, 3702, 3710), 3715, Elevation)) %>% 
  mutate(Elevation = ifelse(Site == "QUE" & Elevation %in% c(3800, 3900), 3888, Elevation)) %>% 
  
  # Project
  mutate(Project = ifelse(Project %in% c("Sean", "sean", "SEAN"), "SEAN", Project)) %>% 
  mutate(Project = ifelse(ID %in% c("EYX1643", "EOT2012", "EOR9773"), "SEAN", Project)) %>% 
  
  # missing experiment
  mutate(Experiment = ifelse(ID %in% c("EYX1643", "EOT2012"), "", Experiment)) %>% 
  mutate(Experiment = ifelse(ID == "ETC9124", "C", Experiment)) %>% 
  mutate(Experiment = plyr::mapvalues(Experiment, 
                                      c("B", "Burn", "BB", "C", "c", "E", "EARLY", "Early", "early", "early-E", "L", "LATE", "Late", "late", "missing experiment"), 
                                      c("B", "B", "BB", "C", "C","B", "B", "B", "B", "B", "C", "C", "C", "C", NA))) %>% 
  
  # wrong individual number
  mutate(Individual_nr = ifelse(Site == "WAY" & Genus == "Eriosorus" & Experiment == "C" & Plot == 2 & Individual_nr == 6, 5, Individual_nr)) %>%  
  mutate(Individual_nr = ifelse(Individual_nr == 15, 5, Individual_nr)) %>% 
  
  ### CALCULATE AREA, SLA, etc.
  # Sisyrinchium: leaves are folded: area needs to be doubled and leaf thickness halfed!!!!
  
  
  ### FIXING COMMENTS
  mutate(Comment = ifelse(ID == "ENF3830", paste(Comment, "_dirt"), Comment)) %>% 
  
  ### FLAG DATA
  ## AREAFLAG
  mutate(AreaFlag = ifelse(ID == "BOT1325", "DryLeaf_wrongArea", "")) %>%  # leaf was very dry, wrong area
  mutate(AreaFlag = ifelse(ID %in% c("ECN1410", "EPV0866", "FCC3736", "FDL7538"), "LeafOutside_wrongArea", "")) %>%   # Leaf on ruler or on edge - nothing one can do about it - area flag
  mutate(AreaFlag = ifelse(ID == "EMY0414", "TooLargeArea", "")) %>% # Lycopodiella, more than2 branches scanned
  # Part missing on scan - wrong area
  mutate(AreaFlag = ifelse(ID %in% c("CZL9321", "DUO6664", "DWL3144", "DWV2987", "EFU8488", "EPV0866", "EPW2330", "ERV6823", "ERW0817", "EUG2994", "HHV3850"), "Cut_wrongArea", "")) %>% 

  # Empty or corrupted scan
  mutate(AreaFlag = ifelse(ID == "BMB7274", "EmptyScan_noArea", "")) %>% 
  mutate(AreaFlag = ifelse(ID == "EHP2066", "CorruptedScan_noArea", "")) %>% 
  mutate(AreaFlag = ifelse(ID == "FDF1809", "CorruptedScan_noArea", "")) %>% 
  mutate(AreaFlag = ifelse(ID == "AUB2092", "ScannedOnWrongSide_noArea", "")) %>% 
  
  ## DRYWEIGHTFLAG
  mutate(DryFlag = ifelse(ID == "EMY0414", "TooLargeWeight", "")) %>%  # Lycopodiella, more than2 branches scanned

## WETWEIGHTFLAG
mutate(WetFlag = ifelse(ID == "EMY0414", "TooLargeWeight", "")) # Lycopodiella, more than2 branches scanned

sp <- traits %>% 
  select(Genus) %>% arrange(Genus) %>% distinct(Genus)
write.csv(sp, file = "sp.csv")

traits.fixed.genus <- traits %>% 
  mutate(Genus = gsub("Achemilla|Alchemilla ", "Alchemilla", Genus),
         Genus = gsub("Belonathus|Belonauthus", "Belonanthus", Genus),
         Genus = gsub("Calamagostus", "Calamagrostis", Genus),
         Genus = gsub("Cordateria", "Cortaderia", Genus),
         Genus = gsub("Elaphoglossum ", "Elaphoglossum", Genus),
         Genus = gsub("Gaulteria", "Gaultheria", Genus),
         Genus = gsub("Gentranella", "Gentianella", Genus),
         Genus = gsub("Geufiaua|Geutiana", "Gentiana", Genus),
         Genus = gsub("Hypocheris|Hypoehaens", "Hypochaeris", Genus),
         Genus = gsub("Hypsophila", "Hysophila", Genus),
         Genus = gsub("Lysopomia", "Lysipomia", Genus),
         Genus = gsub("Melpome|Melpone", "Melpomene", Genus),
         Genus = gsub("Nertera |Netera", "Nertera", Genus),
         Genus = gsub("Orchio|Orquidede|Orchid", "Orchidaceae", Genus),
         Genus = gsub("Oritrophilum|Oritrophium|Orithrophium", "Oritrophium", Genus),
         Genus = gsub("Perzia", "Perezia", Genus),
         Genus = gsub("Prenettya", "Pernettya", Genus),
         Genus = gsub("Pterichius|Pterichris", "Pterichis", Genus),
         Genus = gsub("Rhynchosphora|Rhyncosphora|Rhyncospora|Rynchosphora", "Rhynchospora", Genus),
         Genus = gsub("Scripus", "Scirpus", Genus),
         Genus = gsub("Senecia", "Senecio", Genus),
         Genus = gsub("Werneria ", "Werneria", Genus),
         Genus = gsub("new", "New", Genus),
         Genus = gsub("Neurol", "Neurolepis", Genus),
         Genus = gsub("Paspalum", "Paspallum", Genus),
         Genus = gsub("Myconia", "Miconia", Genus),
         Genus = gsub("Gamachaeta", "Gamochaeta", Genus),
         Genus = gsub("Niphogetum", "Niphogeton", Genus),
         Genus = gsub("Oerithales", "Oreithales", Genus)) 
  
save(traits.fixed.genus, file = "traits.fixed.genus.Rdata")

Genus == "Werneria", Species == "nubigena", Experiment == "B", Plot == 1, Individual_nr == 2 -> QUE not ACj

###
# TO DO !!!
# not enough envelopes, wrong ind_nr 6: traits.fixed.genus %>% filter(Site == "PIL", Genus == "Carex", Experiment == "C", Plot == 5)

# Werneria nubigena, B, Plot 1, Ind_nr 1 has no site???


traits <- traits.fixed.genus %>%
  filter(is.na(Project), !is.na(Site)) %>% 
  mutate(Site = factor(Site, levels = c("WAY", "ACJ", "PIL", "TRE", "QUE")))
  


traits.fixed.genus %>% 
  select(Genus) %>% arrange(Genus) %>% distinct(Genus) %>% pn


traits.cover <- traits.fixed.genus %>% 
  left_join(all.cover, by = c("Site" = "site", "Plot" = "plot", "Experiment" = "treatment", "Genus" = "genus"))
save(traits.cover, file = "traits.cover.Rdata")
traits.cover %>% filter(is.na(cover))

library("taxize")
names <- gnr_resolve(names = sp$Genus, db = "tnrs")
tnrs(sp$Genus)

### Fix species names
traits %>% 
  mutate(Taxon = paste(Genus, Species, sep = " ")) %>% filter(Taxon == "Paspalum Sean")
  mutate(Taxon = gsub("Elaphoglossum sp. ", "Elaphoglossum sp.", Taxon)) %>% 
  mutate(Taxon = gsub("Carex P", "Carex pinchinensis", Taxon)) %>% 
  mutate(Taxon = gsub("Bromus sp. Blue", "Bromus blue", Taxon)) %>% 
  mutate(Taxon = gsub("Eriosorus chelianthoides", "Eriosorus cheilanthoides", Taxon)) %>% 
  mutate(Taxon = gsub("Eriosorus sp", "Eriosorus sp.", Taxon)) %>% 
  mutate(Taxon = gsub("Nertera  granadensis", "Nertera granadensis", Taxon)) %>% 
  mutate(Taxon = gsub("Orchio sp.", "Orchid sp.", Taxon)) %>% 
  mutate(Taxon = gsub("Pernettya sp", "Pernettya sp.", Taxon)) %>% 
  mutate(Taxon = gsub("Werneria nugibena", "Werneria nubigena", Taxon))

Cortaderia NA = Cortaderia sp?
Hieracium NA = Hieracium sp?
Melpomene NA = Melpomene sp?
Paspalum bonplandianum=Paspalum Sean=Paspalum sp. ???
Rhynchosphora 5 different spellings

  
### some 17.3.2018 QEL are actually WAY
