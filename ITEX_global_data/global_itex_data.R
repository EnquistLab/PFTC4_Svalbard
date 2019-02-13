#Pulling global data for ITEX species

#From BIEN, etc.

load("C:/Users/Brian/Google Drive/PFTC4_Svalbard/USE_THESE_DATA/Traits/traitsITEX_SV_2018.Rdata")

taxa<-unique(traitsITEX_SV_2018$Taxon)

taxa<-as.vector(sapply(X = taxa,FUN = function(x){paste(c(toupper(x = strsplit(x,"*")[[1]][1]),strsplit(x,"*")[[1]][-1]),collapse = "")}))
taxa<-c(taxa, "Alopecurus magellanicus") #Alopecurus ovatus is a synonym of Alopecurus magellanicus according to TPL.

library(BIEN)

BIEN_itex_data<-BIEN_trait_species(species = taxa,political.boundaries = T,all.taxonomy = T,source.citation = T)

setdiff(x = taxa,y = unique(BIEN_itex_data$scrubbed_species_binomial)) #Looks like we got everything except A. ovatus, which is a bad name anyhow

clean<-read.csv("C:/Users/Brian/Desktop/current_projects/TraitHub/data_clean/TTT_cleaned_dataset.csv")

clean<-clean[which(clean$AccSpeciesName %in% taxa),]#cut data down to species we need
clean$Trait<-as.character(clean$Trait)


unique(clean$Trait)

unique(BIEN_itex_data$trait_name)


#18 traits to rename
clean$Trait[which(clean$Trait=="Leaf area")]<-"leaf area"
clean$Trait[which(clean$Trait=="Leaf area per leaf dry mass (specific leaf area, SLA)")]<-"leaf area per leaf dry mass"
clean$Trait[which(clean$Trait=="Leaf carbon (C) content per leaf dry mass")]<-"leaf carbon content per leaf dry mass"
clean$Trait[which(clean$Trait=="Leaf carbon (C) isotope discrimination (delta 13C)")]<-"leaf delta 13c"
clean$Trait[which(clean$Trait=="Leaf carbon/nitrogen (C/N) ratio")]<-"leaf carbon content per leaf nitrogen content"
clean$Trait[which(clean$Trait=="Leaf dry mass")]<-"leaf dry mass"
clean$Trait[which(clean$Trait=="Leaf dry mass per leaf fresh mass (Leaf dry matter content, LDMC)")]<-"leaf dry mass per leaf fresh mass"
clean$Trait[which(clean$Trait=="Leaf fresh mass")]<-"leaf fresh mass"
clean$Trait[which(clean$Trait=="Leaf nitrogen (N) content per leaf dry mass")]<-"leaf nitrogen content per leaf dry mass"
clean$Trait[which(clean$Trait=="Leaf nitrogen (N) isotope signature (delta 15N)")]<-"leaf delta 15n"
clean$Trait[which(clean$Trait=="Leaf nitrogen/phosphorus (N/P) ratio")]<-"leaf nitrogen content per leaf phosphorus content"
clean$Trait[which(clean$Trait=="Leaf phosphorus (P) content per leaf dry mass")]<-"leaf phosphorus content per leaf dry mass"
clean$Trait[which(clean$Trait=="Plant height, reproductive")]<-"whole plant height generative"
clean$Trait[which(clean$Trait=="Plant height, vegetative")]<-"whole plant height vegetative"
clean$Trait[which(clean$Trait=="Rooting depth")]<-"whole plant rooting depth"
clean$Trait[which(clean$Trait=="Seed dry mass")]<-"seed dry mass"
clean$Trait[which(clean$Trait=="Stem diameter")]<-"stem diameter"
clean$Trait[which(clean$Trait=="Stem dry mass per stem fresh volume (stem specific density, SSD)")]<-"stem dry mass per stem fresh volume"


#Combining datasets

colnames(clean)
colnames(BIEN_itex_data)

#taxon 
#individualID
#lat
#lon
#elev
#treatment
#day
#year
#valuekindname
#traitname
#traitvalue
#units
#comments


#Taxon
colnames(BIEN_itex_data)[which(colnames(BIEN_itex_data)=="scrubbed_species_binomial")]<-"taxon"
colnames(clean)[which(colnames(clean)=="AccSpeciesName")]<-"taxon"


#individualID
colnames(BIEN_itex_data)[which(colnames(BIEN_itex_data)=="id")]<-"id"
colnames(clean)[which(colnames(clean)=="IndividualID")]<-"id"

#lat
colnames(BIEN_itex_data)[which(colnames(BIEN_itex_data)=="latitude")]<-"latitude"
colnames(clean)[which(colnames(clean)=="Latitude")]<-"latitude"

#lon
colnames(BIEN_itex_data)[which(colnames(BIEN_itex_data)=="longitude")]<-"longitude"
colnames(clean)[which(colnames(clean)=="Longitude")]<-"longitude"

#elev
colnames(BIEN_itex_data)[which(colnames(BIEN_itex_data)=="elevation_m")]<-"elevation_m"
colnames(clean)[which(colnames(clean)=="Elevation")]<-"elevation_m"

#treatment
BIEN_itex_data$treatment<-NA
colnames(clean)[which(colnames(clean)=="Treatment")]<-"treatment"

#traitname
colnames(BIEN_itex_data)[which(colnames(BIEN_itex_data)=="trait_name")]<-"trait_name"
colnames(clean)[which(colnames(clean)=="Trait")]<-"trait_name"

#traitvalue
colnames(BIEN_itex_data)[which(colnames(BIEN_itex_data)=="trait_value")]<-"trait_value"
colnames(clean)[which(colnames(clean)=="Value")]<-"trait_value"

#units
colnames(BIEN_itex_data)[which(colnames(BIEN_itex_data)=="unit")]<-"unit"
colnames(clean)[which(colnames(clean)=="Units")]<-"unit"


#comments
colnames(BIEN_itex_data)[which(colnames(BIEN_itex_data)=="method")]<-"comment"
colnames(clean)[which(colnames(clean)=="Comments")]<-"comment"


BIEN_itex_data<-BIEN_itex_data[intersect(colnames(BIEN_itex_data),colnames(clean))]
clean<-clean[intersect(colnames(BIEN_itex_data),colnames(clean))]

combined_data<-rbind(clean,BIEN_itex_data)
rm(BIEN_itex_data,clean)

#Now, standardize units
trait_units<-unique(combined_data[,c('trait_name','unit')])
trait_units<-trait_units[order(trait_units$trait_name),]
trait_units$trait_name[duplicated(trait_units$trait_name)]

#"leaf area per leaf dry mass"
  #convert mm2/mg to m2.kg-1 
combined_data$unit[which(combined_data$unit=="mm2/mg" & combined_data$trait_name=="leaf area per leaf dry mass")]<-"m2.kg-1"

#"leaf carbon content per leaf dry mass"        
  #mg/g to mg.g-1
combined_data$unit[which(combined_data$unit=="mg/g" & combined_data$trait_name=="leaf carbon content per leaf dry mass")]<-"mg.g-1"

#"leaf carbon content per leaf nitrogen content"
combined_data$unit[which(combined_data$trait_name=="leaf carbon content per leaf nitrogen content")]<-"ratio"

#"leaf dry mass"                                
  #convert mg to g
combined_data$trait_value[which(combined_data$trait_name=="leaf dry mass" & combined_data$unit =="mg")]<- (as.numeric(combined_data$trait_value[which(combined_data$trait_name=="leaf dry mass" & combined_data$unit =="mg")])/1000)
combined_data$unit[which(combined_data$trait_name=="leaf dry mass" & combined_data$unit =="mg")]<-"g"

#"leaf dry mass per leaf fresh mass"
  #convert g/g to mg.g-1

combined_data$trait_value[which(combined_data$trait_name=="leaf dry mass per leaf fresh mass" & combined_data$unit =="g/g")]<-(as.numeric(combined_data$trait_value[which(combined_data$trait_name=="leaf dry mass per leaf fresh mass" & combined_data$unit =="g/g")])*1000)
combined_data$unit[which(combined_data$trait_name=="leaf dry mass per leaf fresh mass" & combined_data$unit =="g/g")]<-"mg.g-1"


#"leaf nitrogen content per leaf dry mass"
  # mg/g to mg.g-1
combined_data$unit[which(combined_data$trait_name=="leaf nitrogen content per leaf dry mass" & combined_data$unit =="mg/g")]<-"mg.g-1"

#"leaf phosphorus content per leaf dry mass"
  # mg/g to mg.g-1
combined_data$unit[which(combined_data$trait_name=="leaf phosphorus content per leaf dry mass" & combined_data$unit =="mg/g")]<-"mg.g-1"


#"plant flowering begin"
  #Not needed, two units that do not allow conversion

#saveRDS(object = combined_data, file = "ITEX_global_data/BIEN_and_TTT_traits_for_ITEX_species.rds")

#test<-readRDS("ITEX_global_data/BIEN_and_TTT_traits_for_ITEX_species.rds")

rm(taxa,trait_units,traitsITEX_SV_2018)

