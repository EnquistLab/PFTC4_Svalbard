# *actual data* 
  #(vegetation composition & abundances, 
  #geolocation, 
  #altitude, 
  #some envt data, sometimes including actual measured temperatures....) 
#for the 
  #KB altitudinal gradient, 
  #ÅP goose grubbing (which includes an altitudinal gradient..) and 
  #ISJ ITEX plots

#! We need to figure out ... 
  #(i) if there are patterns in species composition and ?traits along the altitudinal / temperature gradients / with treatments, 
  #(ii) what kinds of species / leaf numbers  we can expect / should plan for, 
  #(iii) if we have climate gradients with altitude (Kathrin used lapse rates, what do the data say......)  

#@Maitner can you  pls play with 
  #(i) for the Åshild Pedersen (goose)
  #Katrhrin Bockmuehl (alt) 
  #ITEX data?

#Anybody voluntering to chk (ii) or (iii)?

############################################################

############
#Ashild Pedersen Goose grubbing

library(xlsx)
library(funrar)
library(BIEN)

goose_data<-read.xlsx("C:/Users/Brian/Dropbox/PFTC4 Svalbard data/Åshild Pedersen's Goose grubbing gradients/Copy of Vegatation_data_against_factors_12_02_2014 (3).xlsx",1)
colnames(goose_data)
colnames(goose_data)<-gsub(pattern = "Ranculus.spp.",replacement = "Ranunculus.spp.",x = colnames(goose_data))
goose_comm<-goose_data[c("Bistorta.vivipara","Carex.","Cassiope.","Cerastium.spp.","Cochlearia.groenlandica","Cryptogamic.crust","Draba.spp.","Dryas.octopetala",
"Equisetum.spp..","Eriophorum.scheuchzeri","Graminoids..incl..Juncus.","Herbs","Lichen","Luzula.spp.","Moss","Oxyria.digyna","Papaver.dahlianum",
"Ranunculus.spp.","Salix.polaris","Saxifraga.cernua.","Saxifraga.cespitosa","Saxifraga.oppositifolia","Silene.acaulis","Stellaria.crassipes")]
rownames(goose_comm)<-goose_data$Altitude

goose_comm<-funrar::matrix_to_stack(goose_comm)
names(goose_comm)[1]<-"species"
names(goose_comm)[2]<-"elevation"
head(goose_comm)
unique(goose_comm$species)
goose_comm$species<-gsub(goose_comm$species,pattern = ".",replacement = " ",fixed = T)
goose_comm_sla<-BIEN_trait_mean(species = unique(goose_comm$species),trait = "leaf area per leaf dry mass")
goose_comm_seedmass<-BIEN_trait_mean(species = unique(goose_comm$species),trait = "seed mass")
goose_comm_ldmc<-BIEN_trait_mean(species = unique(goose_comm$species),trait = "leaf dry mass per leaf fresh mass")

goose_traits<-goose_comm_ldmc[c('species','mean_value')]
colnames(goose_traits)[2]<-"ldmc"
goose_traits<-merge(x = goose_traits,y = goose_comm_sla[c("species","mean_value")],by="species")
colnames(goose_traits)[3]<-"sla"
goose_traits<-merge(x = goose_traits,y = goose_comm_seedmass[c("species","mean_value")],by="species")
colnames(goose_traits)[4]<-"seed_mass"
goose_traits$ldmc<-as.numeric(as.character(goose_traits$ldmc))
goose_traits$sla<-as.numeric(as.character(goose_traits$sla))
goose_traits$seed_mass<-as.numeric(as.character(goose_traits$seed_mass))
rm(goose_comm_ldmc,goose_comm_seedmass,goose_comm_sla)
goose_traits[2:4]<-scale(goose_traits[2:4])

source("C:/Users/Brian/Desktop/current_projects/PFTC4_Svalbard/Maitner_exploratory_analyses/r_fxs/abundance_trait_distributions.R")
source("C:/Users/Brian/Desktop/current_projects/PFTC4_Svalbard/Maitner_exploratory_analyses/r_fxs/trait_distribution_fx.R")
source("C:/Users/Brian/Desktop/current_projects/PFTC4_Svalbard/Maitner_exploratory_analyses/r_fxs/extract_moments.R")

#Inputs:
#Number of replicated outputs
# Species abundance dataframe (2 columns, first column: species name, second column: abundance)
# Trait data frame (2 or more columns: first column: species name, columns 2+ : traits)

#Output
#Matrix with nrows = number of replicates, ncols = total abundance
goose_trait_dists<-abundance_trait_distributions(number_replicates = 1,trait_data = goose_traits,tidy_abundance = goose_comm)

library(ggplot2)

goose_trait_dists$value<-as.numeric(as.character(goose_trait_dists$value))
goose_moments<-extract_moments(skinny_trait_dist = goose_trait_dists)
goose_moments$site<-as.numeric(as.character(goose_moments$site))


ggplot(goose_moments, aes(x=site, y=mean)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE)+facet_wrap(~trait)

summary(lm(goose_moments$mean[which(goose_moments$trait=="ldmc")]~goose_moments$site[which(goose_moments$trait=="ldmc")]))
summary(lm(goose_moments$mean[which(goose_moments$trait=="sla")]~goose_moments$site[which(goose_moments$trait=="sla")]))
summary(lm(goose_moments$mean[which(goose_moments$trait=="seed_mass")]~goose_moments$site[which(goose_moments$trait=="seed_mass")]))

########################
#Alt gradient

alt_gradient<-read.xlsx("C:/Users/Brian/Dropbox/PFTC4 Svalbard data/Katrin Bochmuhl altitudinal gradients/die drei Gradienten.xls",1)

