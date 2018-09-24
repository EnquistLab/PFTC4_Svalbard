#Code for calculating community trait distributions

#Brian Maitner 24 9 2018

##########################################################

#Could you please calculate the community weighted trait means for the ITEX site? 
#The data is in the PFTC4_Svalbard/USE_THESE_DATA folder on google drive.

#Trait prioritizations:
#1) same plot or 
#2) same site
#3) traits from the gradient
#4) bird cliff.


####################################
#Load data,packages and functions


source("trait_distributions/r_functions/trait_distribution_fx.R")
source("trait_distributions/r_functions/select_traits.R")
library(dplyr)

load("C:/Users/Brian/Google Drive/PFTC4_Svalbard/USE_THESE_DATA/Traits/traitsGradients_SV_2018 (1).Rdata")
traits<-traits2018
rm(traits2018)
traits$mean_thickness_mm<-rowMeans(traits[c('Leaf_thickness_1_mm','Leaf_thickness_2_mm','Leaf_thickness_3_mm')],na.rm = T)
traits$wetSLA_cm2_per_g<-traits$Area_cm2/traits$Wet_mass_g


#The other files are identical...not sure if this will change?
#load("C:/Users/Brian/Google Drive/PFTC4_Svalbard/USE_THESE_DATA/Traits/traitsITEX_SV_2018 (1).Rdata")
#load("C:/Users/Brian/Google Drive/PFTC4_Svalbard/USE_THESE_DATA/Traits/traitsSAXY_SV_2018 (1).Rdata")



#load("C:/Users/Brian/Google Drive/PFTC4_Svalbard/USE_THESE_DATA/Community/communitySV_2018.Rdata") #Elevation/bird cliff data
#load("C:/Users/Brian/Google Drive/PFTC4_Svalbard/USE_THESE_DATA/Community/metaCommunitySV_2018.Rdata") #Site metadata 
load("C:/Users/Brian/Google Drive/PFTC4_Svalbard/USE_THESE_DATA/Community/CommunitySV_ITEX_2003_2015.Rdata")
itex<-CommunitySV_ITEX_2003_2015
rm(CommunitySV_ITEX_2003_2015)

#############################################################



#Clean trait/comm data if needed

#For each plotID x year

  #select best available traits
  #calc distribution
  #record distribution


for( i in 1:nrow(unique(cbind(itex$PlotID,itex$Year)))){
  
  PlotID<-unique(cbind(itex$PlotID,itex$Year))[i,1]
  Year<-unique(cbind(itex$PlotID,itex$Year))[i,2]
  Site<-unique(itex$Site[which(itex$PlotID==PlotID)])
  Treatment<-unique(itex$Treatment[which(itex$PlotID==PlotID)])

  community_i<-itex[which(itex$PlotID==PlotID & itex$Year==Year),]  
  
  traits_i<-select_traits_itex(species = unique(community_i$Taxon),site = Site,plot = PlotID,traits_df = traits)  
  traits_i<-traits_i[ c("Taxon","Plant_height_cm", "Wet_mass_g","Dry_mass_g","Area_cm2", "mean_thickness_mm","wetSLA_cm2_per_g")]
  
  #Trait distribution fx:
  
  #Inputs:
  # Number of replicated outputs
  # Species abundance dataframe (2 columns, first column: species name, second column: abundance)
  # Trait data frame (2 or more columns: first column: species name, columns 2+ : traits)
  
  #Output
  #Matrix with nrows = number of replicates, ncols = total abundance
  
  
  #Make distributions
  distribution_i<-trait_distributions(number_replicates = 200,
                                      abundance_data = cbind(community_i$Taxon,community_i$Abundance),
                                      trait_data = traits_i)  

  #Write distributions to files
  for(t in 1:length(distribution_i)){
    
    trait_t<-names(distribution_i)[[t]]    
    dist_t<-distribution_i[[t]]  
    write.csv(x = dist_t,file = paste("trait_distributions/itex_distributions/",Site,"_",PlotID,"_",Year,"_",trait_t,".csv",sep = ""),row.names = F)
    rm(trait_t,dist_t)  
    
  }#t loop
  
  rm(PlotID,Site,t,Treatment,Year,distribution_i,community_i,traits_i)
  
  
    
  print(paste(round(i/nrow(unique(cbind(itex$PlotID,itex$Year)))*100,digits = 2), " percent done",sep = ""))
    
}#i loop









