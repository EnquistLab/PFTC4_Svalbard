#Script to select best available traits

#inputs: species vector, site and plot, traits_df


select_traits_itex<-function(species, site, plot, traits_df){
  
  #Trait prioritizations:
  #1) same plot or 
  #2) same site
  #3) traits from the gradient
  #4) bird cliff.  
traits_output<-NULL
    
for( i in 1:length(species)){

traits_i<-NULL

traits_i<-traits[which(traits$Taxon==species[i] & traits$PlotID==plot),]  
    
if(nrow(traits_i)==0){traits_i<-traits[which(traits$Taxon==species[i] & traits$Site==site),]  }
  
if(nrow(traits_i)==0){traits_i<-traits[which(traits$Taxon==species[i] & traits$Site=="C"),]  }

if(nrow(traits_i)==0){traits_i<-traits[which(traits$Taxon==species[i]),]  }

traits_output<-rbind(traits_output,traits_i)

}
  
return(traits_output)  

}#end select_traits_itex fx
