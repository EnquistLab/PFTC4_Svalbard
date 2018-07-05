#code to take a species x site matrix, and species x trait mean matrix and produce distributions

#Input: 
      # species x site x abundance table (tidy format) (species site abundance)
      # trait table

#Output:
      # tidy format distributions


abundance_trait_distributions<-function(number_replicates, trait_data, tidy_abundance){
  
  names(trait_data)[1]<-"Taxon"
  
  output<-NULL  
  
  for(i in 1:length(unique(tidy_abundance[,2]))){
    
    site<-as.character(unique(tidy_abundance[,2])[i])
    data_i<-tidy_abundance[which(tidy_abundance[,2]==site),]
    data_i<-data_i[,c(1,3)]
    data_i<-data_i[which(data_i[,2]>0),]
    if(length(which(trait_data$Taxon%in%data_i[,1]))>0){
    traits_i<-trait_distributions(number_replicates = number_replicates,abundance_data = data_i,trait_data = trait_data)
    
    
    
    
    for(t in 1:length(traits_i)){
    trait<-names(traits_i)[t]  
      
    
    for(r in 1:number_replicates){
    
      output<-rbind(output,cbind(r,site,trait,traits_i[[t]][r,]))  
        
      
      
    }#rep loop
    
    
    
      
    }#t loop
    
    }#ifthere are traits
    
  }
  
  colnames(output)[1]<-"replicate"
  output<-as.data.frame(output)
  colnames(output)[4]<-"value"
  return(output)
  
  
}

