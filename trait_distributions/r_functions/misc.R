#Misc functions

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


#helper function designed to calculate the ci

calc_ci<-function(x,ci=0.95){
  mean<-mean(x)
  amt_to_trim <- (1-ci)/2    
  n_to_trim<-round(length(x)*amt_to_trim)  
  x<-sort(x)
  x<-x[-1:-n_to_trim]
  x<-x[-(1+length(x)-n_to_trim  ):-length(x)]
  
  ci_min<-min(x)
  ci_max<-max(x)
  
  output<-as.data.frame(cbind(ci_min,mean,ci_max))
  
  return(output)
  
}


################################
#calculate species means


species_trait_means<-function(trait_data_frame){
  
  output<-as.data.frame(matrix(nrow = length(unique(trait_data_frame[,1])),ncol = ncol(trait_data_frame))  )
  colnames(output)<-colnames(trait_data_frame)  
  
  for(i in 1:length(unique(trait_data_frame[,1]))){
    
    output[i,1]  <- unique(trait_data_frame[,1])[i]
    
    for(t in 2:ncol(trait_data_frame)){
      
      output[i,t] <- mean(trait_data_frame[,t][which(trait_data_frame[,1]==output[i,1])],na.rm = T)
      
      
      
    }#t
    
  }#i  
  
  
  return(output)  
  
  
}