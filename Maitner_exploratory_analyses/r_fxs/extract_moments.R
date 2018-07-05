
extract_moments<-function(skinny_trait_dist){
  out_summary<-NULL
  
  for(t in 1:length(unique(skinny_trait_dist$trait))){
  trait<-as.character(unique(skinny_trait_dist$trait)[t])
  
  for(s in 1:length(unique(skinny_trait_dist$site))){
  
  site<-as.character(unique(skinny_trait_dist$site)[s])  
  
  data_ts<-as.numeric(as.character(skinny_trait_dist$value[which(skinny_trait_dist$site==site & skinny_trait_dist$trait==trait)]))
  mean<-mean(data_ts)
  variance<-var(data_ts)
  skewness<-moments::skewness(data_ts)
  kurtosis<-moments::kurtosis(data_ts)
  
  out_summary<-rbind(out_summary,cbind(trait,site,mean,variance,skewness,kurtosis))
      
    
  }#s loop
  }#t loop
  
  
  out_summary<-as.data.frame(out_summary)  
  out_summary$mean<-as.numeric(as.character(out_summary$mean)) 
  out_summary$variance<-as.numeric(as.character(out_summary$variance))
  out_summary$skewness<-as.numeric(as.character(out_summary$skewness))
  out_summary$kurtosis<-as.numeric(as.character(out_summary$kurtosis))
  return(out_summary)
  
}#end fx