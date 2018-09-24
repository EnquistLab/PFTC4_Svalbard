#Extract moments summary stats from trait distributions

source("trait_distributions/r_functions/calc_ci.R")
library(moments)

extract_moments_itex<-function(file_directory){
  
files<-list.files(file_directory,full.names = T)  
  
out_summary<-NULL


for( i in 1:length(files)){

  
file_i<-files[i]    
file_name_i <- strsplit(x = file_i,split = "/")[[1]][length(strsplit(x = file_i,split = "/")[[1]]  )]  
file_name_i<-strsplit(file_name_i,split = ".csv")[[1]][1]
Site<-unlist(strsplit(file_name_i,".",fixed = T))[1]

PlotID<-unlist(strsplit(file_name_i,".",fixed=T))[2]
trait<-unlist(strsplit(file_name_i,".",fixed = T))[4]
Year<-unlist(strsplit(file_name_i,".",fixed = T))[3]

data_i<-read.csv(file_i)

means<-rowMeans(data_i)
vars<-apply(X = data_i,MARGIN = 1,FUN = var)
skews<-apply(X = data_i,MARGIN = 1,FUN = skewness)
kurts<-apply(X = data_i,MARGIN = 1,FUN = kurtosis)

mean_info<-calc_ci(means)
var_info<-calc_ci(vars)
skew_info<-calc_ci(skews)
kurt_info<-calc_ci(kurts)


#record output

out_i<-cbind(Site,PlotID,Year,trait,
      mean_info$ci_min,mean_info$mean,mean_info$ci_max,
      var_info$ci_min,var_info$mean,var_info$ci_max,
      skew_info$ci_min,skew_info$mean,skew_info$ci_max,
      kurt_info$ci_min,kurt_info$mean,kurt_info$ci_max)


colnames(out_i)<-c("Site","PlotID","Year","trait","mean_lower","mean","mean_upper",
                "var_lower","var","var_upper",
                "skew_lower","skew","skew_upper",
                "kurt_lower","kurt","kurt_upper")

out_summary<-rbind(out_summary,out_i)  
  
}#for i loop

  
  
 return(as.data.frame(out_summary) )
  
}
