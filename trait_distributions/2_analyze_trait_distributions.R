#Summarize and analyze trait distributions
#Brian Maitner

source("trait_distributions/r_functions/summarize_moments.R")


#Extract the trait moments from the distributions
itex_moments<-extract_moments_itex(file_directory = "trait_distributions/itex_distributions/")

saveRDS(object = itex_moments[c(1:4,6)],file = "trait_distributions/community_weighted_means.RDS")

itex_moments<-merge(x = itex_moments,y = unique(itex[c('PlotID','Treatment')]),by="PlotID")
itex_moments$Year<-as.numeric(as.character(itex_moments$Year))
itex_moments$mean<-as.numeric(as.character(itex_moments$mean))
itex_moments$var<-as.numeric(as.character(itex_moments$var))    
itex_moments$skew<-as.numeric(as.character(itex_moments$skew))    
itex_moments$kurt<-as.numeric(as.character(itex_moments$kurt))    

t4d<-ggplot()+geom_hline(yintercept = rep(0,10))+ 
  geom_abline(data = treatment4estd,mapping=aes(slope = treatment4estd$slope,intercept = treatment4estd$intercept,linetype=signif,color=int_signif),show.legend = F)+
  ylim(c(-1.5,2) )+
  xlim(c(0,4))+facet_wrap(~treatment4estd$trait,nrow = 2,ncol = 5)+ggtitle("- 5.5 degrees C")+ylab("Effect size (vs. destination)")+
  scale_colour_manual(name="Intercept",values = c("significant"="red","marginal"="green3","nonsignificant"="blue"))+
  scale_linetype_manual(name="Slope",values = c("significant"="solid","marginal"="dashed","nonsignificant"="dotted"))

  


ggplot(data = itex_moments, aes(x = Year,y =mean  )) +
  geom_point()+
  geom_smooth(method = "lm",se = T,col="grey")+facet_wrap(Treatment ~ trait,nrow = 2,ncol=5,scale="free")
  
  
  
  geom_errorbar(aes(ymin=ci_min, ymax=ci_max))+
  theme(axis.text.x = element_text(angle = -90, hjust = 0,vjust=.5))+
  scale_x_continuous(breaks = c(3000,3500,3850,4130),name = "Elevation (m)")+
  
