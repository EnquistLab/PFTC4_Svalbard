#=========================================================================================
# Arrhenius fits of photosynthesis temperature response
#=========================================================================================
# Prepared by Sean Michaletz (sean.michaletz@gmail.com), May 2017

# << INTRODUCTION >> ---------------------------------------------------------------------
source("Michaletz_2018b.R")

psyn <- res
#--Add columns to identify individual temperature response curves in SUMO and RMBL data

psyn <- transform(psyn, curveID = as.integer(factor(uniqueid, unique(uniqueid))))
# Count number of unique curves in literature, SUMO, and RMBL data
#length(unique(lit$CurveID))
#length(unique(SUMO$curveID))
#length(unique(RMBL$curveID))
#--Renumber SUMO and RMBL curveID values so numbering begins continuous after literature numbers
#SUMO$curveID <- formatted$uniqueid + length(unique(uniqueid$curveID))
#RMBL$curveID <- RMBL$curveID + length(unique(lit$CurveID)) + length(unique(SUMO$curveID))


#=========================================================================================
# Contents
#=========================================================================================

#  Part 1:  Photosynthesis temperature response curves (standard)
#  Part 2:  Arrhenius plots 
#  Part 4:  TPCFitting



#=========================================================================================
# Part 1: Photosynthesis temperature response curves (standard)
#=========================================================================================

#--Plot all curves on a single set of axes.
ggplot(psyn, aes(x=Tleaf_C, y=A_umol_m2_s)) + 
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x,2,raw=TRUE), 
              aes(colour=factor(curveID),linetype=factor(curveID)), data=psyn) +
  geom_point(aes(color=as.factor(curveID)), data=psyn) + 
  xlab(expression('Leaf temperature'~(degree*C))) + 
  ylab(expression("Assimilation rate (" * mu ~ "mol" ~m^-2 ~s^-1 * ")")) +
  theme_bw(base_size=12) + 
  theme(legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_colour_manual(values=rep(cbPalette,times=22)) +
  # plot each 22 times to maximize solids (there are 9 colors)
  scale_linetype_manual(values=rep(c(1:6),each=33)) 

#--Plot all curves on a single set of axes (curves only).
ggplot(psyn, aes(x=Tleaf_C, y=A_umol_m2_s)) + 
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x,2,raw=TRUE), 
              aes(colour=factor(curveID)), data=psyn) +
  xlab(expression('Leaf temperature'~(degree*C))) + 
  ylab(expression("Assimilation rate (" * mu ~ "mol" ~m^-2 ~s^-1 * ")")) +
  theme_bw(base_size=12) + 
  theme(legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_colour_manual(values=rep("gray60", times=197))

#--Plot all curves on individual axes.
for(i in seq_along(unique(psyn$curveID))) {
  print(
    ggplot(subset(psyn, psyn$curveID==i), aes(x=Tleaf_C, y=A_umol_m2_s)) + 
      stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x,2,raw=TRUE),
                  aes(colour=factor(curveID)), data=subset(psyn, psyn$curveID==i)) +
      geom_point(aes(color=as.factor(curveID)), data=subset(psyn, psyn$curveID==i)) + 
      xlab(expression('Leaf temperature'~(degree*C))) + 
      ylab(expression("Assimilation rate (" * mu ~ "mol" ~m^-2 ~s^-1 * ")")) +
      theme_bw(base_size=12) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black"))
  )
}

#--Plot a single curve (e.g., curveID = 47).
ggplot(subset(psyn, psyn$curveID=="47"), aes(x=Tleaf_C, y=A_umol_m2_s)) + 
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x,2,raw=TRUE),aes(colour=factor(curveID))) +
  geom_point(aes(color=as.factor(curveID))) + 
  xlab(expression('Leaf temperature'~(degree*C))) + 
  ylab(expression("Assimilation rate (" * mu ~ "mol" ~m^-2 ~s^-1 * ")")) +
  theme_bw(base_size=12) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))


#=========================================================================================
# Part 2: Arrhenius plots
#=========================================================================================

#--Plot all curves on a single set of axes.
ggplot(psyn, aes(x=invBT_eV, y=A_umol_m2_s)) + 
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x,2,raw=TRUE), 
              aes(colour=factor(curveID),linetype=factor(curveID)), data=psyn) +
  geom_point(aes(color=as.factor(curveID)), data=psyn) + 
  xlab(expression(paste('Leaf temperature ', '<1/',italic('kT'),'>', ' (',  eV^{-1}, ')'))) +
  ylab(expression("Assimilation rate (" * mu ~ "mol" ~m^-2 ~s^-1 * ")")) +
  scale_y_continuous(trans="log", breaks = trans_breaks("log", function(x) exp(x), n=3), 
                     labels = trans_format("log", math_format(e^.x))) +
  theme_bw(base_size=12) + 
  theme(legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_colour_manual(values=rep(cbPalette,times=22)) +
  # plot each 22 times to maximize solids (there are 9 colors)
  scale_linetype_manual(values=rep(c(1:6),each=33)) 

#--Plot all curves on a single set of axes (curves only).
ggplot(psyn, aes(x=invBT_eV, y=A_umol_m2_s)) + 
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x,2,raw=TRUE), 
              aes(colour=factor(curveID)), data=psyn) +
  xlab(expression(paste('Leaf temperature ', '<1/',italic('kT'),'>', ' (',  eV^{-1}, ')'))) + 
  ylab(expression("Assimilation rate (" * mu ~ "mol" ~m^-2 ~s^-1 * ")")) +
  scale_y_continuous(trans="log", breaks = trans_breaks("log", function(x) exp(x), n=3), 
                     labels = trans_format("log", math_format(e^.x))) +
  theme_bw(base_size=12) + 
  theme(legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_colour_manual(values=rep("gray60", times=197))

#--Plot curves 1 to 100 on individual axes.
for(i in seq_along(1:100)) {
  print(
    ggplot(subset(psyn, psyn$curveID==i), aes(x=invBT_eV, y=A_umol_m2_s)) + 
      stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x,2,raw=TRUE),
                  aes(colour=factor(curveID)), data=subset(psyn, psyn$curveID==i)) +
      geom_point(aes(color=as.factor(curveID)), data=subset(psyn, psyn$curveID==i)) + 
      xlab(expression(paste('Leaf temperature ', '<1/',italic('kT'),'>', ' (',  eV^{-1}, ')'))) +  
      ylab(expression("Assimilation rate (" * mu ~ "mol" ~m^-2 ~s^-1 * ")")) +
      scale_y_continuous(trans="log", breaks = trans_breaks("log", function(x) exp(x), n=3), 
                         labels = trans_format("log", math_format(e^.x))) +
      theme_bw(base_size=12) + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black"))
  )
}

#--Plot curves 101 to 197 on individual axes.
for(i in seq_along(101:197)) {
  print(
    ggplot(subset(psyn, psyn$curveID==i), aes(x=invBT_eV, y=A_umol_m2_s)) + 
      stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x,2,raw=TRUE),
                  aes(colour=factor(curveID)), data=subset(psyn, psyn$curveID==i)) +
      geom_point(aes(color=as.factor(curveID)), data=subset(psyn, psyn$curveID==i)) + 
      xlab(expression(paste('Leaf temperature ', '<1/',italic('kT'),'>', ' (',  eV^{-1}, ')'))) +  
      ylab(expression("Assimilation rate (" * mu ~ "mol" ~m^-2 ~s^-1 * ")")) +
      scale_y_continuous(trans="log", breaks = trans_breaks("log", function(x) exp(x), n=3), 
                         labels = trans_format("log", math_format(e^.x))) +
      theme_bw(base_size=12) + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black"))
  )
}

#--Plot a single curve (e.g., curveID = 47).
ggplot(subset(psyn, psyn$curveID=="47"), aes(x=invBT_eV, y=A_umol_m2_s)) + 
  stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x,2,raw=TRUE),aes(colour=factor(curveID))) +
  geom_point(aes(color=as.factor(curveID))) + 
  xlab(expression(paste('Leaf temperature ', '<1/',italic('kT'),'>', ' (',  eV^{-1}, ')'))) +
  ylab(expression("Assimilation rate (" * mu ~ "mol" ~m^-2 ~s^-1 * ")")) +
  scale_y_continuous(trans="log", breaks = trans_breaks("log", function(x) exp(x), n=3), 
                     labels = trans_format("log", math_format(e^.x))) +
  theme_bw(base_size=12) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))




#=========================================================================================
# Part 4: TPCFitting
#=========================================================================================
#--Load TPCfitting code
#source("./code from Pawar & Dell/TPCFitting_v3_stm_fixed.R")

#--TPCFitting_v3_stm.R
# Load small test dataset
#dataForSophia <- read.csv("E:/Documents/Tansley_medal/manuscript/Arrhenius_analyses/code from Pawar & Dell/dataForSophia.csv", header=T)
#--Add column for processID
psyn$rate <- "Net photosynthesis (umol m-2 s-1)"

SS_results<-TPCFit(Data=psyn,trait="Photo",ID="curveID",temper="Tleaf",
       species="Taxon",traitName="rate",PLOT=TRUE,OverPLOT=FALSE,Model="Schoolfield",
       SchoolTpk=TRUE,rand.st=TRUE, n.rand=100)



# Use entire dataset


# Boltzmann
# Time execution
#start.time <- Sys.time()
# Execute function
#BA_results <- TPCFit(Data=psyn, trait="A_umol_m2_s", ID="curveID", temper="Tleaf_C",
#                     species="Taxon", traitName="rate", PLOT=TRUE, OverPLOT=FALSE, 
#                     Model="Boltzmann", SchoolTpk=TRUE, rand.st=TRUE, n.rand=100)
# Calculate time taken
#end.time <- Sys.time()
#time.taken <- end.time - start.time
#time.taken
# n.rand=100 took 1.49667 mins
#median(Boltzmann$E_Boltz, na.rm=T)


# Schoolfield
# Time execution
start.time <- Sys.time()
# Execute function
SS_results <- TPCFit(Data=psyn, trait="A_umol_m2_s", ID="curveID", temper="Tleaf_C",
                     species="Taxon", traitName="rate", PLOT=TRUE, OverPLOT=FALSE, 
                     Model="Schoolfield", SchoolTpk=TRUE, rand.st=TRUE, n.rand=100)
# Calculate time taken
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# n.rand=100 took 2.358905 mins
# n.rand=1000 took 20.06794 mins; this made no difference for median E (0.2918334 for 100 vs. 0.2918333 for 1000)

# Add in grouping variables from psyn dataframe
SS_results$Taxon <- psyn$Taxon[match(SS_results$id, psyn$curveID)]
SS_results$Genus <- psyn$Genus[match(SS_results$id, psyn$curveID)]
SS_results$Species <- psyn$Species[match(SS_results$id, psyn$curveID)]
SS_results$Pathway <- psyn$Pathway[match(SS_results$id, psyn$curveID)]
SS_results$Site <- psyn$Site[match(SS_results$id, psyn$curveID)]
SS_results$Treatment <- psyn$Treatment[match(SS_results$id, psyn$curveID)]
SS_results$Source <- psyn$Source[match(SS_results$id, psyn$curveID)]
SS_results$SourceCompiled <- psyn$SourceCompiled[match(SS_results$id, psyn$curveID)]
SS_results$SourcePrimary <- psyn$SourcePrimary[match(SS_results$id, psyn$curveID)]

# Calculate Q10 for 15-20 C (Section 1.3.5, p. 21 in Kooijman 3rd)
# see p. 119 in black book
SS_results$Q10 <- exp(10*SS_results$E_sch/(0.00008617*(25+273.15)*(10+25+273.15)))

# Subset only 1) C3 and 2) curves that are not absolutely terrible/invalid/no_rise (p. 118 black book)
SS_results2 <- subset(SS_results, SS_results$Pathway=="C3" & 
                        SS_results$id!="28" & SS_results$id!="47" & SS_results$id!="48" & 
                        SS_results$id!="49" & SS_results$id!="60" & SS_results$id!="62" & 
                        SS_results$id!="68" & SS_results$id!="79" & SS_results$id!="94" & 
                        SS_results$id!="97" & SS_results$id!="119" & SS_results$id!="108" & 
                        SS_results$id!="132" & SS_results$id!="137" & SS_results$id!="148" & 
                        SS_results$id!="149" & SS_results$id!="154" & SS_results$id!="155" & 
                        SS_results$id!="157" & SS_results$id!="164" & SS_results$id!="166" & 
                        SS_results$id!="175" & SS_results$id!="177" & SS_results$id!="182" & 
                        SS_results$id!="194")

# Remove all curveID that were not fit
SS_results2 <- subset(SS_results2, !is.na(E_sch))

# Count number of unique leaves and species
length(unique(SS_results2$id))
length(unique(SS_results2$id_spp))

# 95% CI for each leaf
SS_results2$CIlow <- SS_results2$E_sch-1.96*SS_results2$E_SE_sch
SS_results2$CIhi <- SS_results2$E_sch+1.96*SS_results2$E_SE_sch
SS_results2$DiffFromPred <- ifelse("0.32" > SS_results2$CIlow,
                                   ifelse("0.32" < SS_results2$CIhi, 0, 1), 1) # 1=is different from 0.32, 0 is not different 
# What percent of leaves are different from predicted 0.32 ev:
sum(SS_results2$DiffFromPred)/length(unique(SS_results2$id))

# Write data for Table(s)
#write.csv(SS_results2, "SS_results2.csv")
# Number per species
#write.csv(data.frame(table(SS_results2$id_spp)), "SS_results2_SPP_FREQ.csv")

# Calculate species mean value
sppMeanE <- ddply(SS_results2, .(Taxon), summarize,  E_sch=mean(E_sch, na.rm=T), Q10=mean(Q10, na.rm=T))
# Calculate species median value
sppMedianE <- ddply(SS_results2, .(Taxon), summarize,  E_sch=median(E_sch, na.rm=T), Q10=median(Q10, na.rm=T))

#--All samples: mean (95% CI) and median (95% CI)
# This is biased in favor of spp w/ many measured leaves
# Mean (0.53); 0.47 for fixed
mean(SS_results2$E_sch)
# Mean 95% CI (0.43 to 0.64); (0.36 to 0.59) for fixed
mean(SS_results2$E_sch)-1.96*(sd(SS_results2$E_sch)/sqrt(length(SS_results2$E_sch)))
mean(SS_results2$E_sch)+1.96*(sd(SS_results2$E_sch)/sqrt(length(SS_results2$E_sch)))
# Median (0.30); 0.22 for fixed
median(SS_results2$E_sch)
# Median 95% CI (0.23 to 0.37); (0.17 to 0.27) for fixed see https://stats.stackexchange.com/questions/184516/why-is-the-95-ci-for-the-median-supposed-to-be-%C2%B11-57iqr-sqrtn
median(SS_results2$E_sch)-1.57*IQR(SS_results2$E_sch)/sqrt(length(SS_results2$E_sch))
median(SS_results2$E_sch)+1.57*IQR(SS_results2$E_sch)/sqrt(length(SS_results2$E_sch))

#--Species means: mean (95% CI) and median (95% CI)
# This is a way to avoid biasing towards highly-sampled spp
# Mean (0.74); 0.64 for fixed
mean(sppMeanE$E_sch)
# Mean 95% CI (0.57 to 0.91); (0.43 to 0.86) for fixed
mean(sppMeanE$E_sch)-1.96*(sd(sppMeanE$E_sch)/sqrt(length(sppMeanE$E_sch)))
mean(sppMeanE$E_sch)+1.96*(sd(sppMeanE$E_sch)/sqrt(length(sppMeanE$E_sch)))
# Median (0.72); 0.42 for fixed
median(sppMeanE$E_sch)
# Median 95% CI (0.55 to 0.89); (0.27 to 0.56) for fixed see https://stats.stackexchange.com/questions/184516/why-is-the-95-ci-for-the-median-supposed-to-be-%C2%B11-57iqr-sqrtn
median(sppMeanE$E_sch)-1.57*IQR(sppMeanE$E_sch)/sqrt(length(sppMeanE$E_sch))
median(sppMeanE$E_sch)+1.57*IQR(sppMeanE$E_sch)/sqrt(length(sppMeanE$E_sch))

#--Species medians: mean (95% CI) and median (95% CI)
# This is a way to avoid biasing towards highly-sampled spp
# Mean (0.63); 0.54 for fixed
mean(sppMedianE$E_sch)
# Mean 95% CI (0.46 to 0.80); (0.31 to 0.76) for fixed
mean(sppMedianE$E_sch)-1.96*(sd(sppMedianE$E_sch)/sqrt(length(sppMedianE$E_sch)))
mean(sppMedianE$E_sch)+1.96*(sd(sppMedianE$E_sch)/sqrt(length(sppMedianE$E_sch)))
# Median (0.47); 0.27 for fixed
median(sppMedianE$E_sch)
# 95% CI (0.32 to 0.62); (0.14 to 0.39) for fixed see https://stats.stackexchange.com/questions/184516/why-is-the-95-ci-for-the-median-supposed-to-be-%C2%B11-57iqr-sqrtn
median(sppMedianE$E_sch)-1.57*IQR(sppMedianE$E_sch, na.rm=T)/sqrt(length(sppMedianE$E_sch))
median(sppMedianE$E_sch)+1.57*IQR(sppMedianE$E_sch, na.rm=T)/sqrt(length(sppMedianE$E_sch))



# Mean Q10
mean(sppMeanE$Q10, na.rm=T)
# Median Q10
median(sppMedianE$Q10, na.rm=T)
# Range of Q10
min(sppMedianE$Q10, na.rm=T)
max(sppMedianE$Q10, na.rm=T)

#--Plots
#--Fig S1: Plot all fitted curves together
# First, use aggregate to get min and max Tleaf for each leaf, then convert result to dataframe
minMax <- aggregate(Tleaf_C ~ curveID, psyn, function(x) c(Tl_C_min = min(x), Tl_C_max = max(x)))
minMax <- cbind(minMax[-ncol(minMax)], minMax[[ncol(minMax)]])
# Define function to make sequence between min and max Tleaf_C (including max)
# see https://stackoverflow.com/questions/28419281/missing-last-sequence-in-seq-in-r; My function
# was modified to just add the max value "to" to the end of the sequence, so cases
# that already end on that will have a repeated value (which is OK)
seqlast <- function (from, to, by) 
{
  vec <- do.call(what = seq, args = list(from, to, by))
  return(c(vec, to))
}

# Make sequence of Tleaf between min and max for each leaf
SS_fits <- data.frame(id=rep(minMax$curveID, minMax$Tl_C_max-minMax$Tl_C_min+2), 
                      #Tleaf_C=unlist(mapply(seq, minMax$Tl_C_min, minMax$Tl_C_max))); rm(minMax)
                      Tleaf_C=unlist(mapply(seqlast, minMax$Tl_C_min, minMax$Tl_C_max, 1))); rm(minMax)
# Merge Tleaf sequence with fitted parameter values from SS_results2 (this will omit bad curves)
SS_fits = merge(SS_fits, SS_results2, by = "id")
# Make Tleaf_K
SS_fits$Tleaf_K <- SS_fits$Tleaf_C+273.15
# Now use Schoolfield function with TPCFit results to give fitted psyn value for each Tleaf for each curveID
SS_fits$Apred_umol_m2_s <- exp(Schoolfield(lnB0 = SS_fits$lnB0_sch, E = SS_fits$E_sch, E_D = SS_fits$E_D_sch,
                                           T_h = SS_fits$T_h_sch, temp = SS_fits$Tleaf_K, SchoolTpk = TRUE))
# Plot standard temperature response curves
ggplot(SS_fits, aes(Tleaf_K-273.15, Apred_umol_m2_s, group=factor(id))) + 
  geom_line() +                           
  xlab(expression(paste("Leaf temperature (", degree, C, ")"))) + 
  ylab(expression("Assimilation rate (" * mu ~ "mol" ~m^-2 ~s^-1 * ")")) +
  theme_bw(base_size=12) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))
# Plot modified Arrhenius plot (Tleaf_C on bottom)
ggplot(SS_fits, aes(Tleaf_K-273.15, Apred_umol_m2_s, group=factor(id))) +
  geom_line(alpha=0.4) +                           
  xlab(expression(paste("Leaf temperature (", degree, C, ")"))) + 
  ylab(expression("Assimilation rate (" * mu ~ "mol" ~m^-2 ~s^-1 * ")")) +
  scale_x_continuous(sec.axis = sec_axis(trans = ~ 1/(0.00008617*(.+273.15)) , name = expression(paste('Leaf temperature ', '1/',italic('kT'), ' (',  eV^{-1}, ')')))) +
  scale_y_continuous(trans="log", breaks = trans_breaks("log", function(x) exp(x), n=3), 
                     labels = trans_format("log", math_format(e^.x))) +
  theme_bw(base_size=12) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))
# Plot modified Arrhenius plot (1/kT on bottom) [USE THIS]
FigS1b <- ggplot(SS_fits, aes(1/(0.00008617*(Tleaf_C + 273.15)), Apred_umol_m2_s, group=factor(id))) +
  geom_line(alpha=0.4) +                           
  xlab(expression(paste('Leaf temperature ', '1/',italic('kT'), ' (',  eV^{-1}, ')'))) + 
  ylab(expression("Assimilation rate (" * mu ~ "mol" ~m^-2 ~s^-1 * ")")) +
  scale_x_continuous(sec.axis = sec_axis(trans = ~ (1/(.*0.00008617))-273.15 , name = expression(paste("Leaf temperature (", degree, C, ")")))) +
  scale_y_continuous(trans="log", breaks = trans_breaks("log", function(x) exp(x), n=3), 
                     labels = trans_format("log", math_format(e^.x))) +
  theme_bw(base_size=12) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))
FigS1b

# Plot a single curve with data and S-S fit (curveID=131)
FigS1a <- ggplot(subset(psyn, psyn$curveID=="131"), aes(x=1/(0.00008617*(Tleaf_C+273.15)), y=A_umol_m2_s)) + 
  geom_point(shape = 21, size = 2.75, col = "black", fill = "white") +
  geom_line(data = subset(SS_fits, SS_fits$id=="131"), 
            aes(x = 1/(0.00008617*(Tleaf_K)), y = Apred_umol_m2_s), alpha=0.4) +
  xlab(expression(paste('Leaf temperature ', '1/',italic('kT'), ' (',  eV^{-1}, ')'))) +
  ylab(expression("Assimilation rate (" * mu ~ "mol" ~m^-2 ~s^-1 * ")")) +
  scale_x_continuous(sec.axis = sec_axis(trans = ~ (1/(.*0.00008617))-273.15 , name = expression(paste("Leaf temperature (", degree, C, ")")))) +
  scale_y_continuous(trans="log", breaks = trans_breaks("log", function(x) exp(x), n=3), 
                     labels = trans_format("log", math_format(e^.x))) +
  theme_bw(base_size=12) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))
FigS1a

# Plot Fig. S1 panels together
grid.arrange(arrangeGrob(FigS1a,FigS1b, ncol=2, nrow=1))


#Plot E w/ 95% CI for all leaves
ggplot(SS_results2, aes(x=Taxon, y=E_sch)) + 
  geom_point(shape = 21, size = 2.75, col = "black", fill = "white") + 
  geom_hline(aes(yintercept=0.32)) + 
  geom_errorbar(aes(ymin=E_sch-1.96*E_SE_sch, ymax=E_sch+1.96*E_SE_sch), 
                width=.2, position=position_dodge(.9)) + scale_y_continuous(limits = c(0,3.5)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# E boxplots by taxon
ggplot(SS_results2, aes(x=id_spp, y=E_sch, fill=id_spp)) + 
  geom_boxplot() + 
  guides(fill=FALSE) + 
  geom_hline(aes(yintercept=0.32))  + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25))

# Topt boxplots by taxon
ggplot(SS_results2, aes(x=id_spp, y=(T_pk_sch-273.15))) + 
  geom_boxplot(fill="grey") + 
  guides(fill=FALSE) +  
  #scale_y_continuous(limits = c(0,45)) +
  ylab(expression(paste('Optimal temperature for photosynthesis (', degree, C, ')'))) +
  xlab(expression("Taxon")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25)) +
  coord_flip() +
  theme_bw(base_size=12) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))


#--Other random plots
ggplot(SS_results2, aes(E_sch)) + geom_freqpoly(binwidth = 0.05)

ggplot(SS_results2, aes(E_sch)) + 
  geom_freqpoly(binwidth = 0.1) + 
  geom_vline(aes(xintercept=mean(SS_results2$E_sch, na.rm=T)), color="red", linetype="dashed", size=1) +
  xlab(expression("Activation energy (eV)")) + 
  ylab(expression("Frequency"))

ggplot(SS_results2, aes(E_sch)) + 
  geom_histogram(binwidth=0.1) + 
  geom_vline(aes(xintercept=mean(SS_results2$E_sch, na.rm=T)), linetype="dashed") +
  geom_vline(aes(xintercept=median(SS_results2$E_sch, na.rm=T)), linetype="dashed") +
  xlab(expression("Activation energy (eV)")) + 
  ylab(expression("Frequency"))

ggplot(SS_results2, aes(E_sch)) + 
  geom_density() + 
  xlab(expression("Activation energy (eV)")) + 
  ylab(expression("Probability density"))

ggplot(SS_results2, aes(E_sch)) + 
  geom_histogram(aes(y=..density..), binwidth=.1, colour="black", fill="white") + # Histogram with density instead of count on y-axis
  geom_density(alpha=.2, fill="#FF6666") 

#--Fig. S2
# Plot species means (helps prevent sampling bias)
ggplot(sppMeanE, aes(E_sch)) + 
  #geom_histogram(aes(y=..density..), binwidth=.1, colour="black", fill="white") + # Histogram with density instead of count on y-axis
  #geom_density(alpha=.2, fill="#FF6666") + 
  geom_density(alpha=0.9, fill="#33CC00") + 
  #geom_vline(aes(xintercept=mean(sppMeanE$E_sch, na.rm=T)), linetype="dashed", color="black") +
  geom_vline(aes(xintercept=median(sppMeanE$E_sch, na.rm=T)), linetype="dashed", color="black") +
  scale_x_continuous(sec.axis = sec_axis(trans = ~ exp(10*./(0.00008617*(25+273.15)*(10+25+273.15))) , name = expression(Q[10]))) +
  xlab(expression("Activation energy (eV)")) + 
  ylab(expression("Probability density")) +
  theme_bw(base_size=12) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))

#--New Fig. 1???
ggplot(SS_results2, aes(E_sch)) + 
  geom_density(alpha=0.4, fill="#00BA38") + 
  geom_density(data=sppMeanE, alpha=0.4, fill="#619CFF") + 
  geom_density(data=sppMedianE, alpha=0.4, fill="#F8766D") +
  geom_vline(aes(xintercept= 0.32), linetype="dashed", color="black") +
  geom_vline(aes(xintercept=mean(sppMeanE$E_sch)), linetype="dashed", color="#619CFF") +
  geom_vline(aes(xintercept=median(sppMedianE$E_sch)), linetype="dashed", color="#F8766D") +
  scale_x_continuous(sec.axis = sec_axis(trans = ~ exp(10*./(0.00008617*(25+273.15)*(10+25+273.15))) , 
                                         name = expression(Q[10]), breaks = c(1, 3.5, 12.5, 44.2))) +
  xlab(expression("Activation energy (eV)")) + 
  ylab(expression("Probability density")) +
  theme_bw(base_size=12) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))

#--Fig. 1
# Plot species medians [USE THIS]
ggplot(sppMedianE, aes(E_sch)) + 
  #geom_histogram(aes(y=..density..), binwidth=.1, colour="black", fill="white") + # Histogram with density instead of count on y-axis
  #geom_density(alpha=.2, fill="#FF6666") + 
  geom_density(alpha=0.9, fill="#33CC00") + 
  #geom_vline(aes(xintercept=mean(sppMeanE$E_sch, na.rm=T)), linetype="dashed", color="black") +
  geom_vline(aes(xintercept=median(sppMedianE$E_sch, na.rm=T)), linetype="dashed", color="black") +
  scale_x_continuous(sec.axis = sec_axis(trans = ~ exp(10*./(0.00008617*(25+273.15)*(10+25+273.15))) , name = expression(Q[10]))) +
  xlab(expression("Activation energy (eV)")) + 
  ylab(expression("Probability density")) +
  theme_bw(base_size=12) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))

# Q10
ggplot(sppMedianE, aes(Q10)) + 
  #geom_histogram(aes(y=..density..), binwidth=.1, colour="black", fill="white") + # Histogram with density instead of count on y-axis
  #geom_density(alpha=.2, fill="#FF6666") + 
  geom_density(alpha=.2, fill="#00CC33") + 
  #geom_vline(aes(xintercept=mean(sppMeanE$Q10, na.rm=T)), linetype="dashed", color="black") +
  geom_vline(aes(xintercept=median(sppMedianE$Q10, na.rm=T)), linetype="dashed", color="black") +
  xlab(expression("Q10")) + 
  ylab(expression("Probability density")) +
  theme_bw(base_size=12) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))

# Compare distributions across sites
ggplot(SS_results2, aes(x=E_sch, fill=Site)) + geom_density(alpha=.3)
ggplot(subset(SS_results2, !is.na(SS_results2$Site)), aes(x=E_sch, fill=Site)) + geom_density(alpha=.3)
ggplot(subset(SS_results2, !is.na(SS_results2$Site) & SS_results2$Site!="SUMO"), aes(x=E_sch, fill=Site)) + geom_density(alpha=.3)

# SUMO - Heat appears to do nothing, drought/HD reduce E (broader curve), reduce Topt, increase A_opt
# Activation energy
# Combine A and CC since no differences
SS_results2$Treatment <- ifelse(SS_results2$Treatment!='CC', as.character(SS_results2$Treatment), 'A')
ggplot(subset(SS_results2, SS_results2$Site=="SUMO"), aes(x=E_sch, fill=Taxon)) + geom_density(alpha=.3)
# E_sch
ggplot(subset(SS_results2, SS_results2$Site=="SUMO"), aes(x=E_sch, fill=Treatment)) + 
  geom_density(alpha=.3) + 
  facet_grid(Treatment ~ .) +
  xlab(expression("Apparent activation energy (eV)")) + 
  ylab(expression("Probability density"))
ggplot(subset(SS_results2, SS_results2$Site=="SUMO"), aes(x=Treatment, y=E_sch, fill=Treatment)) + 
  geom_boxplot(alpha=0.3) 
fit1 = lm(formula = SS_results2$E_sch ~ SS_results2$Treatment)
anova(fit1) # E does not vary among groups
# T_opt
ggplot(subset(SS_results2, SS_results2$Site=="SUMO"), aes(x=T_pk_sch, fill=Treatment)) + 
  geom_density(alpha=.3) + 
  facet_grid(Treatment ~ .) +
  xlab(expression("Optimal temperature for photosynthesis (degrees C)")) + 
  ylab(expression("Probability density"))
ggplot(subset(SS_results2, SS_results2$Site=="SUMO"), aes(x=Treatment, y=T_pk_sch, fill=Treatment)) + 
  geom_boxplot(alpha=0.3) 
fit2 = lm(formula = SS_results2$T_pk_sch ~ SS_results2$Treatment)
anova(fit2) # Topt does not vary among groups
# A_opt
ggplot(subset(SS_results2, SS_results2$Site=="SUMO"), aes(x=P_pk_sch, fill=Treatment)) + 
  geom_density(alpha=.3) + 
  facet_grid(Treatment ~ .) +
  xlab(expression("Peak photosynthesis rate (umol m-2 s-1)")) + 
  ylab(expression("Probability density"))
ggplot(subset(SS_results2, SS_results2$Site=="SUMO"), aes(x=Treatment, y=P_pk_sch, fill=Treatment)) + 
  geom_boxplot(alpha=0.3) 
fit3 = lm(formula = SS_results2$P_pk_sch ~ SS_results2$Treatment)
anova(fit3) # Aopt does not vary among groups

# Compare distributions between my data and literature data
ggplot(SS_results2, aes(x=E_sch, fill=Source)) + geom_density(alpha=.3)

# Boxplot
ggplot(SS_results2, aes(x=Source, y=E_sch, fill=Source)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip()

# Topt for RMBL
# [[[NOTE THAT THE FOLLOWING INCLUDES CURVES THAT NEVER REACH PEAK - THESE NEED REMOVED!]]]
# First, order plots for plotting
SS_results2$Site <- factor(SS_results2$Site, levels = c('NA','SUMO','Almont', 'Road', 'Pfeiler', 'Painter Boy'),ordered = TRUE)
# Create celcius temperature
SS_results2$T_pk_C <- SS_results2$T_pk - 273.15
# Plot
ggplot(subset(SS_results2, !is.na(SS_results2$Site) & SS_results2$Site!="SUMO"), aes(x=Site, y=T_pk_C, fill=Site)) + 
  geom_boxplot() + 
  guides(fill=FALSE) + 
  coord_flip() + 
  xlab(expression("Site")) + 
  ylab(expression("Optimal temperature for photosynthesis (C)"))
# Test w/ ANOVA
aov1 <- aov( T_pk_C ~ Site, data=subset(SS_results2, !is.na(SS_results2$Site) & SS_results2$Site!="SUMO"))
summary(aov1)
# p = 0.00434 so means are not equal across groups
# Multiple comparisons
TukeyHSD(aov1, conf.level = 0.95)

# Aopt for RMBL
ggplot(subset(SS_results2, !is.na(SS_results2$Site) & SS_results2$Site!="SUMO"), aes(x=Site, y=P_pk_sch, fill=Site)) + 
  geom_boxplot() + 
  guides(fill=FALSE) + 
  coord_flip() + 
  xlab(expression("Site")) + 
  ylab(expression("Peak photosynthesis rate (umol m-2 s-1)"))

# E for RMBL
ggplot(subset(SS_results2, !is.na(SS_results2$Site) & SS_results2$Site!="SUMO"), aes(x=Site, y=E_sch, fill=Site)) + 
  geom_boxplot() + 
  guides(fill=FALSE) + 
  coord_flip() +
  xlab(expression("Site")) + 
  ylab(expression("Apparent activaiton energy (eV)"))


## Generic temperature response curve (Fig. S1)
# Subset data to do a Boltzmann fit
#BA_subset <- subset(psyn, psyn$curveID=="142" & psyn$Tleaf_C<20)
# Make predcicted y-values based on running B-A code from above
#BA_subset$Apred_umol_m2_s=exp(9.362984)*exp(-0.1909955/(0.00008617*(test$Tleaf_C+273.15)))
# Plot
ggplot(subset(psyn, psyn$curveID=="142"), aes(x=Tleaf_C, y=A_umol_m2_s)) + 
  geom_point(shape = 21, size = 2.75, col = "black", fill = "white") + 
  #geom_line(data = BA_subset, aes(x = Tleaf_C, y = Apred_umol_m2_s), color="black", linetype="solid", size=0.5) +
  geom_line(data = subset(SS_fits, SS_fits$id=="142"), aes(x = Tleaf_C, y = Apred_umol_m2_s), alpha=0.4, linetype="solid", size=0.5) +
  xlab(expression(paste("Temperature ", italic("T"), " (", degree, C, ")"))) +
  ylab(expression(paste("Biological rate ", italic("B")))) +
  #scale_x_continuous(sec.axis = sec_axis(trans = ~ 1/(0.00008617*(.+273.15)) , name = expression(paste('Temperature ', '1/',italic('kT'), ' (',  eV^{-1}, ')')))) +
  theme_bw(base_size=12) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))

# Arrhenius plot
ggplot(subset(psyn, psyn$curveID=="142"), aes(x=1/(0.00008617*(Tleaf_C+273.15)), y=A_umol_m2_s)) + 
  geom_point(shape = 21, size = 2.75, col = "black", fill = "white") + 
  geom_smooth(data = subset(SS_fits, SS_fits$id=="142" & SS_fits$Tleaf_C<20), 
              aes(x = 1/(0.00008617*(Tleaf_K)), y = Apred_umol_m2_s),
              method="lm", alpha=0.4, linetype="solid", color="black", size=0.5, se=F) +
  geom_line(data = subset(SS_fits, SS_fits$id=="142"), 
            aes(x = 1/(0.00008617*(Tleaf_K)), y = Apred_umol_m2_s), alpha=0.4, linetype="dashed", size=0.5) +
  xlab(expression(paste('Temperature ', '1/',italic('kT'), ' (',  eV^{-1}, ')'))) +
  ylab(expression("Biological rate")) +
  scale_x_continuous(sec.axis = sec_axis(trans = ~ (1/(.*0.00008617))-273.15 , name = expression(paste("Temperature ", italic("T"), " (", degree, C, ")")))) +
  scale_y_continuous(trans="log", breaks = trans_breaks("log", function(x) exp(x), n=3), 
                     labels = trans_format("log", math_format(e^.x))) +
  theme_bw(base_size=12) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))

# Data from Table 3, Parent and Tardieu (2016)
df <- data.frame(E=c(65.1, 65.1, 80, 89.6, 63.6, 61.7, 70.3, 60, 73.9, 73.6, 73.9, 81.7, 78.9, 60.6, 52.3, 75.2, 60.4, 63.3))
ggplot(df, aes(x=E, fill=Source)) + geom_density(alpha=.3)