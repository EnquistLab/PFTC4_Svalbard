# Example for Xiao Feng
# Prepared by sean.michaletz@gmail.com, 13 Feb 2018


#=========================================================================================
# Part 4: TPCFitting
#=========================================================================================
#--Load TPCfitting code
source("Michaletz_2018b.R")

# Boltzmann-Arrhenius model
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



# Sharpe-Schoolfield model
# Time execution
start.time <- Sys.time()
# Execute function
load("Photosynthesis/mean_obs_data.Rdata")
formatted$rate <- "Net photosynthesis (umol m-2 s-1)"
SS_results <- TPCFit(Data=formatted, trait="Photo", ID="uniqueid", temper="Tleaf",
                     species="Taxon", traitName="rate", PLOT=TRUE, OverPLOT=FALSE, 
                     Model="Schoolfield", SchoolTpk=TRUE, rand.st=TRUE, n.rand=100)
# Calculate time taken
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
