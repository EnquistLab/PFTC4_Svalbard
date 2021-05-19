###############################
#### DATA PAPER DRAKE PLAN ####
###############################

library("drake")

r_make(source = "ITEX_analyses/R/ITEX_DrakePlan.R")
loadd()
drake_failed()

#view dependency graph
r_vis_drake_graph(source = "ITEX_analyses/R/ITEX_DrakePlan.R", targets_only = TRUE)
