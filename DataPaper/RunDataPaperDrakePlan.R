###############################
#### DATA PAPER DRAKE PLAN ####
###############################

library("drake")

r_make(source = "DataPaper/R/DataPaperDrakePlan.R")
loadd()
failed()

#view dependency graph
r_vis_drake_graph(source = "DataPaper/R/DataPaperDrakePlan.R", targets_only = TRUE)
