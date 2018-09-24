#Summarize and analyze trait distributions
#Brian Maitner

source("trait_distributions/r_functions/summarize_moments.R")


#Extract the trait moments from the distributions
itex_moments<-extract_moments_itex(file_directory = "trait_distributions/itex_distributions/")

saveRDS(object = itex_moments[c(1:4,6)],file = "trait_distributions/community_weighted_means.RDS")

