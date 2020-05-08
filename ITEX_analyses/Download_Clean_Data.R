#### DOWNLOAD CLEAN DATA FROM OSF ####

#install.packages("remotes")
#remotes::install_github("Plant-Functional-Trait-Course/PFTCFunctions")
library("PFTCFunctions")

# Climate
download_PFTC_data(country = "Svalbard", 
                   datatype = "climate", 
                   path = "climate/data_clean")

# Traits
download_PFTC_data(country = "Svalbard", 
                   datatype = "trait",
                   path = "traits/cleaned_Data/")

# Meta data (coordinates)
download_PFTC_data(country = "Svalbard", 
                   datatype = "meta", 
                   path = "traits/cleaned_Data")

# Community
download_PFTC_data(country = "Svalbard", 
                   datatype = "community", 
                   path = "community/cleaned_data/")

