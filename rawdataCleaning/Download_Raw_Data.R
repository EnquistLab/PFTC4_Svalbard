#### DOWNLOAD RAW DATA FROM OSF ####

#install.packages("remotes")
#remotes::install_github("Plant-Functional-Trait-Course/PFTCFunctions")
library("PFTCFunctions")
library("googlesheets4")

#### CLIMATE ####
# Raw climate data
download_PFTC_data(country = "Svalbard", 
                   datatype = "raw_climate", 
                   path = "climate/raw_data")

# Unzip files
zipFile <- "climate/raw_data/Climate_Data_ITEX_2015_2018.zip"
outDir <- "climate/raw_data/"
unzip(zipFile, exdir = outDir)


#### META DATA ####
# Meta data
download_PFTC_data(country = "Svalbard", 
                   datatype = "meta", 
                   path = "traits/cleaned_Data")


#### TRAITS ####
# Raw leaf trait data
download_PFTC_data(country = "Svalbard", 
                   datatype = "raw_traits", 
                   path = "traits/data")

# Unzip Leaf Scans
zipFile <- "traits/data/PFTC4_Svalbard_2018_LeafScans.zip"
outDir <- "traits/data/"
unzip(zipFile, exdir = outDir)

# NEED TO ADD CHEMICAL LEAF TRAITS !!!!


#### COMMUNITY ####
# Raw community data
download_PFTC_data(country = "Svalbard", 
                   datatype = "raw_community", 
                   path = "community/data")
