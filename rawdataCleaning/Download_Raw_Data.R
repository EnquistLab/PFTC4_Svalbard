#### DOWNLOAD RAW DATA FROM OSF ####

#install.packages("remotes")
#remotes::install_github("Plant-Functional-Trait-Course/PFTCFunctions")
library("PFTCFunctions")

#### CLIMATE ####
# Raw climate data
get_file(node = "smbqh",
         file = "Climate_Data_ITEX_2015_2018.zip",
         path = "climate/raw_data",
         remote_path = "RawData/RawData_Climate")

# Unzip files
zipFile <- "climate/raw_data/Climate_Data_ITEX_2015_2018.zip"
outDir <- "climate/raw_data/"
unzip(zipFile, exdir = outDir)


#### META DATA ####
# Meta data
get_file(node = "smbqh",
         file = "PFTC4_Svalbard_Coordinates.xlsx",
         path = "traits/cleaned_Data",
         remote_path = "MetaData")


#### TRAITS ####
# Raw leaf trait data
get_file(node = "smbqh",
         file = "LeafTrait_Svalbard_with_DM.csv",
         path = "traits/data",
         remote_path = "RawData/RawData_Traits")

# Raw leaf area data
get_file(node = "smbqh",
         file = "PFTC4_Scalbard_Raw_LeafArea_2018.csv",
         path = "traits/data",
         remote_path = "RawData/RawData_Traits")

# Leaf Scans
get_file(node = "smbqh",
         file = "PFTC4_Svalbard_2018_LeafScans.zip",
         path = "traits/data",
         remote_path = "RawData/RawData_Traits")

# Unzip files
zipFile <- "traits/data/PFTC4_Svalbard_2018_LeafScans.zip"
outDir <- "traits/data/"
unzip(zipFile, exdir = outDir)

# NEED TO ADD CHEMICAL LEAF TRAITS !!!!


#### COMMUNITY ####
# Raw community data
get_file(node = "smbqh",
         file = "PFTC4_Svalbard_2018_Community.csv",
         path = "community/data",
         remote_path = "RawData/RawData_Community")

# Draba dictionary
get_file(node = "smbqh",
         file = "PFTC4_Svalbard_2018_Draba_dictionary.xlsx",
         path = "community/data",
         remote_path = "RawData/RawData_Community")