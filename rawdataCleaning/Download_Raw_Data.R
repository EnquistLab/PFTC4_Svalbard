#### DOWNLOAS RAW DATA FROM OSF ####

#install.packages("remotes")
#remotes::install_github("Plant-Functional-Trait-Course/PFTCFunctions")
library("PFTCFunctions")

# Download raw climate data from OSF
get_file(node = "smbqh",
         file = "Climate_Data_ITEX_2015_2018.zip",
         path = "climate/raw_data",
         remote_path = "RawData/RawData_Climate")

# Unzip files
zipFile <- "climate/raw_data/Climate_Data_ITEX_2015_2018.zip"
outDir <- "climate/raw_data/"
unzip(zipFile, exdir = outDir)


# Download raw climate data from OSF
get_file(node = "smbqh",
         file = "LeafTrait_Svalbard_with_DM.csv",
         path = "traits/data",
         remote_path = "RawData/RawData_Traits")