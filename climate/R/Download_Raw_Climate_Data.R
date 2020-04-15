####################################
#### DOWNLOAD RAW DATA FROM OSF ####
####################################

# Library
#devtools::install_github("Between-the-Fjords/dataDownloader")
library("dataDownloader")

# Download zip file from OSF
get_file(node = "smbqh",
         file = "Climate_Data_ITEX_2015_2018.zip",
         path = "climate/raw_data",
         remote_path = "RawData/RawData_Climate")


# Unzip files
zipFile <- "climate/raw_data/Climate_Data_ITEX_2015_2018.zip"
outDir <- "climate/raw_data/"
unzip(zipFile, exdir = outDir)