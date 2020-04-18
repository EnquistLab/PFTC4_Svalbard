####################################
#### DOWNLOAD RAW DATA FROM OSF ####
####################################

# Download zip file from OSF
download_PFTC_data(country = "Svalbard", 
                   datatype = "raw_climate", 
                   path = "climate/raw_data")

# Unzip files
zipFile <- "climate/raw_data/Climate_Data_ITEX_2015_2018.zip"
outDir <- "climate/raw_data/"
unzip(zipFile, exdir = outDir)