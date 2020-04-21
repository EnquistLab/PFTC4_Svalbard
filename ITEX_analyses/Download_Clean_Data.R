#### DOWNLOAD CLEAN DATA FROM OSF ####

#install.packages("remotes")
#remotes::install_github("Plant-Functional-Trait-Course/PFTCFunctions")
library("PFTCFunctions")

#### CLIMATE ####
# Weather station
get_file(node = "smbqh",
         file = "ItexSvalbard_Climate_2015_2018.csv",
         path = "climate/data_clean/",
         remote_path = "Climate")

# iButton station
get_file(node = "smbqh",
         file = "ItexSvalbard_Temp_2005_2015.csv",
         path = "climate/data_clean/",
         remote_path = "Climate")

# Traits
get_file(node = "smbqh",
         file = "ItexSvalbard_Temp_2005_2015.csv",
         path = "traits/cleaned_Data/",
         remote_path = "Traits")

# Community
get_file(node = "smbqh",
         file = "ITEX_Svalbard_2003_2015_Community_cleaned.csv",
         path = "community/cleaned_data/",
         remote_path = "Community")

