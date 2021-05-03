##################################
#### DOWNLOAD AND IMPORT DATA ####
##################################

### Download data
DataDownloadPlan <- drake_plan(
  # Climate
  climate_download = target(download_PFTC_data(country = "Svalbard", 
                                               datatype = "climate", 
                                               path = "cleaned_Data/climate")),
  
  # Traits
  trait_download = target(download_PFTC_data(country = "Svalbard", 
                                             datatype = "trait",
                                             path = "cleaned_Data/traits")),
  
  # Meta data (coordinates)
  meta_download = target(download_PFTC_data(country = "Svalbard", 
                                            datatype = "meta", 
                                            path = "cleaned_Data/meta")),
  
  # Community
  community_download = target(download_PFTC_data(country = "Svalbard", 
                                                 datatype = "community", 
                                                 path = "cleaned_Data/community")),
  
)

  



# Import data
DataImportPlan <- drake_plan(
  
  # climate
  climate_itex = read_csv(file = "cleaned_data/climate/ItexSvalbard_Climate_2015_2018.csv"),
  temperature_itex = read_csv(file = "cleaned_data/climate/ItexSvalbard_Temp_2005_2015.csv"),
  
  # community
  community_itex = read_csv(file = "cleaned_data/community/ITEX_Svalbard_2003_2015_Community_cleaned.csv"),
  communit_gradient = read_csv(file = "cleaned_data/community/PFTC4_Svalbard_2018_Community_cleaned.csv") %>% 
    filter(!is.na(Gradient)),
  
  # meta
  coordinates = read_excel(path = "cleaned_data/meta/PFTC4_Svalbard_Coordinates.xlsx"),
  
  # traits
  traits_itex = read_csv(file = "cleaned_data/traits/PFTC4_Svalbard_2018_ITEX.csv") %>% 
    select(Year, Treatment, Site:Leaf_Area_Total_cm2, P_percent, C_percent:dC13_permil, Flag, Comment, Flag_corrected, Remark_CN) %>% 
    pivot_longer(cols = Plant_Height_cm:dC13_permil, names_to = "Traits", values_to = "Value") %>% 
    filter(!is.na(Treatment)),
  
  traits_gradient = read_csv(file = "cleaned_data/traits/PFTC4_Svalbard_2018_TraitsGradients.csv") %>% 
    select(Year, Treatment:Leaf_Area_Total_cm2, P_percent, C_percent:dC13_permil, Flag, Comment, Flag_corrected, Remark_CN) %>% 
    pivot_longer(cols = Plant_Height_cm:dC13_permil, names_to = "Traits", values_to = "Value") %>% 
    filter(!is.na(Treatment)),
  
  traits_bryophytes = read_csv(file = "cleaned_data/traits/PFTC4_Svalbard_2018_Bryo_TraitsGradients.csv"),
  traits_saxy = read_csv(file = "cleaned_data/traits/PFTC4_Svalbard_Traits_2018_Saxy.csv")
  
)
