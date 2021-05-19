##################################
#### DOWNLOAD AND IMPORT DATA ####
##################################

### Download data
DataDownloadPlan <- drake_plan(

  # Climate
  download_PFTC_data(country = "Svalbard", 
                     datatype = "climate", 
                     path = "climate/data_clean"),
  
  # Traits
  download_PFTC_data(country = "Svalbard", 
                     datatype = "trait",
                     path = "traits/cleaned_Data/"),
  
  # Meta data (coordinates)
  download_PFTC_data(country = "Svalbard", 
                     datatype = "meta", 
                     path = "traits/cleaned_Data"),
  
  # Community
  download_PFTC_data(country = "Svalbard",
                     datatype = "community",
                     path = "community/cleaned_data/"),
  
  # C-fluxes
  get_file(node = "smbqh",
           file = "Cflux_SV_ITEX_2018.csv",
           path = "fluxes/cleaned_data/",
           remote_path = "C-Flux")
  # for some reason this does not work!!!
  # download_PFTC_data(country = "Svalbard",
  #                    datatype = "c-flux",
  #                    path = "fluxes/cleaned_data/")

)

  



# Import data
DataImportPlan <- drake_plan(
  
  # import community data
Community = read_csv(file = "community/cleaned_data/PFTC4_Svalbard_2003_2015_ITEX_Community.csv") %>% 
  # remove iced Cassiope plots
  filter(!PlotID %in% c("CH-4", "CH-6", "CH-9", "CH-10")),
  
  metaItex = Community %>%
    distinct(Site, Treatment, PlotID),
  
  #import trait data
  Traits = read_csv(file = "traits/cleaned_data/PFTC4_Svalbard_2018_ITEX_Traits.csv") %>% 
    # remove iced Cassiope plots
    filter(!PlotID %in% c("CH-4", "CH-6", "CH-9", "CH-10")) %>% 
    # remove NP ratio from data. Not part of original analysis
    filter(Trait != "NP_ratio"),
  
  
  # import height data
  Height = read_csv(file = "community/cleaned_data/PFTC4_Svalbard_2003_2015_ITEX_Vegetation_Structure.csv") %>% 
  filter(Variable == "MedianHeight_cm",
         Year == 2015) %>% 
  mutate(Site = factor(Site, levels = c("SB", "CH", "DH"))),

  
  # flux data #
  Flux_Raw = read_csv("fluxes/cleaned_data/Cflux_SV_ITEX_2018.csv"),
  
  ## process data
  ITEX.data.pre.calcs = Flux_Raw %>%
  mutate(
    #Gradient = "1",
    #BlockID = NA,
    Date = dmy(Date),     # changing Date from character to date format
    #AirTemp = NA, # need iButton data!!
    NEE = -1* NEE  ## proper terminology
  ) %>%
  select(
    #Country,
    #Year,
    #Gradient,
    #BlockID,
    #Project,
    #Site,           ## changing variable names
    PlotID,
    Treatment,
    Date,
    Time = StartTime,
    Type = Cover,
    LNrsqd = rsqd,
    nee_lm = NEE,
    SoilTemp = ST_mean,
    SoilMoist = SM_mean,
    CanTemp = IR_mean,
    #AirTemp,
    PAR = PAR_mean#,
    #Weather,
    #Notes = comment
    ),
  
  
  # import temperature and climate data
  # weather station data
  Temperature = read_csv(file = "climate/data_clean/PFTC4_Svalbard_2005_2018_ITEX_Temperature.csv"),
  Climate = read_csv(file = "climate/data_clean/PFTC4_Svalbard_2015_2018_ITEX_Climate.csv"),
  
)
