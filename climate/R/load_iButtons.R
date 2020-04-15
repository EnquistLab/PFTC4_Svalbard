####################################
  #### CLEAN RAW IBUTTON DATA ####
####################################

# Load meta data ITEX
load("metaItex.Rdata")

# Read in raw files and clean
ibutton <- list.files("climate/data/DATA_ITEX_2015_2018/Plots_iButtons", recursive = TRUE, pattern = "\\.csv$", full.names = TRUE) %>%
  grep(pattern = "201608-201610|bis9|2016 downloads", x = ., invert = TRUE, value = TRUE) %>% 
  set_names(.) %>% 
  map_df(read.csv, skip = 19, stringsAsFactors = FALSE, .id = "files") %>% 
  #as_tibble() %>%  
  mutate(
    Date.Time = lubridate::dmy_hms(`Date.Time`),
    Type = if_else(grepl("soil", files), "soil", "surface"),
    PlotID = toupper(basename(files)),
    PlotID = str_replace(PlotID, "-", "_"),
    PlotID = str_replace(PlotID, ".CSV", ""),
    PlotID = str_replace(PlotID, "20150707_|20150925_|20150925_|20170620_|20170815_", ""),
    PlotID = str_replace(PlotID, "--SOIL|--SURF|_SOIL|-SOIL|_SURF|-SURF|_SSOIL|_SUR", ""),
    PlotID = str_replace(PlotID, "_S_15061125BUTTON41|_15061125BUTTON41", ""),
    PlotID = str_replace(PlotID, "BIS_L1L", "BIS_L1"),
    PlotID = str_replace(PlotID, "BIS_L2L", "BIS_L2"),
    PlotID = str_replace(PlotID, "BIS_L10.CSV", "BIS_L10")
  ) %>% 
  rename("DateTime" = "Date.Time") %>% 
  # Maybe those are tests, maybe I can figure out which plots are missing
  filter(!PlotID %in% c("TES_41_SEPT2015", "TEST_41_SEPT2015_2", "TEST_41_SEPT2015_3")) %>% 
  mutate(PlotID = recode(PlotID,  "BIS2" = "BIS-2")) %>% 
  mutate(PlotID = str_replace(PlotID, "_", "-"),
         PlotID = str_replace(PlotID, "-L|-H", "-"),
         PlotID = str_replace(PlotID, "DRY--5", "DRY-5")) %>% 
  # no Itex plots
  filter(!PlotID %in% c("A2", "A3", "A5", "A6", "A7", "A9", "B2", "B2L", "B3", "B5", "B6", "B7", "B8", "CL1", "CL2", "CL6", "DL6", "DL7", "DL9", "B-5", "B-6", "B-7", "B-8", "BS-1", "BS-10", "BS-2", "BS-3", "BS-5", "BS-9", "BIS-")) %>% 
  left_join(metaItex, by = "PlotID") %>% 
  select(-files) %>% 
  # remove unrealistic values
  filter(Value < 55 & Value > -25) %>% 
  mutate(Value = if_else(Type == "soil" & DateTime < "2014-09-18 00:00:00", NA_real_, Value),
         Value = if_else(Type == "surface" & DateTime < "2014-09-27 00:00:00", NA_real_, Value),
         Value = if_else(Type == "surface" & PlotID == "CAS-4" & Value < -10, NA_real_, Value),
         Value = if_else(Type == "surface" & PlotID == "BIS-2" & DateTime > "2018-07-08 00:00:00", NA_real_, Value)) %>% 
  filter(!is.na(Value)) %>% 
  mutate(Logger = "iButton") %>% 
  as_tibble()

#ibutton <- setDT(ibutton)

# Check data
# ibutton %>% 
#   filter(Type == "surface", Date.Time < "2016-01-27 00:00:00",
#          Value < 55, Value > -25) %>% 
#   filter(Site == "CAS", Treatment == "OTC") %>% 
#   ggplot(aes(x = Date.Time, y = Value, colour = PlotID)) +
#   geom_line() +
#   facet_grid( ~ PlotID)
