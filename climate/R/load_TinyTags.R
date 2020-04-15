####################################
  #### CLEAN RAW TINYTAG DATA ####
####################################

# needs libraries loaded in "climate/R/Merge_ClimateData.R"

ColumnNames <- read_excel(path = "climate/data/DATA_ITEX_2015_2018/TinyTag_loggers/2005 Temp Data.xls", sheet = "DATA", col_names = FALSE) %>% 
  slice(1:5) %>% 
  t() %>% 
  as_tibble() %>% 
  slice(-52) %>% 
  rename("Location" = "V1",
         "Treatment" = "V2",
         "Type" = "V3") %>% 
  mutate(Treatment = recode(Treatment, "Control" = "CTL")) %>% 
  fill(Location) %>% 
  mutate(Treatment = paste(Location, Treatment, Type, sep = "_")) %>% 
  select(Treatment) %>%
  t() %>% 
  as_tibble()

dat <- read_excel(path = "climate/data/DATA_ITEX_2015_2018/TinyTag_loggers/2005 Temp Data.xls", sheet = "DATA", col_names = FALSE, skip = 7) %>% 
  select(-"...52")
colnames(dat) <- ColumnNames

TinyTag <- dat %>% 
  rename("DateTime" = "Location_Treatment_Soil/Surface") %>% 
  mutate(`BIS L4_OTC_surface` = as.numeric(`BIS L4_OTC_surface`)) %>% 
  pivot_longer(cols = c(-DateTime), names_to = "Treatment", values_to = "Value") %>% 
  filter(!is.na(Value)) %>% 
  filter(grepl("OTC|CTL", Treatment)) %>% 
  separate(col = Treatment, into = c("Site", "PlotID", "Treatment", "Type"), sep = " |_") %>% 
  mutate(PlotID = gsub("L", "", PlotID),
         PlotID = paste(Site, PlotID, sep = "-"),
         Logger = "TinyTag")
