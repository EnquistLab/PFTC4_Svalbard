library(tidyverse)
library(readr)
library(readxl)

source("photosynthesis/code/licor_read_files.R")

# read in raw licor files and parse file name (format: 0000-id-ddmmyy) into two new columns (id and date)
licor <- list.files(path = "photosynthesis/data/25072018_licor_files/", full.names = TRUE) %>% 
  set_names(basename(.)) %>% 
  map_df(read_licor, .id = "run") %>% 
  separate(run, into = c("zero","uniqueid", "date"), sep = "-") %>% 
  select(-zero) %>%
  group_by(uniqueid, variable, parameter, level, unit) %>%
  summarise_all(.funs = mean)
licor$date <- as.character(licor$date)

peru_licor <- list.files( path ="photosynthesis/data/peru/", full.names = TRUE) %>% 
  set_names(basename(.)) %>%
  map_df(read_licor, .id = "run") %>% 
  separate(run, into = c("date","initials", "uniqueid"), sep = "-") %>% 
  select(-initials) %>%
  group_by(uniqueid, variable, parameter, level, unit) %>%
  summarise_all(.funs = mean)

china_licor <- read_csv("photosynthesis/data/DataMaster_2015-2016_Finse.csv") %>%
  rename(uniqueid = Filename, date = Date) %>%
  select(-sample, -Notes, -Site, -Taxon) 

licor <- bind_rows(svalbard=licor, peru=peru_licor, china=china_licor, .id = "place")

# read in leaf area data, fix any estimates over 6cm
leaf_area <- read_csv('photosynthesis/data/leafarea.csv') %>% 
  select(-X1) %>% 
  mutate(
    total.leaf.area = if_else(total.leaf.area > 6, 6, total.leaf.area),
    uniqueid = substring(sample, 1, 7)
    )%>%
  select(-sample)

leaf_area_peru <- read_csv('photosynthesis/data/leafareaperu.csv') %>% 
  select(-X1) %>% 
  mutate(
    total.leaf.area = if_else(total.leaf.area > 6, 6, total.leaf.area),
    uniqueid = substring(sample, 1, 7)
  ) %>%
  select(-sample)

 leaf_area_china <- read_excel('photosynthesis/data/leafareachina.xlsx')  %>% 
   select(`Area from whole leaf scan`, Filename) %>% 
   rename(total.leaf.area = `Area from whole leaf scan`, uniqueid = Filename) 
 leaf_area_china$total.leaf.area <- 6 # temporary until leaf area data is ready
  # mutate( total.leaf.area = if_else(total.leaf.area > 6, 6, total.leaf.area),
    # uniqueid = substring(sample, 1, 7)) %>%
  # select(-sample)

leaf_area <- bind_rows(leaf_area, leaf_area_peru, leaf_area_china)

# read in the species names of each leaf run by unique id
metadat <- read_excel("photosynthesis/data/Photo_data_sheets_filled.xlsx", sheet = 1) %>%
select(Taxon, Filename, Site)
metadat_peru <- read_excel("photosynthesis/data/peru_photo_data_sheets.xlsx", sheet = 2) %>%
  select(Taxon, `Sample ID`, Site) %>%
  rename(Filename = `Sample ID`)
metadat_china <- read_excel('photosynthesis/data/leafareachina.xlsx') %>%
  select(Taxon, Filename, Site)
metadat <- bind_rows(metadat, metadat_peru, metadat_china)

anti_join(licor, leaf_area, by = "uniqueid") %>%
  ungroup() %>% 
  distinct(uniqueid)
anti_join(leaf_area, licor, by = "uniqueid")

licor_leaf <- inner_join(licor, leaf_area, by = "uniqueid") %>% inner_join(metadat, by = c("uniqueid" = "Filename")) #%>%
  #rename(Taxon = Taxon.x = Taxon.y, Site = Site.x = Site.y)

# calculate extra leaf related metrics which scale from basic set of measurements
recalc_licor <- . %>% 
  select(-(Photo:CTleaf), -BLC_1, -BLCond) %>% 
  mutate(
    
    BLC_1 = Area*BLCslope+BLCoffst,
    fda = Flow * 0.000001 / (total.leaf.area * 0.0001),
    Photo = (CO2R - CO2S * (1000 - H2OR) / (1000 - H2OS)) * fda, 
    Trans = (H2OS-H2OR)/(1000-H2OS)*fda,
    Tair_K = Tleaf + 273.15,
    Twall_K = Tair + 273.15,
    `R(W/m2)` = (PARi*f_parin + PARo*f_parout)*alphaK,
    `Tl-Ta` = ((`R(W/m2)`+ 0.00000010773*(Twall_K^4-Tair_K^4))-Trans*44100)/(BLC_1*51.4+0.00000043092*Tair_K^3),
    CTleaf = Tleaf+`Tl-Ta`*`EBal?`,
    SVTleaf = 0.61365*exp(17.502*CTleaf/(240.97+CTleaf)),
    h2o_i = SVTleaf*1000/Press, 
    h2odiff = h2o_i-H2OS, 
    CTair = if_else(`EBal?` != 0, Tleaf, (Tair+Tleaf)/2),
    SVTair = 0.61365*exp(17.502*CTair/(240.97+CTair)),
    CndTotal = if_else(h2odiff!=0, (1000-(h2o_i+H2OS)/2)/h2odiff*Trans, 0), 
    vp_kPa = H2OS*Press/1000,
    VpdA = SVTair-vp_kPa,
    BLCond = BLC_1*(StmRat+1)*(StmRat+1)/(StmRat*StmRat+1),
    Cond = if_else(CndTotal!=0, 1/(1/CndTotal-1/BLCond),0),
    CndCO2 = 1/(1.6/Cond+1.37/BLCond),
    Ci = ((CndCO2-Trans/2)*CO2S-Photo)/(CndCO2+Trans/2),
    Ci_Pa = Ci*Press*0.001,
    `Ci/Ca` = Ci/CO2S,
    RHsfc = (1-Trans*Press/SVTleaf/Cond)*100,
    C2sfc = CO2S-Photo/(BLCond/1.35),
    `AHs/Cs` = Photo*RHsfc/100/C2sfc,
    Trmmol = Trans*1000,
    VpdL = SVTleaf-vp_kPa
  )

# confirm calculated data matches expected from excel sheet for one run
res <- licor_leaf %>% recalc_licor
res %>% filter(uniqueid == "AAF6802") %>% select(Photo, HHMMSS)

# plot leaf temperature/photosynthetic response by nutrient site
formatted <- res %>% 
  ggplot(aes(x = Tleaf, y = Photo, colour = place, group = uniqueid)) + 
  geom_point() + 
#  geom_line() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), show.legend = FALSE, se = FALSE) +
  facet_wrap(~ Taxon)

# need to select  first 15 data points (instead of last) for AAF
