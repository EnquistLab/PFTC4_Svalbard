library(tidyverse)
library(readr)
library(readxl)

source("Photosynthesis/licor_read_files.R")

# put files for analysis into same folder as code

# read in raw licor files and parse file name (format: 0000-id-ddmmyy) into two new columns (id and date)
licor <- list.files(path = "Photosynthesis/25072018_licor_files/", full.names = TRUE) %>% 
  set_names(basename(.)) %>% 
  map_df(read_licor, .id = "run") %>% 
  separate(run, into = c("zero","uniqueid", "date"), sep = "-") %>% 
  select(-zero)

# read in leaf area data, fix any estimates over 6cm
leaf_area <- read_csv('Photosynthesis/leafarea.csv') %>% 
  select(-X1) %>% 
  mutate(
    total.leaf.area = if_else(total.leaf.area > 6, 6, total.leaf.area),
    uniqueid = substring(sample, 1, 7)
    )

# read in the species names of each leaf run by unique id
metadat <- read_excel("Photosynthesis/Photo_data_sheets_filled.xlsx", sheet = 1)

anti_join(licor, leaf_area, by = "uniqueid") %>%
  ungroup() %>% 
  distinct(uniqueid)
anti_join(leaf_area, licor, by = "uniqueid")

licor_leaf <- inner_join(licor, leaf_area, by = "uniqueid") %>% inner_join(metadat, by = c("uniqueid" = "Filename"))

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
res %>% 
  group_by(uniqueid, level, Taxon, Site) %>% 
  summarise_at(vars(StmRat:VpdL), .funs = mean) %>% 
  ggplot(aes(x = Tleaf, y = Photo, colour = Site, group = uniqueid)) + 
  geom_point() + 
#  geom_line() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), show.legend = FALSE, se = FALSE) +
  facet_wrap(~ Taxon)

# need to select  first 15 data points (instead of last) for AAF
