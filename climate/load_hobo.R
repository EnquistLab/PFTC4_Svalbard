#LOAD LIBRARIES#

library("tidyverse")
library("lubridate")
library("readxl")
library("ggplot2")

#read cvs file#
dat <- read.csv(file = "climate/data/DATA_ITEX_2015-18/Plots_iButtons/2015 downloads/iButton 07072015/20150707_Dry-L4-soil.csv",header=TRUE, skip = 19) %>% 
  rename(
    date = `Date`,
    time = `Time, GMT+00:00`,
    sol_rad = `Solar Radiation, W/m² (LGR S/N: 10654923, SEN S/N: 10456179)`,
    temp = `Temp, °C (LGR S/N: 10654923, SEN S/N: 10655577)`,
    rh = `RH, % (LGR S/N: 10654923, SEN S/N: 10655577)`,
    par = `PAR, uE (LGR S/N: 10654923, SEN S/N: 10659182)`,
    wat_cont = `Water Content, m³/m³ (LGR S/N: 10654923, SEN S/N: 10659684)`
  ) %>% 
  mutate(date_time=paste(date, time),  date_time = mdy_hms(date_time)) %>% 
  select(-`#`, -date, -time) %>% 
  mutate_at(vars(-date_time), as.numeric)

dat <- dat %>%
  mutate(Date.Time = dmy_hms(Date.Time)) 

#read excel file#
data <- read_excel(path = "climate/data/DATA_ITEX_2015-18/Hobo_weather st/2015 downloads/NNW/20150925/20150925_Wether_station_NNW_.xls", skip = 1, col_names = TRUE) 


ggplot(data, mapping = aes( x = date_time, y = temp)) +
  geom_point() + 
  geom_line()

####hobo weather stations####


read_xl_csv <- function(f){
  if(grepl("csv$", f)){
    read.csv(f, skip = 1, stringsAsFactors = FALSE, check.names = FALSE)
  } else if(grepl("xls", f)){
    read_excel(f, skip = 1, sheet = 1)
  } else {
    stop("unknown file type")
  }
}

# read_xl_csv( f = "my_file.csv")

hobo <- list.files(path = "climate/data/DATA_ITEX_2015-18/Hobo_weather st/", recursive = TRUE, full.names = TRUE, pattern = "csv$|\\.xls") %>% 
  grep(pattern = "SSE", x = ., value = TRUE) %>% 
  grep(pattern = "~",x = ., invert = TRUE, value = TRUE) %>% 
  set_names(.) %>% 
  map(read_xl_csv) %>% 
  map(select, -`#`) %>% 
  map(mutate, `PAR, uE (LGR S/N: 2271742, SEN S/N: 2008892, LBL: Weatherstn_SSE)` = gsub(",", "", `PAR, uE (LGR S/N: 2271742, SEN S/N: 2008892, LBL: Weatherstn_SSE)`)) %>% 
  map_df(mutate_at, .vars = vars(-matches("Date|Time")), .fun = as.numeric, .id = "file") %>% 
  mutate(
    date_time = if_else(!is.na(Date),paste(Date,  `Time, GMT+00:00`), NA_character_),
    date_time = coalesce(date_time, `Date Time, GMT+00:00`), 
    date_time = lubridate::mdy_hms(date_time)) %>% 
  select(-Date, -`Date Time, GMT+00:00`, -`Time, GMT+00:00`)%>% 
  
  rename(
    par = `PAR, uE (LGR S/N: 2271742, SEN S/N: 2008892, LBL: Weatherstn_SSE)`,
    wat_cont1 = `Water Content, m³/m³ (LGR S/N: 2271742, SEN S/N: 2268271, LBL: Weatherstn_SSE)`,
    temp1 = `Temp, °C (LGR S/N: 2271742, SEN S/N: 10655574, LBL: Weatherstn_SSE)`,
    rh = `RH, % (LGR S/N: 2271742, SEN S/N: 10655574, LBL: Weatherstn_SSE)`,
    sol_rad1 = `Solar Radiation, W/m² (LGR S/N: 2271742, SEN S/N: 10663215, LBL: Weatherstn_SSE)`,
    wat_cont2 = `Water Content, mÂ³/mÂ³ (LGR S/N: 2271742, SEN S/N: 2268271, LBL: Weatherstn_SSE)`, 
    temp2 = `Temp, Â°C (LGR S/N: 2271742, SEN S/N: 10655574, LBL: Weatherstn_SSE)`,
    sol_rad2 = `Solar Radiation, W/mÂ² (LGR S/N: 2271742, SEN S/N: 10663215, LBL: Weatherstn_SSE)`) %>% 
  
  mutate(
    wat_cont = coalesce(`wat_cont1`, `wat_cont2`), 
    temp = coalesce(temp1, temp2),
    sol_rad = coalesce(sol_rad1, sol_rad2),
    wat_cont = coalesce(wat_cont1, wat_cont2)) %>% 
  select(-matches('\\d')) %>% 
  mutate(
    wat_cont = if_else(wat_cont < -100, NA_real_, wat_cont)
  )


#end read files :)

list.files(path = "climate/data/DATA_ITEX_2015-18/Hobo_weather st/", recursive = TRUE, full.names = TRUE, pattern = "\\.xls|\\.csv$") %>% 
  grep(pattern = "SSE", x = ., value = TRUE) %>% 
  grep(pattern = "~",x = ., invert = TRUE, value = TRUE) %>% 
  map(~{print(.);read_excel(., skip = 1, sheet = 1)})

#col_types = c("text", ))


read_excel("climate/data/DATA_ITEX_2015-18/Hobo_weather st//2016 dwonloads/SSE/20160425/Endalen_Weatherstn_SSE_20160425.xls")


# Plotting data
head(hobo)
library(ggplot2)

ggplot(data = hobo)+
  geom_point(mapping = aes(x = date_time, y = par, color = file), show.legend = FALSE)

ggplot(data = hobo)+
  geom_point(mapping = aes(x = date_time, y = wat_cont, color = file), show.legend = FALSE)

ggplot(data = hobo)+
  geom_point(mapping = aes(x = date_time, y = rh, color = file), show.legend = FALSE)

ggplot(data = hobo)+
  geom_point(mapping = aes(x = date_time, y = temp, color = file), show.legend = FALSE)

ggplot(data = hobo)+
  geom_point(mapping = aes(x = date_time, y = sol_rad, color = file), show.legend = FALSE)

#### Calculating monthly hobo means ####

names (hobo)


day_temp_hobo <- hobo %>% 
  filter(!is.na(temp)) %>%
  separate(col = "date_time", into= c("date", "time"), sep = " ") %>% 
  group_by(file, date) %>% 
  summarise(n = n(), dayTemp = mean(temp))

day_temp_hobo

month_temp_hobo <- day_temp_hobo %>% 
  filter(!is.na(dayTemp)) %>% 
  ungroup() %>% 
  mutate_at(vars("date"), .fun = ymd) %>%  
  mutate(date = dmy(paste0("15-", format(date , "%b.%Y")))) %>% # sets all the dates of the month to the 15th.
  group_by(file, date) %>% 
  summarise(n = n(), monthTemp = mean(dayTemp))

month_temp_hobo
names(month_temp_hobo)

ggplot(data = month_temp_hobo)  + 
  geom_point(mapping = aes(x = date, y = monthTemp))


####hobo stations in plots####

read_xl_csv <- function(f){
  if(grepl("csv$", f)){
    read.csv(f, skip = 1, stringsAsFactors = FALSE, check.names = FALSE)
  } else if(grepl("xls", f)){
    read_excel(f, skip = 1, sheet = 1)
  } else {
    stop("unknown file type")
  }
}


hobo_st <- list.files(path = "climate/data/DATA_ITEX_2015-18/Hobo_st_plots/", recursive = TRUE, full.names = TRUE, pattern = "csv$|\\.xls") %>% 
  grep(pattern = "calculations", x = ., invert = TRUE, value = TRUE) %>% 
  grep(pattern = "Northface", x = ., invert = TRUE, value = TRUE) %>% 
  grep(pattern = "Working with data", x = ., invert = TRUE, value = TRUE) %>%   # this cleans out all the unwanted files
  set_names(basename(.)) %>%        # we only need the actual file name, not the whole pathway
  map(read_xl_csv) %>% 
  map(as_tibble) %>% 
  map(select, -`#`) %>%             # taking out the first column called #, because not needed.
  map(rename_all, .funs = gsub, pattern = "Control", replacement = "CTR") %>%  #homogenizing names
  
  #there are a lot of different date and time columns and formats. 
  
  map(mutate_at, .vars = vars(-matches("Date|Time")), .fun = as.numeric) %>%  # all vars except date and time are numeric
  map_df(~{
    date_time_col <- (.) %>% select(matches("Date Time"))
    
    if(ncol(date_time_col) == 1){
      if(date_time_col %>% pull(1) %>% class() == "character"){
        print("date time is a character")
        . <- (.) %>% mutate_at(vars(matches("Date Time")), .fun = mdy_hms)  # all Date Time columns in date format.
      }    
    }
    date_col <- (.) %>% select(matches("^Date$"))
    if(ncol(date_col) == 1){
      if(date_col %>% pull(1) %>% class() == "character"){
        print("date is a character")
        . <- (.) %>% rename_at(vars(matches("^Time")), ~"Time") %>% 
          mutate(date_time = mdy_hms(paste(Date, Time))) %>% 
          select(-Date, -Time)       
        # This was merging 'date' and 'time' columnns, setting them in date format. Get rid of separate Date and Time columns. 
      }
    }                                                                   
    
    return(.)
    #Coalesce all different columns into one locigally named column:
    
  }, .id = "file") %>% 
  mutate(watCont_ctr = coalesce(`Water Content, mÂ³/mÂ³ (LGR S/N: 10438267, SEN S/N: 10448340, LBL: OTC)` , `Water Content, mÂ³/mÂ³ (LGR S/N: 10438267, SEN S/N: 10448346, LBL: CTR)` ,`Water Content, m³/m³ (LGR S/N: 10438268, SEN S/N: 10448347, LBL: CTR-3)`, `Water Content, m³/m³ (LGR S/N: 10438267, SEN S/N: 10448340, LBL: CTR)` , `Water Content, m³/m³ (LGR S/N: 2257208, SEN S/N: 10659687, LBL: BIS_low_CTR_c4)` , `Water Content, mÂ³/mÂ³ (LGR S/N: 10438268, SEN S/N: 10448347, LBL: CTR-3)` , `Water Content, mÂ³/mÂ³ (LGR S/N: 10438267, SEN S/N: 10448340, LBL: CTR)` , `Water Content, m^3/m^3 (LGR S/N: 2257208, SEN S/N: 1057234200)` , `Water Content, mÂ³/mÂ³ (LGR S/N: 2257208, SEN S/N: 10659687)` , `Water Content, m³/m³ (LGR S/N: 2257208, SEN S/N: 10659687)`)) %>% 
  
  mutate(watCont_otc = coalesce(`Water Content, m³/m³ (LGR S/N: 10438268, SEN S/N: 10448341, LBL: OTC-2)` , `Water Content, m³/m³ (LGR S/N: 10438267, SEN S/N: 10448346, LBL: OTCl)` , `Water Content, m³/m³ (LGR S/N: 2257208, SEN S/N: 10659686, LBL: BIS_low_OTC_c1)` , `Water Content, mÂ³/mÂ³ (LGR S/N: 10438268, SEN S/N: 10448341, LBL: OTC-2)` , `Water Content, mÂ³/mÂ³ (LGR S/N: 10438267, SEN S/N: 10448346, LBL: OTCl)` , `Water Content, mÂ³/mÂ³ (LGR S/N: 2257208, SEN S/N: 10659686, LBL: BIS_low_OTC_c1)`)) %>% 
  
  mutate(temp_ctr = coalesce(`Temp, Â°C (LGR S/N: 10438267, SEN S/N: 10448514, LBL: CTR)` , `Temp, °C (LGR S/N: 10438268, SEN S/N: 10448510, LBL: CTR-3)` , `Temp, °C (LGR S/N: 10438267, SEN S/N: 10448512, LBL: CTR)` , `Temp, °C (LGR S/N: 2257208, SEN S/N: 2028202, LBL: BIS_low_CTR_c3)` , `Temp, Â°C (LGR S/N: 10438268, SEN S/N: 10448510, LBL: CTR-3)` , `Temp, Â°C (LGR S/N: 10438267, SEN S/N: 10448512, LBL: CTR)` , `Temp, Â°C (LGR S/N: 2257208, SEN S/N: 2028202, LBL: BIS_low_CTR_c3)`)) %>% 
  
  mutate(temp_otc = coalesce(`Temp, Â°C (LGR S/N: 10438267, SEN S/N: 10448512, LBL: OTC)` , `Temp, °C (LGR S/N: 10438268, SEN S/N: 10448506, LBL: OTC-2)` , `Temp, °C (LGR S/N: 10438267, SEN S/N: 10448514, LBL: OTC)` , `Temp, °C (LGR S/N: 2257208, SEN S/N: 10655576, LBL: BIS_low_OTC_c2)` , `Temp, Â°C (LGR S/N: 10438268, SEN S/N: 10448506, LBL: OTC-2)` , `Temp, Â°C (LGR S/N: 10438267, SEN S/N: 10448514, LBL: OTC)` , `Temp, Â°C (LGR S/N: 2257208, SEN S/N: 10655576, LBL: BIS_low_OTC_c2)`)) %>% 
  
  mutate(rh_ctr = coalesce(`RH, % (LGR S/N: 10438267, SEN S/N: 10448514, LBL: CTR)` , `RH, % (LGR S/N: 10438268, SEN S/N: 10448510, LBL: CTR-3)` , `RH, % (LGR S/N: 10438267, SEN S/N: 10448512, LBL: CTR)` , `RH, % (LGR S/N: 2257208, SEN S/N: 2028202, LBL: BIS_low_CTR_c3)`)) %>% 
  
  mutate(rh_otc = coalesce(`RH, % (LGR S/N: 10438267, SEN S/N: 10448512, LBL: OTC)` , `RH, % (LGR S/N: 10438268, SEN S/N: 10448506, LBL: OTC-2)` , `RH, % (LGR S/N: 10438267, SEN S/N: 10448514, LBL: OTC)` , `RH, % (LGR S/N: 2257208, SEN S/N: 10655576, LBL: BIS_low_OTC_c2)`)) %>% 
  
  select(-matches("SEN")) %>% # all now unwanted column names have 'SEN' in it, so delete anything that contains that
  
  mutate(date_time = coalesce(date_time , `Date Time, GMT+00:00` , Date)) %>% 
  select(-`Time, GMT+00:00` , -`Date Time, GMT+00:00` , -Date) %>% # combine date and time columns, lose unwanted ones
  
  mutate(habitat = case_when(
    grepl(pattern = "dry", file, ignore.case = TRUE) ~ "dry",
    grepl(pattern = "bis" , file, ignore.case = TRUE) ~"bis",
    grepl(pattern = "cas" , file, ignore.case = TRUE) ~"cas",
    TRUE ~ "unknown"
  )) %>%  
  # create a column called habitat, call it "dry" when you find 'dry' in the file name, irrespective of capital letters or not.
  
  # temp, rh and watCont have two columns each, based on treatment (OTC or CTR). they are called "temp_otc", for example. We want to have all temp data in one column, and add an extra column indicating which treatment it has. Therefore we  first stack all columns (exept for file, date time and habitat). We call that column "trait". 
  
  gather(key = "trait", value = "value", watCont_ctr:rh_otc) %>% 
  
  # we then create a new column indicating treatment using the names in 'trait', separating them into the variable (temp) and the treatment(otc), using the _ as separator
  
  separate(col = "trait", into= c("trait", "treatment"), sep = "_") %>% 
  
  # Lastly, we spread the long column (trait) by its value (which is now only temp, watCont, rh).
  
  spread(key =trait, value = value) %>% 
  
  # Filtering out the 'broken logger data':
  
  filter(!(between(date_time, ymd_hms("2018-07-26 04:00:02"), ymd_hms("2018-08-01 00:00:00")) & habitat == "dry" & file == "Dry-L-6.xlsx")) %>% 
  
  mutate(
    watCont = if_else(watCont < -100, NA_real_, watCont)
  )

# HELL YEAH

hobo_st

gp


### PLOT SOME GRAPHS ###

g <- ggplot(data = hobo_st, mapping = aes(x = date_time, y = temp, color = file)) + 
  geom_point() +
  theme(legend.position = "none")
g

g + aes(color = treatment) + theme(legend.position = "right")

g + aes(color = habitat) + theme(legend.position = "right")

g + aes(y = rh, color = habitat) + theme(legend.position = "right")

g + aes(y = watCont, color = habitat) + theme(legend.position = "right")

g + aes(y = watCont, color = treatment) + theme(legend.position = "right")+
  facet_grid(habitat ~ .)

g + aes(color = treatment) + theme(legend.position = "right")+
  facet_grid(habitat ~ .)

#### Calculating monthly means ####

names (hobo_st)


day_temp <- hobo_st %>% 
  filter(!is.na(temp)) %>%
  separate(col = "date_time", into= c("date", "time"), sep = " ") %>% 
  group_by(file, date, habitat, treatment) %>% 
  summarise(n = n(), dayTemp = mean(temp))

day_temp

month_temp <- day_temp %>% 
  filter(!is.na(dayTemp)) %>% 
  ungroup() %>% 
  mutate_at(vars("date"), .fun = ymd) %>%  
  mutate(date = dmy(paste0("15-", format(date , "%b.%Y")))) %>% # sets all the dates of the month to the 15th.
  group_by(file, date, habitat, treatment) %>% 
  summarise(n = n(), monthTemp = mean(dayTemp))

month_temp
names(month_temp)

ggplot(data = day_month, mapping = aes(x = date, y = monthTemp, color = treatment, shape = habitat)) + 
  geom_smooth(mapping = aes(linetype = habitat))+
  geom_point(size = 3) +
  facet_grid(habitat ~ .)


