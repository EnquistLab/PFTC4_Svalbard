files.xls <- dir(path = "climate/data/DATA/Hobo_weather st/", pattern = "\\.xls$", full.names = TRUE, recursive = TRUE)
files.csv <- dir(path = "climate/data/DATA/Hobo_weather st/", pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)

climate.weather.raw1 <- files.xls[grepl("^(?!~)", basename(files.xls), perl = TRUE)] %>% 
  set_names(basename(.)) %>% 
  map_df(read_excel, col_names = TRUE, skip = 1, .id = "file") 

climate.weather.raw2 <- files.csv[grepl("^(?!~)", basename(files.csv), perl = TRUE)] %>% 
  set_names(basename(.)) %>% 
  map_df(read_csv, col_names = TRUE, skip = 1, .id = "file")


colnames(climate.weather.raw1)
