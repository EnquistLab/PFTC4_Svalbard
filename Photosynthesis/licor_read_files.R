
read_licor <- function(file){
  ## reading in raw licor data file
  f <- read_lines(file = file) #min_N = 15
  
  #removing junk from top
  remove <- grep("\\$", f)
  f <- f[-c(remove, remove - 1)]
  remove <- grep("OPEN", f)
  f <- f[-c(remove, remove + 1)]
  remove <- grep("<", f)
  f <- f[-remove]
  
  header_row <- grep("Obs", f)
  if(length(header_row) > 1){ 
    f <- f[-header_row[-1]]
  }
  
    
  ##finding rows of metadata
  meta_rows <- grep("^\"", f)
  meta_rows <- meta_rows[-1] #keep header row
  
  if(length(meta_rows) == 0){
    return(data_frame())
  }
  
  meta <- f[meta_rows]
  dat <- f[-meta_rows]
  
  if(length(dat) <= 1){
    return(data_frame())
  }
  
  meta <- gsub("\"", "", meta)
  ## replace multiple spaces with a single space
  meta <- gsub(" +", " ", meta)
  ## grouping multiple worded parameters 
  meta <- gsub("CO2 Mixer", "CO2_Mixer", meta)
  
  
  dat <- read_delim(paste(dat, collapse = "\n"), delim = "\t")
  
  meta <- read_delim(paste(meta, collapse = "\n"), delim = " ", col_names = c("HHMMSS", "variable", "parameter", "arrow", "level", "unit")) %>% 
    select(-arrow)
  
  
  dat <- bind_rows(meta %>% # bind logged data and metadata
              filter(parameter == "Tblock"), dat) %>% # discard parameter changes except block temp
    arrange(HHMMSS) %>% # sort data by time logged
    fill(variable, parameter, level, unit) %>%  # fill block temp parameter in for logged data
    filter(!is.na(Obs)) %>%
    group_by(level) %>% # group by block temperature
    mutate(n = n()) %>% # count observations by set block temperature
    filter(n >= 15) %>% # only keep block temps for which there are more than minimum number of records
    slice((n[1] - 15):n[1])

  return(dat)
}
###
if(FALSE){
file <- "0000-AAA8891-210718"

read_licor(file = file)
}


