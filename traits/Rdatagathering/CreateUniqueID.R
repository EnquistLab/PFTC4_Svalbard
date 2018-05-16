library("tidyverse")
library("writexl")

source("traits/Rdatagathering/envelope_codes.R")

# Make List with ID
dat <- all_codes %>% 
  select(hashcode) %>% 
  rename(ID = hashcode)
write_xlsx(dat, path = "UniqueID_PFTC4_Svalbard.xlsx", col_names = TRUE)

