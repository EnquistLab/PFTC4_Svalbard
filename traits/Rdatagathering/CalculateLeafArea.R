
#### CALCULATE LEAF AREA ####

# load libraries
#devtools::install_github("richardjtelford/LeafArea")
library("LeafArea")
library("tidyverse")


#### Check LeafIDs
# Load trait IDs
load("traits/Rdatagathering/envelope_codes.Rdata", verbose = TRUE)

# List of scan names
list.of.files <- dir(path = paste0("/Volumes/Ohne Titel/Leaf Scans"), pattern = "jpeg|jpg", recursive = TRUE, full.names = TRUE)

dd <- basename(list.of.files) %>% 
  as.tibble() %>% 
  mutate(value = gsub(".jpeg", "", value))

setdiff(dd$value, all_codes$hashcode)
# Only "Unknown_2018-07-20_01_05"


# Function to calculate leaf area
loop.files <-  function(files){
  
  file.copy(files, new.folder)
  #if(grepl("-NA$", files)){
  #newfile <- basename(files)
  #file.rename(paste0(new.folder, "/", newfile), paste0(new.folder,
  #"/", gsub("-NA$", "", newfile)))
  #}
  print(files)
  area <- try(run.ij(set.directory = new.folder, distance.pixel = 237, known.distance = 2, log = TRUE, low.size = 0.005, trim.pixel = 55, trim.pixel2 = 150, save.image = TRUE))
  if(inherits(area, "try-error")){
    return(data.frame(LeafArea = NA))
  }
  file.copy(dir(new.folder, full.names = TRUE, pattern = "\\.tif"), output.folder)
  Sys.sleep(0.1)
  if(any(!file.remove(dir(new.folder, full.names = TRUE) ))) stop()
  res <- data.frame(ID = names(unlist(area[[2]])), LeafArea = (unlist(area[[2]])))
  return(res)
}



#### Calculate the leaf area using run.ij and check if there are problem.

# test run.ij
run.ij(set.directory = "~/Desktop/TestLeaf", distance.pixel = 237, known.distance = 2, log = TRUE, low.size = 0.005, trim.pixel = 55, trim.pixel2 = 150, save.image = TRUE)


#### Run run.ij to get areas
list.of.files <- dir(path = paste0("/Volumes/Ohne Titel/Leaf Scans"), pattern = "jpeg|jpg", recursive = TRUE, full.names = TRUE)

new.folder <- "/Volumes/Ohne Titel/Temp/"
output.folder <- "/Volumes/Ohne Titel/Leaf_Output/"

LeafArea.raw <- plyr::ldply(list.of.files, loop.files)

dim(LeafArea.raw)
save(LeafArea.raw, file = "traits/data/LeafArea.raw.Rdata")

# remove duplicate leaves
LeafArea %>% 
  group_by(ID) %>% 
  filter()

#### Calculate leaf area
load("traits/data/LeafArea.raw.Rdata", verbose = TRUE)
LeafArea2018 <- LeafArea.raw %>% 
  mutate(ID = substr(ID, 1, 7)) %>% 
  # Sum areas for each ID
  group_by(ID) %>% 
  summarise(Area_cm2 = sum(LeafArea), NumberLeavesScan = n())

save(LeafArea2018, file = "traits/data/LeafArea2018.Rdata")



#### Sean leaf areas without loop

file.list.sean <- list.files(path = "C:/Users/cpo082/Desktop/leaf
                             data/SEAN_cropped")

sean_area <- run.ij (set.directory = "C:/Users/cpo082/Desktop/leaf
                     data/SEAN_cropped", distance.pixel = 237, known.distance = 2, log =
                       TRUE, save.image = TRUE, low.size = 0.05)

sean_cropped_LA_new <- data.frame(ID = names(unlist(sean_area
                                                           [[2]])), LeafArea = (unlist(sean_area[[2]])))

save(sean_cropped_LA_new, file = "C:/Users/cpo082/Desktop/leaf
     data/sean_cropped_LA_new.Rdata")
