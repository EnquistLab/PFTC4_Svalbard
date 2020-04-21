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


### SIRI
list.of.files <- dir(path = paste0("/Volumes/PFTC3_Peru/Siri_leaves"), pattern = "jpeg|jpg", recursive = TRUE, full.names = TRUE)
new.folder <- "/Volumes/PFTC3_Peru/Temp"
output.folder <- "/Volumes/PFTC3_Peru/Output_Siri_Leaves"

LeafArea.Siri.raw <- plyr::ldply(list.of.files, loop.files)
dim(LeafArea.Siri.raw)

LeafArea.Siri.raw <- LeafArea.Siri
save(LeafArea.Siri.raw, file = "LeafArea.Siri.raw.Rdata")
LeafAreaSiri_2018 <- LeafArea.Siri.raw %>% 
  # remove leaves that were calculated double by mistake
  group_by(ID, LeafArea) %>% 
  mutate(n = n()) %>% 
  filter(n == 1) %>% 
  ungroup() %>%
  # remove jpeg etc from ID name
  mutate(ID = substr(ID, 1, 7)) %>% 
  
  # Sum areas for each ID
  group_by(ID) %>% 
  summarise(Area_cm2 = sum(LeafArea), NumberLeavesScan = n())

save(LeafAreaSiri_2018, file = "LeafAreaSiri_2018.Rdata")

ddd <- basename(list.of.files) %>% 
  as.tibble() %>% 
  mutate(value = substr(value, 1, 7))

setdiff(ddd$value, LeafAreaSiri_2018$ID)
