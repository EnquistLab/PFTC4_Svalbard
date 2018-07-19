
### CALCULATE LEAF AREA

#load packages etc
If(!require("devtools")){
  install.packages("devtools")
}
if(!require("LeafArea")){
  devtools::install_github("richardjtelford/LeafArea")
}

source("traits/Rdatagathering/varEntryDialog.r")

##get filename to check
path <- "~/Desktop/Svalbard_leaves"
print(ls(path))
filepath <- file.path(
    path, 
    basename(file.choose())
  )




#Function to calculate leaf area
process.file <-  function(file){
  
  file.copy(file, newfolder)
  area <- run.ij(path.imagej = "/usr/bin/imagej", set.directory = new.folder, distance.pixel = 237, known.distance = 2, log = TRUE, low.size = 0.005, trim.pixel = 50, trim.pixel2 = 150, save.image = TRUE)

  file.copy(dir(new.folder, full.names = TRUE, pattern = "\\.tif"), output.folder)
  Sys.sleep(0.1)
  if(any(!file.remove(dir(new.folder, full.names = TRUE) ))) stop()
  res <- data.frame(ID = names(unlist(area[[2]])), LeafArea = (unlist(area[[2]])))
  return(res)
}



# Calculate leaf area
newfolder = file.path(path, "temp")
if(dir.exists(newfolder)){
  dir.create(path, newfolder)
}  

output.folder <- file.path(path, "ij_out")
if(dir.exists(output.folder)){
  dir.create(path, output.folder)
}  


process.file(filepath)
