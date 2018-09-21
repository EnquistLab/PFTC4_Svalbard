library("readxl")
library("tidyverse")


dat <- read_excel(path = "data/Katrin Bochmuhl altitudinal gradients/total_species_account.xls", sheet = "Platåfjellet")

dat %>%
  select(-species, -`280`, `290`, `300`, `310`, `320`, `330`, `340`, `350`, `360`) %>% 
  rename(species = species__1) %>% 
  gather(key = plot, value = abundance, - species, -abbreviation) %>% 
  filter(plot == 450, abundance > 0)
  
bren <- read_excel(path = "data/Katrin Bochmuhl altitudinal gradients/total_species_account.xls", sheet = "Brentskarhaugen")
bren %>% 
  select(-species) %>% 
  rename(species = species__1) %>% 
  gather(key = plot, value = abundance, - species, -abbreviation) %>% 
  filter(plot== 528, abundance > 0)

platafjell <- read_excel(path = "data/Katrin Bochmuhl altitudinal gradients/Gradienten nach plots sortiert.xls", sheet = "Platåfjellet")

platafjell <- platafjell %>% 
  rename(elevation = `altitude / m`, UTM = `position / UTM`, coordinate = position, veg.cover = `vegetation cover / %`, temperature = `temperature / °C`) %>% 
  fill(plot:coordinate, veg.cover:comment) %>% 
  separate(UTM, into = c("zone", "x", "junk", "y"), sep = " ") %>% 
  select(-junk) %>% 
  mutate(x = as.numeric(x), y = as.numeric(y))

platafjell %>% 
  filter(abundance != "missing (snow)") %>% 
  group_by(species) %>% 
  mutate(n = n()) %>% 
  filter(n > 2) %>% 
  filter(species == "Pedicularis hirsuta L.") %>% 
  ggplot(aes(x = elevation, y = abundance)) +
  geom_point() +
  theme(legend.position = 'none')


plotCurves <- function(dat){
  dat %>% 
    ggplot(aes(x = elevation, y = abundance)) +
    geom_point() +
    ggtitle(unique(dat$species)) +
    ylim(0, 4) +
    theme(legend.position = 'none')
}

### Make plots and print PDF
AbundanceCurves <- platafjell %>% 
  filter(abundance != "missing (snow)") %>% 
  mutate(abundance = as.numeric(abundance)) %>% 
  group_by(species) %>% 
  mutate(n = n()) %>% 
  filter(n > 2) %>%  
  group_by(species) %>% 
  do(ab.curves = plotCurves(.))

pdf(file = "Curves.pdf")
AbundanceCurves$ab.curves
dev.off()


# make map
library("sp")
library("rgdal")
library("ggmap")
library("raster")
coords <- SpatialPoints(cbind(dat2$x, dat2$y), proj4string = CRS("+proj=utm +zone=33")) %>% 
  spTransform(CRS("+proj=longlat")) %>% 
  as.data.frame()

dat2 <- dat2 %>% 
  #select(-x, -y) %>% 
  bind_cols(coords)

longyearbyen <- get_map(location = "Longyearbyen", zoom = 8)
ggmap(longyearbyen) +
  geom_point(data = dat2, mapping = aes(x = coords.x1, y = coords.x2, color = elevation)) +
  geom_path(data = dat2, mapping = aes(x = coords.x1, y = coords.x2))


longyearbyen2 <- raster("data/Katrin Bochmuhl altitudinal gradients/Export.tif") 
longyearbyen2 <- as.data.frame(longyearbyen2, xy = TRUE)

ggplot() +
  geom_raster(data = longyearbyen2 %>% filter(Export == 174), aes(x = x, y = y, fill = as.factor(Export)), show.legend = FALSE) +
  geom_point(data = dat2, mapping = aes(x = x, y = y, color = elevation)) +
  geom_path(data = dat2, mapping = aes(x = x, y = y))


  #mutate(lat = as.numeric(stringi::stri_extract(regex = "^(\\d{2})", lat))
         #as.numeric(stringi::stri_extract(regex = "^(\\d{2})", lat))
         #as.numeric(stringi::stri_extract(regex = "^(\\d{2})", lat))
