library("rgbif")
library("tidyverse")
library("mapdata")

if(!exists("gbif/sj.rdata")){
  load("gbif/sj.rdata")
}else{#slow
  sj <- occ_search(
      taxonKey = 7707728,
      hasCoordinate = TRUE,
      country = "SJ",
      limit = 100000,
      decimalLatitude = "74,82",
      decimalLongitude = "8,36"
    )
  
  save(sj, file = "gbif/sj.rdata")
}

sj$data <- sj$data %>% filter(basisOfRecord != "FOSSIL_SPECIMEN")

sj$data %>% count(name) %>% print(n = Inf)

mp <- map_data("worldHires", region = "Norway:Svalbard")

sj$data %>% filter(grepl("Salix", name)) %>% count(name) %>% arrange(desc(n))


sj$data %>% 
  filter(genus == "Silene") %>% 
  mutate(name = gsub("^(\\w)\\w+ (.*)$", "\\1.\\2", name)) %>% 
  ggplot(aes(x =  decimalLongitude, decimalLatitude)) +
  geom_map(data = mp, map = mp, aes(map_id = region), inherit.aes = FALSE, fill = "grey80") +
  geom_point() +
  coord_quickmap() +
  lims(x = c(10, 34), y = c(74.6, 80.2)) +
  labs(x = "", y = "") +
  theme_minimal() +
  facet_wrap(~ name, ncol = 7)
