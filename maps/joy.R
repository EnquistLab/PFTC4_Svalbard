library("rgdal")
library("tidyverse")
library("ggridges")

#50m data from https://data.npolar.no/dataset/dce53a47-c726-4845-85c3-a65b46fe2fea

sv <- raster("maps/data/NP_S0_DTM50/S0_DTM50.tif")

sv2 <- sv %>% 
  as.data.frame(xy = TRUE) %>% 
  as_tibble()
dim(sv2)




sv2 %>% 
  filter(
    y %% 10000 == 15, 
    y > 8410000# no Bjørnøya
  ) %>% 
  filter(
    y >= min(y[S0_DTM50 > 0])
    ) %>% 
  mutate(S0_DTM50 = if_else(S0_DTM50 == 0, NA_real_, S0_DTM50)) %>% 
  ggplot(aes(x = x, y = y, group = y, height = S0_DTM50 * 15)) +
  geom_ridgeline(fill = NA) +
  coord_equal() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  ggthemes::theme_map() 
