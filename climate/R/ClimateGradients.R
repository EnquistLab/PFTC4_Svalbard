library("tidyverse")


ClimateGradient <- read.csv(file = "climate/data/SMdata.csv", header = TRUE, sep = ";")

ClimateGradient <- ClimateGradient %>% 
  select(-moistAVG, -tempAVG) %>% 
  mutate(Site = substr(plot, 1, 1),
         Elevation = substr(plot, 2, 2),
         Plot = substr(plot, 3, 3)) %>% 
  mutate(SoilMoisture = rowMeans(select(., matches("moist")), na.rm = TRUE),
         SoilTemperature = rowMeans(select(., matches("temp")), na.rm = TRUE)) %>% 
  select(-plot) %>% 
  arrange(Site, Elevation, Plot) %>% 
  gather(key = Variable, value = Value, SoilMoisture, SoilTemperature)

ggplot(ClimateGradient, aes(x = Elevation, y = Value)) +
  geom_point() +
  facet_grid(Variable ~ Site, scales = "free_y")
