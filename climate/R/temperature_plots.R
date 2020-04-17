library("tidyverse")
library("lubridate")
library("directlabels")

meanT <- read.table("climate/data/vgdcnSV000001008.dat") %>% 
  set_names(c("year", "month", "day", "meanT")) %>% 
  mutate(date = ymd(paste(year, month, day, sep = "-")), 
         date0 = ymd(paste(2016, month, day, sep = "-")))

meanT %>% 
  ggplot(aes(x = date, y = meanT)) + 
  geom_line()

meanT %>% 
  ggplot(aes(x = date0, y = meanT, group = year, colour = factor(floor(year/10) * 10))) + 
  geom_line() +
  geom_smooth(aes(group = 1), colour = "red") +
  scale_x_date(date_labels = "%b") +
  labs(x = "", y = "Mean daily temperature", colour = "Decade")

monthly <- meanT %>% 
  mutate(month = factor(month, label = month.abb)) %>% 
  group_by(year, month) %>% 
  summarise(meanT = mean(meanT)) 

monthly %>% 
  ggplot(aes(x = year, y = meanT, colour = month, label = month)) +
  geom_point(show.legend = FALSE) +
  geom_smooth(show.legend = FALSE) +
  geom_dl(data = (monthly %>% group_by(month) %>% filter(year < 1980) %>% summarise(meanT = mean(meanT), year = 1975)), method = "first.bumpup") +
  scale_colour_brewer(palette = "Paired") +
  xlim(1973, NA)
  

