library("tidyverse")
library("readr")

#precipitation
ppt <- read_csv2("climate/data/longyearbyen_ppt.txt", skip = 21, col_types = "ccnnnnnnn") %>% 
  filter(Stnr == "99840") %>% #removes summary at end of file
  separate(Måned, into = c("month", "year"), sep = "\\.") %>% 
  mutate(month = as.numeric(month), year = as.numeric(year)) %>% 
  mutate(date = lubridate::ymd(paste(year, month, 15, sep = "-")),
         month = factor(month, labels = month.abb)) 

ppt %>% group_by(year) %>% 
  filter(!is.na(RR)) %>% 
  summarise(n = n(), annual = sum(RR)) %>% 
  filter(n == 12) %>% 
  ggplot(aes(x = year, y = annual)) + 
  geom_col() +
  geom_smooth() +
  labs(y = "Precipitation mm")


ppt %>% 
  filter(!is.na(RR)) %>% 
  ggplot(aes(x = year, y = RR)) + 
  geom_col() +
  geom_smooth(method = "gam", method.args = list(family = Gamma)) +
  facet_wrap(~ month) +
  labs(y = "Precipitation mm")

ppt %>% group_by(month) %>% 
  summarise(mean = mean(RR, na.rm = TRUE)) %>% 
  ggplot(aes(x = month, y = mean)) +
  geom_col() +
  labs(y = "Mean precipitation mm") +
  theme(axis.title.x = element_blank())
