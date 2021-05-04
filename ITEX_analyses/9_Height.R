### PLANT HEIGHT ###
source("ITEX_analyses/2_Import_data.R")

height <- height_raw %>% 
  filter(SUBSITE %in% c("BIS-L", "CAS-L", "DRY-L",
                        YEAR == 2015)) %>% 
  mutate(SUBSITE = recode(SUBSITE, "BIS-L" = "BIS", "CAS-L" = "CAS", "DRY-L" = "DRY")) %>% 
  rename("Year" = "YEAR", "Site" = "SUBSITE", "Treatment" = "TREATMENT") %>% 
  mutate(Site = case_when(Site == "BIS" ~ "SB",
                          Site == "CAS" ~ "CH",
                          Site == "DRY" ~ "DH"))

CanopyHeight <- ggplot(height, aes(x = Site, y = HEIGHT, fill = Treatment)) +
  geom_boxplot() +
  scale_fill_manual(values = c("grey", "red")) +
  labs(y = "Canopy height cm", x = "Habitat Type") +
  annotate("text", x = 0.7, y = 30, label = "T *") +
  theme_classic() +
  theme(text = element_text(size = 20))

ggsave(CanopyHeight, filename = "ITEX_analyses/output/Fig_S6_CanopyHeight.jpeg", width = 6, height = 4, dpi = 300)


height %>%
  mutate(log.h = log(HEIGHT)) %>% 
  nest(data = -c(Site, Year)) %>% 
  mutate(
    fit = map(data, ~ lm(log.h ~ Treatment, data = .x)),
    tidied = map(fit, tidy)
  ) %>% 
  unnest(tidied) %>% 
  filter(p.value < 0.05,
         term != "(Intercept)")


height %>%
  filter(SUBSITE %in% c("BIS-L", "CAS-L", "DRY-L"),
         YEAR == 2015) %>% 
  mutate(SUBSITE = recode(SUBSITE, "BIS-L" = "BIS", "CAS-L" = "CAS", "DRY-L" = "DRY")) %>% 
  rename("Year" = "YEAR", "Site" = "SUBSITE", "Treatment" = "TREATMENT") %>%
  group_by(Site, Treatment) %>% 
  summarise(mean = mean(HEIGHT, na.rm = TRUE),
            se = mean / sqrt(n()))

fit <- lm(log.h ~ Treatment, data = dd)
plot(fit)