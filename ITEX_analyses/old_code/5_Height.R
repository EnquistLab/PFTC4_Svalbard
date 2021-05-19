### PLANT HEIGHT ###
source("ITEX_analyses/2_Import_data.R")

height <- veg_structure_raw %>% 
  filter(Variable == "MedianHeight_cm",
         Year == 2015) %>% 
  mutate(Site = factor(Site, levels = c("SB", "CH", "DH")))

CanopyHeight <- ggplot(height, aes(x = Site, y = Value, fill = Treatment)) +
  geom_boxplot() +
  scale_fill_manual(values = c("grey", "red")) +
  labs(y = "Canopy height cm", x = "Habitat Type") +
  annotate("text", x = 2, y = 30, label = "T *") +
  annotate("text", x = 3, y = 30, label = "T *") +
  theme_classic() +
  theme(text = element_text(size = 20))

ggsave(CanopyHeight, filename = "ITEX_analyses/output/Fig_S6_CanopyHeight.jpeg", width = 6, height = 4, dpi = 300)


height %>%
  group_by(Site, Treatment) %>% 
  summarise(mean = mean(Value, na.rm = TRUE),
            se = mean / sqrt(n()))

dd <- height %>% filter(Site == "DH")
fit1 <- lmer(Value ~ Treatment + (1|PlotID), data = dd)
fit2 <- lmer(Value ~ 1 + (1|PlotID), data = dd)
AIC(fit1)
AIC(fit2)
plot(fit)
