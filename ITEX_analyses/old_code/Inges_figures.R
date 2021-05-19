p1 <- CWM_ITV %>%
  mutate(ITV = ITV - noITV) %>%
  gather(key = ITV, value = Var.exp, noITV:ITV) %>%
  ggplot(aes(x = Cflux, y = Var.exp * 100, fill = ITV)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("grey20", "grey70"), 
                    #limits = c("ITV", "noITV"),
                    labels = c("intra", "inter"), 
                    name = "Trait variation") +
  geom_hline(yintercept = 0, colour = "grey40") +
  labs(y = '', tag = "C") +
  scale_y_continuous(limits = c(-10, 70), 
                     breaks = seq(-10, 70, by = 10)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.2, 0.8))

p2 <- VarianceDecomp %>% 
  mutate(Variables = factor(Variables, levels = c("unique.Env", "combined.Env.Com", "unique.Com", "combined.Env.Com.Trait", "combined.Com.Trait", "combined.Env.Trait", "unique.Trait"))) %>% 
  ggplot(aes(x = Cflux, y = value*100, fill = Variables)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("grey90", "#0072B2", "#0072B2", "#009E73" , "#009E73", "#F0E442", "#F0E442"), 
                    labels = c("Environment", "Environment & Taxonomy", "Taxonomy", "All", "Taxonomy & Trait", "Environment & Trait", "Trait")) +
  geom_col_pattern(aes(#specify angle
    pattern_angle = Variables,
    # specify patter
    pattern = Variables),
    pattern_fill = "black",
    pattern_spacing = 0.01) +
  # distinguish pattern type
  scale_pattern_manual(values = c("stripe", "stripe", "none", "stripe" , "none", "stripe", "none"),
                       labels = c("Environment", "Environment & Taxonomy", "Taxonomy", "All", "Taxonomy & Trait", "Environment & Trait", "Trait")) +
  #distinguish pattern angle
  scale_pattern_angle_manual(values = c(45, 45, 0, 45 , 0, 45, 0),
                             labels = c("Environment", "Environment & Taxonomy", "Taxonomy", "All", "Taxonomy & Trait", "Environment & Trait", "Trait"),
                             guide = guide_legend(override.aes = list(pattern_spacing = 0.01))) +
  geom_hline(yintercept = 0, colour = "grey40") +
  scale_y_continuous(limits = c(-10, 70), breaks = seq(-10, 70, by = 10)) +
  labs(x = "", y = "Explained variance %", tag = "A") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.65,0.85))

p3 <- VarianceDecomp2 %>% 
  mutate(Variables = factor(Variables, levels = c("unique.Env", "combined.Env.Com", "unique.Com", "combined.Env.Com.Trait", "combined.Com.Trait", "combined.Env.Trait", "unique.Trait"))) %>% 
  ggplot(aes(x = Cflux, y = value*100, fill = Variables)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("grey90", "#0072B2", "#0072B2", "#009E73" , "#009E73", "#F0E442", "#F0E442"), 
                    labels = c("Environment", "Environment & Taxonomy", "Taxonomy", "All", "Taxonomy & Trait", "Environment & Trait", "Trait")) +
  geom_col_pattern(aes(#specify angle
    pattern_angle = Variables,
    # specify patter
    pattern = Variables),
    pattern_fill = "black",
    pattern_spacing = 0.01) +
  # distinguish pattern type
  scale_pattern_manual(values = c("stripe", "stripe", "none", "stripe" , "none", "stripe", "none"),
                       labels = c("Environment", "Environment & Taxonomy", "Taxonomy", "All", "Taxonomy & Trait", "Environment & Trait", "Trait")) +
  #distinguish pattern angle
  scale_pattern_angle_manual(values = c(45, 45, 0, 45 , 0, 45, 0),
                             labels = c("Environment", "Environment & Taxonomy", "Taxonomy", "All", "Taxonomy & Trait", "Environment & Trait", "Trait")) +
  geom_hline(yintercept = 0, colour = "grey40") +
  scale_y_continuous(limits = c(-10, 70), breaks = seq(-10, 70, by = 10)) +
  labs(x = "", y = "", tag = "B") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

carbon_flux_figure <- p2 + p3 + p1

ggsave("carbon_flux_figure.jpeg", carbon_flux_figure, height = 8, width = 20)
