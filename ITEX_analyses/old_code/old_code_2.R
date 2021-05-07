#### Old code ####
# trait distances, no longer used.
# trait_distances <- traitMean %>% 
#   filter(Year != 2009) %>% 
#   select(-mean_noitv) %>% 
#   filter(Trait != "Dry_Mass_Total_g", Trait != "Wet_Mass_Total_g", Trait != "wetSLA_cm2_per_g", Trait != "Wet_mass_g") %>% 
#   spread(key = Trait, value = mean) %>% 
#   group_by(PlotID) %>% 
#   do( tibble(out = as.vector(dist(select(., -(Site:Year)), method = "euclidian")))) %>% 
#   left_join(metaItex)
# 
# trait_distances_noitv <- traitMean %>% 
#   filter(Year != 2009) %>% 
#   select(-mean) %>% 
#   filter(Trait != "Dry_Mass_Total_g", Trait != "Wet_Mass_Total_g", Trait != "wetSLA_cm2_per_g", Trait != "Wet_mass_g") %>% 
#   spread(key = Trait, value = mean_noitv) %>% 
#   group_by(PlotID) %>% 
#   do( tibble(out = as.vector(dist(select(., -(Site:Year)), method = "euclidian")))) %>% 
#   left_join(metaItex)
# 
# 
# trait_dist <- trait_distances %>% ungroup() %>% mutate(y_max = max(out), y_min = min(out)) %>% 
#   ggplot(aes(x = Site, y = out, fill = Treatment)) +
#   geom_boxplot() +
#   scale_fill_manual(values = c("gray", "red")) +
#   ylab("Euclidian Distance") +
#   xlab("Habitat Type") +
#   ggtitle("Trait Change") +
#   theme_classic() +
#   theme(text = element_text(size = 20)) +
#   stat_compare_means(aes(group = Site), label = "p.signif", method = "anova", hide.ns = F, label.x = 0.5, label.y.npc = 0.05)+
#   stat_compare_means(aes(group = Treatment), label = "p.signif", method = "anova", hide.ns = F, label.y.npc = 0.95) +
#   geom_blank(aes(y = y_min)) +
#   geom_blank(aes(y = y_max + 0.35*y_max))
# 
# ggarrange(comm_dist, trait_dist, ncol = 2, common.legend = T)



#try doing var_split by site to isolate the treatment effect and look at differences between sites
var_split_site <- traitMean %>%
  group_by(Trait, Site) %>% 
  do(test = trait.flex.anova(~Treatment, mean, mean_noitv, data = .)) 

var_split_site_exp <- data.frame(RelSumSq.Turnover = 1000, RelSumSq.Intraspec. = 1000, RelSumSq.Covariation = 1000, RelSumSq.Total = 1000, trait = "E", site = "G", level = "F")

for(i in 1:nrow(var_split_site)){
  out <- as.data.frame(var_split_site$test[[i]][2])
  out$trait <- as.factor(rep(var_split_site[i,1], 3))
  out$site <- as.factor(rep(var_split_site[i,2], 3))
  out$level <- rownames(out)
  var_split_site_exp <- rbind(var_split_site_exp, out)
}

var_split_site_exp %>% 
  filter(RelSumSq.Turnover < 999) %>% 
  rename(Turnover = RelSumSq.Turnover, Intraspecific = RelSumSq.Intraspec., Covariation = RelSumSq.Covariation, Total = RelSumSq.Total) %>% 
  gather(key = variable, value = value, -trait, -level, -site) %>% 
  filter(variable != "Total", level != "Total") %>% 
  mutate(trait = plyr::mapvalues(trait, from = c("SLA_cm2_g", "LDMC", "Leaf_Area_cm2", "Leaf_Thickness_Ave_mm", "N_percent", "C_percent", "P_Ave", "CN_ratio", "dC13_percent", "dN15_percent", "Dry_Mass_g", "Plant_Height_cm"), to = c("SLA", "LDMC", "'Leaf'*' '*'Area'", "'Leaf'*' '*'Thickness'", "'%'*'N'", "'%'*'C'", "'%'*'P'", "'C'*':'*'N'", "paste(delta^13, 'C')", "paste(delta^15, 'N')", "'Dry'*' '*'Mass'*' '*'(g)'", "'Plant'*' '*'Height'*' '*'(cm)'"))) %>% 
  mutate(trait = factor(trait, levels = c("SLA", "LDMC", "'Leaf'*' '*'Area'", "'Leaf'*' '*'Thickness'", "'%'*'N'", "'%'*'C'", "'%'*'P'", "'C'*':'*'N'", "paste(delta^13, 'C')", "paste(delta^15, 'N')", "'Dry'*' '*'Mass'*' '*'(g)'", "'Plant'*' '*'Height'*' '*'(cm)'"))) %>% 
  ggplot(aes(x = level, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  facet_grid(trait~site, labeller = label_parsed) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90), text = element_text(size = 15), legend.position = "right") +
  xlab(NULL) +
  ylab("Proportion Variation Explained") +
  scale_fill_manual(values = c("blue", "darkorange","forestgreen"), name = "Source of Variation")