#load data
source("ITEX_analyses/2_Import_data.R")

#### TRAIT BOOTSTRAPPING ####
#prepare community data
comm <- CommunitySV_ITEX_2003_2015 %>%
  filter(FunctionalGroup != "lichen", FunctionalGroup != "moss", FunctionalGroup != "liverwort", FunctionalGroup != "fungi") %>% 
  select(-FunctionalGroup) %>% 
  filter(Taxon != "equisetum arvense", Taxon != "equisetum scirpoides") %>% 
  filter(Year == 2015) %>% 
  mutate(Site_trt = paste0(Site, Treatment))

#prepare trait data
trait <- Svalbard_2018_ITEX_Traits %>% 
  select(Treatment, Site, PlotID, Taxon, Trait, Value) %>% 
  filter(!is.na(Value)) %>% 
  mutate(Site_trt = paste0(Site, Treatment)) %>%  
  mutate(Site = factor(Site, levels = c("SB", "CH", "DH"))) %>% 
  filter(Trait != "Wet_Mass_g")

#prepare trait data without intraspecific variation
trait.null <- Svalbard_2018_ITEX_Traits %>% 
  select(Treatment, Site, PlotID, Taxon, Trait, Value) %>%   
  filter(!is.na(Value)) %>% 
  filter(Treatment == "CTL") %>% 
  filter(Trait != "Wet_Mass_g") %>% 
  group_by(Taxon, Trait) %>% 
  summarize(Value = mean(as.numeric(Value), na.rm = T)) %>% 
  right_join(trait, by = c("Taxon", "Trait")) %>% 
  select(-Value.y, "Value" = Value.x) %>%  
  mutate(Site = factor(Site, levels = c("SB", "CH", "DH"))) 

#set seed for bootstrapping repeatability
set.seed(2525)
trait_imp <- trait_impute(comm = comm, 
                          traits = trait, 
                          scale_hierarchy = c("Site", "Site_trt", "PlotID"),
                          global = F,
                          taxon_col = "Taxon",
                          trait_col = "Trait",
                          value_col = "Value",
                          abundance_col = "Abundance",
                          min_n_in_sample = 2
)

trait_imp_null <- trait_impute(comm = comm, 
                               traits = trait.null, 
                               scale_hierarchy = c("Site", "Site_trt", "PlotID"),
                               global = F,
                               taxon_col = "Taxon",
                               trait_col = "Trait",
                               value_col = "Value",
                               abundance_col = "Abundance",
                               min_n_in_sample = 2
)

#check trait coverage
trait_imp %>% 
  #filter(Trait == "C_percent") %>% 
  autoplot(.) +
  theme(axis.text.x = element_text(angle = 90))

trait_imp_null %>% 
  autoplot(.) +
  theme(axis.text.x = element_text(angle = 90))

#do the bootstrapping
CWM <- trait_np_bootstrap(trait_imp, nrep = 100, sample_size = 200)
CWM_notiv <- trait_np_bootstrap(trait_imp_null, nrep = 100, sample_size = 200)

CWM_mean <- trait_summarise_boot_moments(CWM) %>% 
  select(Site:mean) 

CWM_notiv_mean <- trait_summarise_boot_moments(CWM_notiv) %>% 
  select(Site:mean) %>% 
  rename("mean_noitv" = "mean")

traitMean <- CWM_mean %>% 
  left_join(CWM_notiv_mean) %>% 
  select(-n) %>% 
  mutate(Year = 2015)

#### Trait analyses ####
#prepare bootstrapped trait data for analyses
traitMean <- traitMean %>% ungroup() %>% 
  mutate(Trait = plyr::mapvalues(Trait, from = c("P_percent", "dC13_permil", "dN15_permil"), to = c("P_Ave", "dC13_percent", "dN15_percent"))) %>% 
  mutate(Treatment = substr(Site_trt, 3, 6)) %>% 
  mutate(Site = factor(Site, levels = c("SB", "CH", "DH")))

metaItex <- CommunitySV_ITEX_2003_2015 %>% 
  distinct(Site, Treatment, PlotID)

#### pca plot ####

trait_pca <- traitMean %>% 
  select(-mean_noitv) %>% 
  mutate(Trait = plyr::mapvalues(Trait, from = c("SLA_cm2_g", "LDMC", "Leaf_Area_cm2", "Leaf_Thickness_mm", "N_percent", "C_percent", "P_Ave", "CN_ratio", "dC13_percent", "dN15_percent", "Dry_Mass_g", "Plant_Height_cm"), to = c("SLA", "LDMC", "Leaf Area", "Leaf Thickness", "%N", "%C", "%P", "C:N", "delta13C", "delta15N", "Dry Mass", "Plant Height"))) %>% 
  pivot_wider(names_from = Trait, values_from = mean)

trait_pca_data <- trait_pca %>% 
  select("%C":"SLA")

trait_pca_info <- trait_pca %>% 
  select(Site:Treatment)

pca_res <- prcomp(trait_pca_data, center = T, scale. = T)

pca_points <- cbind(trait_pca_info, pca_res$x) %>% rename("Habitat" = "Site")
pca_arrows <- pca_res$rotation

pca_plot <- autoplot(pca_res, data = pca_points, 
         shape = 'Treatment', 
         colour = "Habitat", 
         size = 3,
         loadings = TRUE, 
         loadings.colour = 'black',
         loadings.label = TRUE, 
         loadings.label.size = 4, 
         loadings.label.colour = "black",
         loadings.label.repel = T,
         parse = T) +
  theme_classic() +
  theme(legend.position = "top",
        text = element_text(size = 12)) +
  scale_color_manual(values = c("blue", "forestgreen", "orange"))

jpeg(filename = "ITEX_analyses/output/Fig_2_pca_plot.jpg", width = 7, height = 5.75, units = "in", res = 400)
pca_plot
dev.off()

#PERMANOVA of PCA groups
pca_test <- pca_points %>% 
  group_by(Habitat, Treatment) %>% 
  select(-Site_trt, -Year, -Treatment, -PlotID) 

perm <- adonis(pca_test[c(3:14)] ~ pca_test$Treatment * pca_test$Habitat, method='eu')

perm

#### plot 3: mean trait values by plot ####
anova_trait <- traitMean %>% 
  group_by(Trait) %>% 
  nest(data = -Trait) %>% 
  mutate(
    aov = map(data, ~ aov(mean ~ Treatment*Site, data = .x)),
    aov_tidy = map(aov, tidy)
  ) 


anova_trait <- anova_trait %>% 
  select(Trait, aov_tidy) %>% 
  unnest(aov_tidy)




anova_text_trait <- anova_trait %>% ungroup() %>% mutate(Trait = plyr::mapvalues(Trait, from = c("SLA_cm2_g", "LDMC", "Leaf_Area_cm2", "Leaf_Thickness_mm", "N_percent", "C_percent", "P_Ave", "CN_ratio", "dC13_percent", "dN15_percent", "Dry_Mass_g", "Plant_Height_cm"), to = c("`SLA`*` `*(cm^2/g)", "`LDMC`*` `*(g/g)", "'Leaf'*' '*'Area'*' '*(cm^2)", "'Leaf'*' '*'Thickness'*' '*(mm)", "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')", "'Dry'*' '*'Mass'*' '*'(g)'", "'Plant'*' '*'Height'*' '*'(cm)'"))) %>% 
  mutate(Trait = factor(Trait, levels = c("'Plant'*' '*'Height'*' '*'(cm)'", "`SLA`*` `*(cm^2/g)", "'Dry'*' '*'Mass'*' '*'(g)'","'Leaf'*' '*'Area'*' '*(cm^2)",  "'Leaf'*' '*'Thickness'*' '*(mm)", "`LDMC`*` `*(g/g)",  "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')" ))) %>% 
  mutate(term = plyr::mapvalues(term, from = c("Treatment", "Site", "Treatment:Site"), to = c("T", "H", "TxH"))) %>% 
  filter(term != "Residuals") %>% 
  mutate(test = paste(term, ifelse(p.value < 0.05, "*", ifelse(p.value<0.1 & p.value > 0.05, "+", "")), sep = " ")) %>% 
  mutate(test = ifelse(grepl("\\*", test), test, ifelse(grepl("\\+", test), test, NA))) %>% 
  pivot_wider(id_cols = Trait, names_from = term, values_from = test) %>% 
  mutate(T = ifelse(is.na(T), "", T)) %>% 
  mutate(H = ifelse(is.na(H), "", H)) %>% 
  mutate(text = trimws(ifelse(!is.na(TxH), TxH, paste(T, H))))




trait_mean <- traitMean %>% ungroup() %>%  mutate(Trait = plyr::mapvalues(Trait, from = c("SLA_cm2_g", "LDMC", "Leaf_Area_cm2", "Leaf_Thickness_mm", "N_percent", "C_percent", "P_Ave", "CN_ratio", "dC13_percent", "dN15_percent", "Dry_Mass_g", "Plant_Height_cm"), to = c("`SLA`*` `*(cm^2/g)", "`LDMC`*` `*(g/g)", "'Leaf'*' '*'Area'*' '*(cm^2)", "'Leaf'*' '*'Thickness'*' '*(mm)", "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')", "'Dry'*' '*'Mass'*' '*'(g)'", "'Plant'*' '*'Height'*' '*'(cm)'"))) %>% 
  mutate(Trait = factor(Trait, levels = c("'Plant'*' '*'Height'*' '*'(cm)'", "`SLA`*` `*(cm^2/g)", "'Dry'*' '*'Mass'*' '*'(g)'","'Leaf'*' '*'Area'*' '*(cm^2)",  "'Leaf'*' '*'Thickness'*' '*(mm)", "`LDMC`*` `*(g/g)",  "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')" )))  %>% 
  filter(Year == "2015") %>% 
  filter(Year == 2015) %>% 
  #gather(key = key, value = value, -Site, -Year, -PlotID, -Trait, -Treatment, -Site_trt) %>%  #filter(trait == "Dry_Mass_g" | trait == "LDMC" | trait == "Leaf_Area_cm2" | trait == "Leaf_Thickness_Ave_mm" | trait == "Plant_Height_cm" | trait == "SLA_cm2_g") %>% #filter(key != "diff", key != "prop_diff") %>% 
  filter(PlotID != "CAS-4", PlotID != "CAS-9", PlotID != "CAS-10", PlotID != "CAS-6") %>% 
  group_by(Trait) %>% 
  mutate(y_max = max(mean), y_min = min(mean)) %>% 
  ggplot() +
  geom_boxplot(aes(x = Site, y = mean, fill = Treatment)) +
  scale_fill_manual(values = c("darkgray", "red")) +
  geom_blank(aes(y = y_max + 0.1*abs(y_max))) +
  ylab("CWM Trait Value") +
  xlab("Habitat Type") +
  theme_classic() +
  theme(text = element_text(size = 15),
        strip.background = element_blank(),
        legend.position = "top") +
  facet_wrap(~Trait, scales = "free", labeller = label_parsed) +
  geom_text(aes(label = text, x = 0, y = Inf, hjust = -0.15, vjust = 1), size = 3.5, color = "black",  data = anova_text_trait) 

jpeg("ITEX_analyses/output/Fig_3_trait_mean.jpg", width = 9, height = 6, units = "in", res = 400)
trait_mean
dev.off()

#### plot 4: ITV plot ####
traitMean <- traitMean %>% 
  mutate(itv_diff = mean-mean_noitv)

t_test_itv <- traitMean %>% 
  group_by(Trait, Site, Treatment) %>% 
  summarise(P = t.test(itv_diff, mu = 0)$p.value,
            Sig = ifelse(P < 0.05, "*", ifelse(P<0.1 & P > 0.05, "+", "")),
            MaxWidth = max(itv_diff))%>%
  ungroup() %>% 
  mutate(Trait = plyr::mapvalues(Trait, from = c("SLA_cm2_g", "LDMC", "Leaf_Area_cm2", "Leaf_Thickness_mm", "N_percent", "C_percent", "P_Ave", "CN_ratio", "dC13_percent", "dN15_percent", "Dry_Mass_g", "Plant_Height_cm"), to = c("`SLA`*` `*(cm^2/g)", "`LDMC`*` `*(g/g)", "'Leaf'*' '*'Area'*' '*(cm^2)", "'Leaf'*' '*'Thickness'*' '*(mm)", "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')", "'Dry'*' '*'Mass'*' '*'(g)'", "'Plant'*' '*'Height'*' '*'(cm)'"))) %>% 
  mutate(Trait = factor(Trait, levels = c("'Plant'*' '*'Height'*' '*'(cm)'", "`SLA`*` `*(cm^2/g)", "'Dry'*' '*'Mass'*' '*'(g)'","'Leaf'*' '*'Area'*' '*(cm^2)",  "'Leaf'*' '*'Thickness'*' '*(mm)", "`LDMC`*` `*(g/g)",  "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')" )))


itv_plot <- traitMean %>% mutate(Trait = plyr::mapvalues(Trait, from = c("SLA_cm2_g", "LDMC", "Leaf_Area_cm2", "Leaf_Thickness_mm", "N_percent", "C_percent", "P_Ave", "CN_ratio", "dC13_percent", "dN15_percent", "Dry_Mass_g", "Plant_Height_cm"), to = c("`SLA`*` `*(cm^2/g)", "`LDMC`*` `*(g/g)", "'Leaf'*' '*'Area'*' '*(cm^2)", "'Leaf'*' '*'Thickness'*' '*(mm)", "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')", "'Dry'*' '*'Mass'*' '*'(g)'", "'Plant'*' '*'Height'*' '*'(cm)'"))) %>% 
  mutate(Trait = factor(Trait, levels = c("'Plant'*' '*'Height'*' '*'(cm)'", "`SLA`*` `*(cm^2/g)", "'Dry'*' '*'Mass'*' '*'(g)'","'Leaf'*' '*'Area'*' '*(cm^2)",  "'Leaf'*' '*'Thickness'*' '*(mm)", "`LDMC`*` `*(g/g)",  "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')" ))) %>%
  filter(Year == 2015) %>% 
  ggplot() +
  geom_boxplot(aes(x = Site, y = itv_diff, fill = Treatment)) +
  geom_hline(aes(yintercept = 0)) +
  geom_blank(aes(x = Site, y = itv_diff + itv_diff*0.6)) + 
  scale_fill_manual(values = c("darkgray", "red")) +
  theme_classic() +
  theme(text = element_text(size = 15),
        strip.background = element_blank(),
        legend.position = "top") +
  facet_wrap(~Trait, scales = "free_y", labeller = label_parsed) +
  geom_text(aes(label = Sig, y = Inf, x = Site, group = Treatment, vjust = 1), position = position_dodge(0.75), data = t_test_itv, size = 4.5) +
  xlab("Habitat Type") +
  ylab("Mean Trait Value - Mean Trait Value (no ITV)")

jpeg("ITEX_analyses/output/Fig_4_itv_plot.jpg", width = 8.75, height = 6, units = "in", res = 400)
itv_plot
dev.off()


#### turnover vs intraspecific variation ####
source("ITEX_analyses/functions/inter_intra_anova.R")

var_split <- traitMean %>%
  group_by(Trait) %>% 
  do(test = trait.flex.anova(~Site * Treatment, mean, mean_noitv, data = .)) 

var_split_exp <- data.frame(RelSumSq.Turnover = 1000, RelSumSq.Intraspec. = 1000, RelSumSq.Covariation = 1000, RelSumSq.Total = 1000, trait = "E", level = "F")

for(i in 1:nrow(var_split)){
  out <- as.data.frame(var_split$test[[i]][2])
  out$trait <- as.factor(rep(var_split[i,1], 5))
  out$level <- rownames(out)
  var_split_exp <- rbind(var_split_exp, out)
}

var_split <- var_split_exp %>% 
  mutate(level = trimws(level)) %>% 
  filter(RelSumSq.Turnover < 999) %>% 
  rename(Turnover = RelSumSq.Turnover, Intraspecific = RelSumSq.Intraspec., Covariation = RelSumSq.Covariation, Total = RelSumSq.Total) %>% 
  mutate(level = plyr::mapvalues(level, from = c("Site", "Site:Treatment"), to = c("Habitat", "Habitat:Treatment"))) %>% 
  gather(key = variable, value = value, -trait, -level) %>% 
  filter(variable == "Total") %>% 
  filter(level != "Total") %>% 
  mutate(level = factor(level, levels = c("Habitat", "Treatment", "Habitat:Treatment", "Residuals"))) %>% 
  mutate(level = plyr::mapvalues(level, from = c("Habitat", "Treatment", "Habitat:Treatment", "Residuals"), to = c("H", "T", "HxT", "Resid"))) %>% 
  mutate(trait = plyr::mapvalues(trait, from = c("SLA_cm2_g", "LDMC", "Leaf_Area_cm2", "Leaf_Thickness_mm", "N_percent", "C_percent", "P_Ave", "CN_ratio", "dC13_percent", "dN15_percent", "Dry_Mass_g", "Plant_Height_cm"), to = c("`SLA`*` `*(cm^2/g)", "`LDMC`*` `*(g/g)", "'Leaf'*' '*'Area'*' '*(cm^2)", "'Leaf'*' '*'Thickness'*' '*(mm)", "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')", "'Dry'*' '*'Mass'*' '*'(g)'", "'Plant'*' '*'Height'*' '*'(cm)'"))) %>% 
  mutate(trait = factor(trait, levels = c("'Plant'*' '*'Height'*' '*'(cm)'", "`SLA`*` `*(cm^2/g)", "'Dry'*' '*'Mass'*' '*'(g)'","'Leaf'*' '*'Area'*' '*(cm^2)",  "'Leaf'*' '*'Thickness'*' '*(mm)", "`LDMC`*` `*(g/g)",  "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')" )))

varpart_graph <- var_split_exp %>% 
  mutate(level = trimws(level)) %>% 
  filter(RelSumSq.Turnover < 999) %>% 
  rename(Turnover = RelSumSq.Turnover, Intraspecific = RelSumSq.Intraspec., Covariation = RelSumSq.Covariation, Total = RelSumSq.Total) %>% 
  mutate(level = plyr::mapvalues(level, from = c("Site", "Site:Treatment"), to = c("Habitat", "Habitat:Treatment"))) %>% 
  gather(key = variable, value = value, -trait, -level) %>% 
  filter(variable != "Covariation", level != "Total", variable != "Total") %>% 
  mutate(level = factor(level, levels = c("Habitat", "Treatment", "Habitat:Treatment", "Residuals"))) %>% 
  mutate(level = plyr::mapvalues(level, from = c("Habitat", "Treatment", "Habitat:Treatment", "Residuals"), to = c("H", "T", "HxT", "Resid"))) %>% 
  mutate(trait = plyr::mapvalues(trait, from = c("SLA_cm2_g", "LDMC", "Leaf_Area_cm2", "Leaf_Thickness_mm", "N_percent", "C_percent", "P_Ave", "CN_ratio", "dC13_percent", "dN15_percent", "Dry_Mass_g", "Plant_Height_cm"), to = c("`SLA`*` `*(cm^2/g)", "`LDMC`*` `*(g/g)", "'Leaf'*' '*'Area'*' '*(cm^2)", "'Leaf'*' '*'Thickness'*' '*(mm)", "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')", "'Dry'*' '*'Mass'*' '*'(g)'", "'Plant'*' '*'Height'*' '*'(cm)'"))) %>% 
  mutate(trait = factor(trait, levels = c("'Plant'*' '*'Height'*' '*'(cm)'", "`SLA`*` `*(cm^2/g)", "'Dry'*' '*'Mass'*' '*'(g)'","'Leaf'*' '*'Area'*' '*(cm^2)",  "'Leaf'*' '*'Thickness'*' '*(mm)", "`LDMC`*` `*(g/g)",  "'N'*' '*'(%)'", "'C'*' '*'(%)'", "'P'*' '*'(%)'", "'C'*':'*'N'", "paste(delta^13, 'C'*' '*'(\u2030)')", "paste(delta^15, 'N'*' '*'(\u2030)')" ))) %>% 
  ggplot() +
  geom_bar(aes(x = level, y = value, fill = variable), stat = "identity") +
  geom_point(aes(x = level, y  = value), data = var_split, size = 1) +
    facet_wrap(~trait, nrow = 3, labeller = label_parsed) +
  theme_classic() +
  theme(text = element_text(size = 15), legend.position = "top",
        strip.background = element_blank()) +
  xlab(NULL) +
  ylab("Proportion Variation Explained") +
  scale_fill_manual(values = c("blue", "darkorange"), name = "Source of Variation") +
  scale_x_discrete(drop = FALSE)

jpeg(file = "ITEX_analyses/output/Fig_S7_varpart_graph.jpg", width = 7.5, height = 9, units = "in", res = 400)
varpart_graph
dev.off()

