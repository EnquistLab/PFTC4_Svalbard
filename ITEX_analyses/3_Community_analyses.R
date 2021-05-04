#### COMMUNITY DATA ANALYSIS ####
source("ITEX_analyses/2_Import_data.R")


#### NMDS ORDINATION ####
set.seed(32)

# BISTORTA
comm_fat_BIS <- CommunitySV_ITEX_2003_2015 %>% 
  select(-Taxon, -FunctionalGroup) %>% 
  arrange(Year) %>% 
  spread(key = Spp, value = Abundance, fill = 0) %>% 
  filter(Site == "SB")

comm_fat_spp_BIS <- comm_fat_BIS %>% select(-(Site:Year))

NMDS_BIS <- metaMDS(comm_fat_spp_BIS, noshare = TRUE, try = 30)

fNMDS_BIS <- fortify(NMDS_BIS) %>% 
  filter(Score == "sites") %>% 
  bind_cols(comm_fat_BIS %>% select(Site:Year))

# CASSIOPE
comm_fat_CAS <- CommunitySV_ITEX_2003_2015 %>% 
  select(-Taxon, -FunctionalGroup) %>% 
  arrange(Year) %>% 
  spread(key = Spp, value = Abundance, fill = 0) %>% 
  filter(Site == "CH")

comm_fat_spp_CAS <- comm_fat_CAS %>% select(-(Site:Year))

NMDS_CAS <- metaMDS(comm_fat_spp_CAS, noshare = TRUE, try = 100)

fNMDS_CAS <- fortify(NMDS_CAS) %>% 
  filter(Score == "sites") %>% 
  bind_cols(comm_fat_CAS %>% select(Site:Year))


# DRYAS
comm_fat_DRY <- CommunitySV_ITEX_2003_2015 %>% 
  select(-Taxon, -FunctionalGroup) %>% 
  arrange(Year) %>% 
  spread(key = Spp, value = Abundance, fill = 0) %>% 
  filter(Site == "DH")

comm_fat_spp_DRY <- comm_fat_DRY %>% select(-(Site:Year))

NMDS_DRY <- metaMDS(comm_fat_spp_DRY, noshare = TRUE, try = 100)

fNMDS <- fortify(NMDS_DRY) %>% 
  filter(Score == "sites") %>% 
  bind_cols(comm_fat_DRY %>% select(Site:Year)) %>% 
  bind_rows(fNMDS_BIS, fNMDS_CAS) %>% 
  mutate(Site = factor(Site, levels = c("SB", "CH", "DH")))

# Make figure
CommunityOrdination <- ggplot(fNMDS, aes(x = NMDS1, y = NMDS2, group = PlotID, shape = Treatment, linetype = Treatment)) +
  geom_point(aes(size = ifelse(Year == min(as.numeric(Year)), "First", "Other"))) +
  geom_path() + 
  coord_equal() +
  scale_size_discrete(name = "Year", range = c(1.2, 2.5), limits = c("Other", "First"), breaks = c("First", "Other")) +
  scale_shape_manual(values = c(1, 16)) + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  labs(x = "NMDS axis 1", y = "NMDS axis 2") +
  annotate("text", x = 0.5, y = 0.5, label = "Year*") +
  facet_grid(~ Site) +
  theme_bw()

ggsave(CommunityOrdination, filename = "ITEX_analyses/output/Fig_S3_CommunityOrdination.jpeg", width = 8, height = 3.5, dpi = 300)

