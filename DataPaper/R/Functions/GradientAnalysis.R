communit_gradient %>% distinct(Taxon) %>% print(n = Inf)


# Calculate Diversity Indices
Gradient_Diversity <- function(communit_gradient){
  diversity_itex = communit_gradient %>% 
    group_by(Gradient, Site, PlotID) %>%  
    summarise(n = n(),
              Richness = n, 
              Diversity = diversity(Cover), 
              Evenness = Diversity/log(Richness),
              sumAbundance = sum(Cover)) %>% 
    pivot_longer(cols = Richness:sumAbundance, names_to = "DiversityIndex", values_to = "Value")

  return(diversity_itex)  
}

diverstiy_gradient = Gradient_Diversity(communit_gradient)


ggplot(diverstiy_gradient, aes(x = Site, y = Value, colour = Gradient)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_colour_manual(values = c("green4", "grey")) +
  facet_wrap(~ DiversityIndex, scales = "free")



# Traits

traits_gradient %>% 
  ggplot(aes(x = Value, fill = Gradient)) +
  geom_density(alpha = 0.5) +
  labs(x = "Mean trait value", y = "Density") +
  facet_wrap(~Traits, scales = "free")
