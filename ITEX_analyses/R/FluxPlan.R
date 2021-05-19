# Flux plan

FluxPlan <- drake_plan(
  
  # Calculate GPP
  ITEX.data.post.calcs = calc_GPP(ITEX.data.pre.calcs),
  
  # Standardize fluxes
  Standard_Fluxes = standardize_fluxes(CommResp, Height, ITEX.data.post.calcs),
  
  # Join with traits
  Flux_and_Traits = flux_and_trait(Trait_Mean, Standard_Fluxes),
  
  # Effect size plot
  Fig_X_Effect_size = make_effect_size_figure(Flux_and_Traits),
  
  # Model selection
  Trait_Model_Output = trait_model_selelction(Flux_and_Traits),
  Model_Output = model_selection(Flux_and_Traits),
  
  # Flux Figure
  Fig_5_Fluxes = make_flux_figure(Trait_Model_Output, Model_Output)
)