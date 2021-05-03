# Analysis and Figure plan

AnalysisFigurePlan <- drake_plan(
  
  ### ITEX
  ## Diversity
  diversity_Itex = ITEX_Diversity(community_itex),
  diversity_Itex_Figure = ITEX_Diversity_Figure(diversity_Itex),
  
  # Ordination
  fNMDS_Itex = ITEX_Ordination(community_itex),
  ITEX_Ordination = MakeITEXOrdination(fNMDS_Itex)
  
  
)