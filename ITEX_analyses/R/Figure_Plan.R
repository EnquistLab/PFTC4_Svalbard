# Figure plan

FigurePlan <- drake_plan(
  
  # Community Figures
  Fig_1_metric_change = community_metrics_figure(Comm_Anova_tidy, Comm_Metric_Change, Comm_t_Test),
  Fig_S4_metric_change_supp = community_metrics_figure_supp(Comm_Anova_tidy, Comm_Metric_Change, Comm_t_Test_Supp),
  Fig_S5_metric_time = metric_time_figure(CommResp),
  
  # Ordination
  Fig_S3_CommunityOrdination = make_ordination(NMDS_output),
  
  # Height
  Fig_S6_CanopyHeight = make_height_figure(Height),
  
  # Trait PCA
  Fig_2_pca_plot = make_trait_pca_figure(trait_pca_info, pca_res),
  
  # Trait mean
  Fig_3_trait_mean = make_trait_mean_figure(Anova_Trait_Tidy, Trait_Mean),
  
  # ITV
  Fig_4_itv_plot = make_itv_figure(Trait_Mean),
  
  # Inter vs intra
  Fig_S7_varpart_graph = make_intra_vs_inter_figure(Var_Split_Exp, Var_Split),
  
  # Climate plot
  Fig_S1_FinalClimatePlot = make_climate_figure(Monthly_Temp, Daily_Climate)
  
)





