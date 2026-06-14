# ##############################################################################
# Figure 6 

figX.1 <- plots1F$plot_type6_Rres_final_treatment_code1_mainvecpop_FALSE_vector_value_20_fit_adj_0.8 + 
  #figX.1 <- plots1F$plot_type6_Rres_final_ttype1_spec_FALSE_FALSE_vector_value_20_fit_adj_0.8 + 
  ggtitle(my_title("curative", split_across_lines = "other")) + guides(linetype = "none") + 
  coord_cartesian(ylim = c(0, 10))
figX.2 <- plots2F$plot_type6_Rres_final_treatment_code2_mainvecpop_FALSE_vector_value_20_fit_adj_0.8 + 
  #figX.2 <- plots2F$plot_type6_Rres_final_ttype2_spec_FALSE_FALSE_vector_value_20_fit_adj_0.8 + 
  ggtitle(my_title("longlasting", split_across_lines = "other")) + guides(linetype = "none") + 
  coord_cartesian(ylim = c(0, 10)) 
figX.3 <- plots3F$plot_type6_Rres_final_treatment_code3_mainvecpop_FALSE_vector_value_20_fit_adj_0.8 + 
  #figX.3 <- plots3F$plot_type6_Rres_final_ttype3_spec_FALSE_FALSE_vector_value_20_fit_adj_0.8 + 
  ggtitle(my_title("ongoing", split_across_lines = "other")) + guides(linetype = "none") +  
  coord_cartesian(ylim = c(0, 10))


figA_with_legend <- ( (figX.1) / figX.2 ) + plot_layout(guides = 'collect', axes = 'collect') & 
  theme(legend.position = "right", axis.text = element_text(size = 10), legend.box = "horizontal") 
legend <- cowplot::get_legend(figA_with_legend)


figAB <- ( (figX.1) / figX.2 / figX.3 ) + plot_layout(guides = 'collect') & 
  theme(legend.position = "none", axis.text = element_text(size = 10)) 
figAB_with_legend <- (figAB + cowplot::ggdraw(legend)) + plot_layout(ncol = 2)
my_ggsave(plot = figAB_with_legend, filename = "output/ms_figs/5B_spare_figX6.pdf", width = 8.5, height = 8.5) # best size

# ##############################################################################
figX.1 <- plots1T$plot_type6_Rres_final_treatment_code1_mainvecpop_TRUE_vector_value_20_fit_adj_0.8 + 
  #figX.1 <- plots1T$plot_type6_Rres_final_ttype1_spec_FALSE_TRUE_vector_value_20_fit_adj_0.8 + 
  ggtitle(my_title("curative", split_across_lines = "other")) + guides(linetype = "none") + 
  coord_cartesian(ylim = c(0, 10))
figX.2 <- plots2T$plot_type6_Rres_final_treatment_code2_mainvecpop_TRUE_vector_value_20_fit_adj_0.8 + 
  #figX.2 <- plots2T$plot_type6_Rres_final_ttype2_spec_FALSE_TRUE_vector_value_20_fit_adj_0.8 + 
  ggtitle(my_title("longlasting", split_across_lines = "other")) + guides(linetype = "none") + 
  coord_cartesian(ylim = c(0, 10)) 
figX.3 <- plots3T$plot_type6_Rres_final_treatment_code3_mainvecpop_TRUE_vector_value_20_fit_adj_0.8 + 
  #figX.3 <- plots3T$plot_type6_Rres_final_ttype3_spec_FALSE_TRUE_vector_value_20_fit_adj_0.8 + 
  ggtitle(my_title("ongoing", split_across_lines = "other")) + guides(linetype = "none") +  
  coord_cartesian(ylim = c(0, 10))


figA_with_legend <- ( (figX.1) / figX.2 ) + plot_layout(guides = 'collect', axes = 'collect') & 
  theme(legend.position = "right", axis.text = element_text(size = 10), legend.box = "horizontal") 
legend <- cowplot::get_legend(figA_with_legend)


figAB <- ( (figX.1) / figX.2 / figX.3 ) + plot_layout(guides = 'collect') & 
  theme(legend.position = "none", axis.text = element_text(size = 10)) 
figAB_with_legend <- (figAB + cowplot::ggdraw(legend)) + plot_layout(ncol = 2)
my_ggsave(plot = figAB_with_legend, filename = "output/ms_figs/5B_spare_figX6B.pdf", width = 8.5, height = 8.5) # best size

# ##############################################################################
