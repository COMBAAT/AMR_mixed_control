# Generating plots for the manuscript
rm(list = ls()[!grepl("^(plot|df|this_NW_set|this_vector_measure)", ls())])
source("funcs/plot_helper.R")
library(patchwork)
library(cowplot)

#names(plots3T)
names(plots2F)[grepl("plot_type1", names(plots2F))]


# ##############################################################################
figX.1 <- plots1F$plot_type5_prevalence_ttype1_spec_FALSE_FALSE_vector_value_20 + 
  ggtitle("Cooperative") + 
  guides(shape = "none") +
  ylim(0, 1)
figX.2 <- plots1F$plot_type5_RiskA_ttype1_spec_FALSE_FALSE_vector_value_20 + 
  guides(shape = "none") +
  ylim(0, 10)
figX.3 <- plots1F$plot_type5_Rres_final_ttype1_spec_FALSE_FALSE_vector_value_20_fit_adj_0.8 + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  guides(shape = "none") +
  ylim(0, 10)

figX.4 <- plots1T$plot_type5_prevalence_ttype1_spec_FALSE_TRUE_vector_value_20 + 
  ggtitle("Local") + 
  guides(shape = "none") +
  ylim(0, 1)
figX.5 <- plots1T$plot_type5_RiskA_ttype1_spec_FALSE_TRUE_vector_value_20 + 
  guides(shape = "none") +
  ylim(0, 10)
figX.6 <- plots1T$plot_type5_Rres_final_ttype1_spec_FALSE_TRUE_vector_value_20_fit_adj_0.8 + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  guides(shape = "none") +
  ylim(0, 10)



figA <- ( (figX.1 + figX.4) / (figX.2 + figX.5) / (figX.3 + figX.6)) +
  plot_layout(guides = 'collect') & 
  theme(legend.position = "bottom") 
ggsave("output/ms_figs/use_as_fig5_former4_NEW.pdf", figA, width = 10, height = 9) # best size

# ##############################################################################
# Figure 5 


# ##############################################################################
# ##############################################################################
# Figure 6 

figX.1 <- plots1F$plot_type6_Rres_final_ttype1_spec_FALSE_FALSE_vector_value_20_fit_adj_0.8 + 
  ggtitle(my_title("curative", split_across_lines = "other")) + guides(linetype = "none") + 
coord_cartesian(ylim = c(0, 10))
figX.2 <- plots2F$plot_type6_Rres_final_ttype2_spec_FALSE_FALSE_vector_value_20_fit_adj_0.8 + 
  ggtitle(my_title("longlasting", split_across_lines = "other")) + guides(linetype = "none") + 
coord_cartesian(ylim = c(0, 10)) 
figX.3 <- plots3F$plot_type6_Rres_final_ttype3_spec_FALSE_FALSE_vector_value_20_fit_adj_0.8 + 
  ggtitle(my_title("ongoing", split_across_lines = "other")) + guides(linetype = "none") +  
coord_cartesian(ylim = c(0, 10))


figA_with_legend <- ( (figX.1) / figX.2 ) + plot_layout(guides = 'collect', axes = 'collect') & 
  theme(legend.position = "right", axis.text = element_text(size = 10), legend.box = "horizontal") 
legend <- cowplot::get_legend(figA_with_legend)


figAB <- ( (figX.1) / figX.2 / figX.3 ) + plot_layout(guides = 'collect') & 
  theme(legend.position = "none", axis.text = element_text(size = 10)) 
figAB_with_legend <- (figAB + cowplot::ggdraw(legend)) + plot_layout(ncol = 2)
ggsave("output/ms_figs/use_as_fig6A.pdf", figAB_with_legend, width = 8.5, height = 8.5) # best size

# ##############################################################################
figX.1 <- plots1T$plot_type6_Rres_final_ttype1_spec_FALSE_TRUE_vector_value_20_fit_adj_0.8 + 
  ggtitle(my_title("curative", split_across_lines = "other")) + guides(linetype = "none") + 
  coord_cartesian(ylim = c(0, 10))
figX.2 <- plots2T$plot_type6_Rres_final_ttype2_spec_FALSE_TRUE_vector_value_20_fit_adj_0.8 + 
  ggtitle(my_title("longlasting", split_across_lines = "other")) + guides(linetype = "none") + 
  coord_cartesian(ylim = c(0, 10)) 
figX.3 <- plots3T$plot_type6_Rres_final_ttype3_spec_FALSE_TRUE_vector_value_20_fit_adj_0.8 + 
  ggtitle(my_title("ongoing", split_across_lines = "other")) + guides(linetype = "none") +  
  coord_cartesian(ylim = c(0, 10))


figA_with_legend <- ( (figX.1) / figX.2 ) + plot_layout(guides = 'collect', axes = 'collect') & 
  theme(legend.position = "right", axis.text = element_text(size = 10), legend.box = "horizontal") 
legend <- cowplot::get_legend(figA_with_legend)


figAB <- ( (figX.1) / figX.2 / figX.3 ) + plot_layout(guides = 'collect') & 
  theme(legend.position = "none", axis.text = element_text(size = 10)) 
figAB_with_legend <- (figAB + cowplot::ggdraw(legend)) + plot_layout(ncol = 2)
ggsave("output/ms_figs/use_as_fig6B.pdf", figAB_with_legend, width = 8.5, height = 8.5) # best size

# ##############################################################################
