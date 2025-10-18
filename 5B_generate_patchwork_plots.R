# Generating plots for the manuscript
rm(list = ls()[!grepl("^(plot|df|this_NW_set|this_vector_measure)", ls())])
source("funcs/plot_helper.R")
library(patchwork)
library(cowplot)

#names(plots3T)
names(plots2F)[grepl("plot_type1", names(plots2F))]


# ##############################################################################

# ##############################################################################
coop_local_title <- paste0("Cooperative      ", "                ", "                   Local    ")

figX.1 <- plots1F$plot_type15_invasion_1_ttype1_spec_FALSE_FALSE_vector_value_20_fit_adj_0.8_single_panel 
figX.2 <- plots1T$plot_type15_invasion_1_ttype1_spec_FALSE_TRUE_vector_value_20_fit_adj_0.8_single_panel 
figX.3 <- plots2F$plot_type15_invasion_1_ttype2_spec_FALSE_FALSE_vector_value_20_fit_adj_0.8_single_panel 
figX.4 <- plots2T$plot_type15_invasion_1_ttype2_spec_FALSE_TRUE_vector_value_20_fit_adj_0.8_single_panel 
figX.5 <- plots3F$plot_type15_invasion_1_ttype3_spec_FALSE_FALSE_vector_value_20_fit_adj_0.8_single_panel 
figX.6 <- plots3T$plot_type15_invasion_1_ttype3_spec_FALSE_TRUE_vector_value_20_fit_adj_0.8_single_panel


row1 <- (figX.1 + plot_spacer() + figX.2) + plot_layout(widths = c(1, 0.15, 1))
row2 <- (figX.3 + plot_spacer() + figX.4) + plot_layout(widths = c(1, 0.15, 1))
row3 <- (figX.5 + plot_spacer() + figX.6) + plot_layout(widths = c(1, 0.15, 1))

figB <- row1 / row2 / row3 + plot_layout(guides = 'collect')
figB <- figB + plot_annotation(title = coop_local_title,
                               theme = theme(plot.title = element_text(size = 24, hjust = 0.3)))
ggsave("output/ms_figs/use_as_fig7.pdf", figB, width = 14, height = 14) # best size



figX.1 <- plots1F$plot_type16_landscape_ttype1_spec_FALSE_FALSE_vector_value_20_single_panel 
figX.2 <- plots1T$plot_type16_landscape_ttype1_spec_FALSE_TRUE_vector_value_20_single_panel 
figX.3 <- plots2F$plot_type16_landscape_ttype2_spec_FALSE_FALSE_vector_value_20_single_panel 
figX.4 <- plots2T$plot_type16_landscape_ttype2_spec_FALSE_TRUE_vector_value_20_single_panel 
figX.5 <- plots3F$plot_type16_landscape_ttype3_spec_FALSE_FALSE_vector_value_20_single_panel 
figX.6 <- plots3T$plot_type16_landscape_ttype3_spec_FALSE_TRUE_vector_value_20_single_panel 

row1 <- (figX.1 + plot_spacer() + figX.2) + plot_layout(widths = c(1, 0.15, 1))
row2 <- (figX.3 + plot_spacer() + figX.4) + plot_layout(widths = c(1, 0.15, 1))
row3 <- (figX.5 + plot_spacer() + figX.6) + plot_layout(widths = c(1, 0.15, 1))

figB <- row1 / row2 / row3 + plot_layout(guides = 'collect')
figB <- figB + plot_annotation(title = coop_local_title,
                               theme = theme(plot.title = element_text(size = 24, hjust = 0.3)))
ggsave("output/ms_figs/use_as_fig8.pdf", figB, width = 14, height = 14) # best size


############################################
this_font_size = 16
figX.1 <- plots1F$plot_type15_invasion_1_ttype1_spec_FALSE_FALSE_vector_value_20_fit_adj_0.6_single_panel +
  ggtitle(paste0("Relative fitness 0.6")) + 
  theme(plot.title = element_text(size = this_font_size)) 
figX.2 <- plots1T$plot_type15_invasion_1_ttype1_spec_FALSE_TRUE_vector_value_20_fit_adj_0.6_single_panel +
  ggtitle(paste0("Relative fitness 0.6")) + 
  theme(plot.title = element_text(size = this_font_size)) 
figX.3 <- plots1F$plot_type15_invasion_1_ttype1_spec_FALSE_FALSE_vector_value_20_fit_adj_0.8_single_panel +
  ggtitle(paste0("Relative fitness 0.8")) + 
  theme(plot.title = element_text(size = this_font_size)) 
figX.4 <- plots1T$plot_type15_invasion_1_ttype1_spec_FALSE_TRUE_vector_value_20_fit_adj_0.8_single_panel +
  ggtitle(paste0("Relative fitness 0.8")) + 
  theme(plot.title = element_text(size = this_font_size)) 
figX.5 <- plots1F$plot_type15_invasion_1_ttype1_spec_FALSE_FALSE_vector_value_20_fit_adj_0.95_single_panel +
  ggtitle(paste0("Relative fitness 0.95")) + 
  theme(plot.title = element_text(size = this_font_size)) 
figX.6 <- plots1T$plot_type15_invasion_1_ttype1_spec_FALSE_TRUE_vector_value_20_fit_adj_0.95_single_panel +
  ggtitle(paste0("Relative fitness 0.95")) + 
  theme(plot.title = element_text(size = this_font_size)) 


row1 <- (figX.1 + plot_spacer() + figX.2) + plot_layout(widths = c(1, 0.15, 1))
row2 <- (figX.3 + plot_spacer() + figX.4) + plot_layout(widths = c(1, 0.15, 1))
row3 <- (figX.5 + plot_spacer() + figX.6) + plot_layout(widths = c(1, 0.15, 1))

figB <- row1 / row2 / row3 + plot_layout(guides = 'collect')
figB <- figB + plot_annotation(title = coop_local_title,
                               theme = theme(plot.title = element_text(size = 24, hjust = 0.3)))
ggsave("output/ms_figs/use_as_fig9.pdf", figB, width = 14, height = 14) # best size
