# Generating plots for the manuscript
rm(list = ls()[!grepl("^(plot|df|this_NW_set|this_vector_measure|saved_simulations)", ls())])
source("funcs/plot_helper.R")
library(patchwork)
library(cowplot)
library(ggtext)

#names(plots3T)
names(plots2F)[grepl("plot_type1", names(plots2F))]


# ##############################################################################

############################################


############################################
this_font_size = 16
figX.1 <- plots1F$plot_type15_invasion_1_treatment_code1_mainvecpop_FALSE_vector_value_20_fit_adj_0.6_single_panel_with_contours_TRUE
#figX.1 <- plots1F$plot_type15_invasion_1_ttype1_spec_FALSE_FALSE_vector_value_20_fit_adj_0.6_single_panel
figX.1 <- add_fancy_title(figX.1, "Collective insecticide", "", "Relative fitness 0.6" )

figX.2 <- plots1T$plot_type15_invasion_1_treatment_code1_mainvecpop_TRUE_vector_value_20_fit_adj_0.6_single_panel_with_contours_TRUE
#figX.2 <- plots1T$plot_type15_invasion_1_ttype1_spec_FALSE_TRUE_vector_value_20_fit_adj_0.6_single_panel 
figX.2 <- add_fancy_title(figX.2, "Local insecticide", "", "Relative fitness 0.6" )

figX.3 <- plots1F$plot_type15_invasion_1_treatment_code1_mainvecpop_FALSE_vector_value_20_fit_adj_0.8_single_panel_with_contours_TRUE 
#figX.3 <- plots1F$plot_type15_invasion_1_ttype1_spec_FALSE_FALSE_vector_value_20_fit_adj_0.8_single_panel 
figX.3 <- add_fancy_title(figX.3, "", "", "Relative fitness 0.8" )

figX.4 <- plots1T$plot_type15_invasion_1_treatment_code1_mainvecpop_TRUE_vector_value_20_fit_adj_0.8_single_panel_with_contours_TRUE
#figX.4 <- plots1T$plot_type15_invasion_1_ttype1_spec_FALSE_TRUE_vector_value_20_fit_adj_0.8_single_panel
figX.4 <- add_fancy_title(figX.4, "", "", "Relative fitness 0.8" )

figX.5 <- plots1F$plot_type15_invasion_1_treatment_code1_mainvecpop_FALSE_vector_value_20_fit_adj_0.95_single_panel_with_contours_TRUE
#figX.5 <- plots1F$plot_type15_invasion_1_ttype1_spec_FALSE_FALSE_vector_value_20_fit_adj_0.95_single_panel 
figX.5 <- add_fancy_title(figX.5, "", "", "Relative fitness 0.95" )

figX.6 <- plots1T$plot_type15_invasion_1_treatment_code1_mainvecpop_TRUE_vector_value_20_fit_adj_0.95_single_panel_with_contours_TRUE
figX.6 <- add_fancy_title(figX.6, "", "", "Relative fitness 0.95" )


row1 <- (figX.1 + plot_spacer() + figX.2) + plot_layout(widths = c(1, 0.1, 1))
row2 <- (figX.3 + plot_spacer() + figX.4) + plot_layout(widths = c(1, 0.1, 1))
row3 <- (figX.5 + plot_spacer() + figX.6) + plot_layout(widths = c(1, 0.1, 1))

figB <- row1 / row2 / row3 + plot_layout(guides = 'collect')
figB <- figB + plot_annotation(title = "Responsive curative treatment",
                               theme = theme(plot.title = element_text(size = 24, hjust = 0.4)))
my_ggsave(plot = figB, filename = "output/ms_figs/5D_supp_mat_fig7a.pdf", width = 14, height = 14) # best size

############################################

this_font_size = 16
figX.1 <- plots2F$plot_type15_invasion_1_treatment_code2_mainvecpop_FALSE_vector_value_20_fit_adj_0.6_single_panel_with_contours_TRUE
#figX.1 <- plots2F$plot_type15_invasion_1_ttype2_spec_FALSE_FALSE_vector_value_20_fit_adj_0.6_single_panel
figX.1 <- add_fancy_title(figX.1, "Collective insecticide", "", "Relative fitness 0.6" )

figX.2 <- plots2T$plot_type15_invasion_1_treatment_code2_mainvecpop_TRUE_vector_value_20_fit_adj_0.6_single_panel_with_contours_TRUE
#figX.2 <- plots2T$plot_type15_invasion_1_ttype2_spec_FALSE_TRUE_vector_value_20_fit_adj_0.6_single_panel 
figX.2 <- add_fancy_title(figX.2, "Local insecticide", "", "Relative fitness 0.6" )

figX.3 <- plots2F$plot_type15_invasion_1_treatment_code2_mainvecpop_FALSE_vector_value_20_fit_adj_0.8_single_panel_with_contours_TRUE 
#figX.3 <- plots2F$plot_type15_invasion_1_ttype2_spec_FALSE_FALSE_vector_value_20_fit_adj_0.8_single_panel 
figX.3 <- add_fancy_title(figX.3, "", "", "Relative fitness 0.8" )

figX.4 <- plots2T$plot_type15_invasion_1_treatment_code2_mainvecpop_TRUE_vector_value_20_fit_adj_0.8_single_panel_with_contours_TRUE
#figX.4 <- plots2T$plot_type15_invasion_1_ttype2_spec_FALSE_TRUE_vector_value_20_fit_adj_0.8_single_panel
figX.4 <- add_fancy_title(figX.4, "", "", "Relative fitness 0.8" )

figX.5 <- plots2F$plot_type15_invasion_1_treatment_code2_mainvecpop_FALSE_vector_value_20_fit_adj_0.95_single_panel_with_contours_TRUE
#figX.5 <- plots2F$plot_type15_invasion_1_ttype2_spec_FALSE_FALSE_vector_value_20_fit_adj_0.95_single_panel 
figX.5 <- add_fancy_title(figX.5, "", "", "Relative fitness 0.95" )

figX.6 <- plots2T$plot_type15_invasion_1_treatment_code2_mainvecpop_TRUE_vector_value_20_fit_adj_0.95_single_panel_with_contours_TRUE
#figX.6 <- plots2T$plot_type15_invasion_1_ttype2_spec_FALSE_TRUE_vector_value_20_fit_adj_0.95_single_panel
figX.6 <- add_fancy_title(figX.6, "", "", "Relative fitness 0.95" )


row1 <- (figX.1 + plot_spacer() + figX.2) + plot_layout(widths = c(1, 0.1, 1))
row2 <- (figX.3 + plot_spacer() + figX.4) + plot_layout(widths = c(1, 0.1, 1))
row3 <- (figX.5 + plot_spacer() + figX.6) + plot_layout(widths = c(1, 0.1, 1))

figB <- row1 / row2 / row3 + plot_layout(guides = 'collect')
figB <- figB + plot_annotation(title = "Responsive longlasting treatment",
                               theme = theme(plot.title = element_text(size = 24, hjust = 0.4)))
my_ggsave(plot = figB, filename = "output/ms_figs/5D_supp_mat_fig7b.pdf", width = 14, height = 14) # best size

############################################
this_font_size = 16
figX.1 <- plots3F$plot_type15_invasion_1_treatment_code3_mainvecpop_FALSE_vector_value_20_fit_adj_0.6_single_panel_with_contours_TRUE
figX.1 <- add_fancy_title(figX.1, "Collective insecticide", "", "Relative fitness 0.6" )

figX.2 <- plots3T$plot_type15_invasion_1_treatment_code3_mainvecpop_TRUE_vector_value_20_fit_adj_0.6_single_panel_with_contours_TRUE
#figX.2 <- plots3T$plot_type15_invasion_1_ttype3_spec_FALSE_TRUE_vector_value_20_fit_adj_0.6_single_panel 
figX.2 <- add_fancy_title(figX.2, "Local insecticide", "", "Relative fitness 0.6" )

figX.3 <- plots3F$plot_type15_invasion_1_treatment_code3_mainvecpop_FALSE_vector_value_20_fit_adj_0.8_single_panel_with_contours_TRUE 
#figX.3 <- plots3F$plot_type15_invasion_1_ttype3_spec_FALSE_FALSE_vector_value_20_fit_adj_0.8_single_panel 
figX.3 <- add_fancy_title(figX.3, "", "", "Relative fitness 0.8" )

figX.4 <- plots3T$plot_type15_invasion_1_treatment_code3_mainvecpop_TRUE_vector_value_20_fit_adj_0.8_single_panel_with_contours_TRUE
#figX.4 <- plots3T$plot_type15_invasion_1_ttype3_spec_FALSE_TRUE_vector_value_20_fit_adj_0.8_single_panel
figX.4 <- add_fancy_title(figX.4, "", "", "Relative fitness 0.8" )

figX.5 <- plots3F$plot_type15_invasion_1_treatment_code3_mainvecpop_FALSE_vector_value_20_fit_adj_0.95_single_panel_with_contours_TRUE
#figX.5 <- plots3F$plot_type15_invasion_1_ttype3_spec_FALSE_FALSE_vector_value_20_fit_adj_0.95_single_panel 
figX.5 <- add_fancy_title(figX.5, "", "", "Relative fitness 0.95" )

figX.6 <- plots3T$plot_type15_invasion_1_treatment_code3_mainvecpop_TRUE_vector_value_20_fit_adj_0.95_single_panel_with_contours_TRUE
#figX.6 <- plots3T$plot_type15_invasion_1_ttype3_spec_FALSE_TRUE_vector_value_20_fit_adj_0.95_single_panel
figX.6 <- add_fancy_title(figX.6, "", "", "Relative fitness 0.95" )


row1 <- (figX.1 + plot_spacer() + figX.2) + plot_layout(widths = c(1, 0.1, 1))
row2 <- (figX.3 + plot_spacer() + figX.4) + plot_layout(widths = c(1, 0.1, 1))
row3 <- (figX.5 + plot_spacer() + figX.6) + plot_layout(widths = c(1, 0.1, 1))

figB <- row1 / row2 / row3 + plot_layout(guides = 'collect')
figB <- figB + plot_annotation(title = "Ongoing longlasting treatment",
                               theme = theme(plot.title = element_text(size = 24, hjust = 0.4)))
my_ggsave(plot = figB, filename = "output/ms_figs/5D_supp_mat_fig7c.pdf", width = 14, height = 14) # best size

############################################
