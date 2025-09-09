# Generating plots for the manuscript
rm(list = ls()[!grepl("^(plot|df|this_NW|this_vector_measure)", ls())])
source("funcs/plot_helper.R")
library(patchwork)

#names(plots3T)
names(plots2F)[grepl("plot_type1", names(plots2F))]

# Figure 1 is the model schematic

# ##############################################################################
# Figure S1
figX.1a <- plots1F$plot_type1_R0sen_ttype1_spec_FALSE_FALSE
figX.1b <- plots2F$plot_type1_R0sen_ttype2_spec_FALSE_FALSE
figX.2a <- plots1F$plot_type1_prevalence_ttype1_spec_FALSE_FALSE
figX.2b <- plots2F$plot_type1_prevalence_ttype2_spec_FALSE_FALSE
figX.3a <- plots1F$plot_type1_Incidence_ttype1_spec_FALSE_FALSE
figX.3b <- plots2F$plot_type1_Incidence_ttype2_spec_FALSE_FALSE
figX.4a <- plots1F$plot_type1_No_trt_cat_ttype1_spec_FALSE_FALSE
figX.4b <- plots2F$plot_type1_No_trt_cat_ttype2_spec_FALSE_FALSE
figX.5a <- plots1F$plot_type1_RiskA_ttype1_spec_FALSE_FALSE
figX.5b <- plots2F$plot_type1_RiskA_ttype2_spec_FALSE_FALSE

figX.1 <- (figX.1a + labs(title = "A") + ggtitle("Curative drug\n ") + plot_spacer() +
             figX.1b + labs(title = "B") + ggtitle("Long-lasting drug\n ")) +
  plot_layout(widths = c(1, 0.1, 1))
#figX.1

figX.2 <- (figX.2a + plot_spacer() + figX.2b) +
  plot_layout(widths = c(1, 0.1, 1))
#figX.2

figX.3 <- (figX.3a + plot_spacer() + figX.3b) +
  plot_layout(widths = c(1, 0.1, 1))
#figX.3

figX.4 <- (figX.4a + plot_spacer() + figX.4b) +
  plot_layout(widths = c(1, 0.1, 1))
#figX.4

figX.5 <- (figX.5a + plot_spacer() + figX.5b) +
  plot_layout(widths = c(1, 0.1, 1))
#figX.5

figX <- figX.1 / figX.2 / figX.3 / figX.4 / figX.5 + plot_layout(guides = 'collect', axes = "collect") &
  theme(legend.position = "bottom") 
figX
#ggsave("output/ms_figs/figS1_large.pdf", figX, width = 11, height = 13) # best size
ggsave("output/ms_figs/pwS1_large.pdf", figX, width = 11, height = 13, dpi = 150) 
print("here1")

# ##############################################################################
# Figure S2
figX.1a <- plots3F$plot_type1_R0sen_ttype3_spec_FALSE_FALSE
figX.1b <- plots2F$plot_type1_R0sen_ttype2_spec_FALSE_FALSE
figX.2a <- plots3F$plot_type1_prevalence_ttype3_spec_FALSE_FALSE
figX.2b <- plots2F$plot_type1_prevalence_ttype2_spec_FALSE_FALSE
figX.3a <- plots3F$plot_type1_Incidence_ttype3_spec_FALSE_FALSE
figX.3b <- plots2F$plot_type1_Incidence_ttype2_spec_FALSE_FALSE
figX.4a <- plots3F$plot_type1_No_trt_cat_ttype3_spec_FALSE_FALSE
figX.4b <- plots2F$plot_type1_No_trt_cat_ttype2_spec_FALSE_FALSE
figX.5a <- plots3F$plot_type1_RiskA_ttype3_spec_FALSE_FALSE
figX.5b <- plots2F$plot_type1_RiskA_ttype2_spec_FALSE_FALSE

figX.1 <- (figX.1a + labs(title = "A") + ggtitle("Ongoing long-lasting drug\n ") + plot_spacer() +
             figX.1b + labs(title = "B") + ggtitle("Responsive long-lasting drug\n ")) +
  plot_layout(widths = c(1, 0.1, 1))
#figX.1

figX.2 <- (figX.2a + plot_spacer() + figX.2b) +
  plot_layout(widths = c(1, 0.1, 1))
#figX.2

figX.3 <- (figX.3a + plot_spacer() + figX.3b) +
  plot_layout(widths = c(1, 0.1, 1))
#figX.3

figX.4 <- (figX.4a + plot_spacer() + figX.4b) +
  plot_layout(widths = c(1, 0.1, 1))
#figX.4

figX.5 <- (figX.5a + plot_spacer() + figX.5b) +
  plot_layout(widths = c(1, 0.1, 1))
#figX.5

figX <- figX.1 / figX.2 / figX.3 / figX.4 / figX.5 + plot_layout(guides = 'collect', axes = "collect") & 
  #plot_annotation(tag_levels = 'A') & 
  #theme_bw(base_size = 16) & 
  theme(legend.position = "bottom") 
figX
#ggsave("output/ms_figs/pwS2.pdf", figX, width = 11, height = 13) # best size

# ##############################################################################

# ##############################################################################
# Figure 2 shows the impact of controls on the epi variables prevalence and incidence
# Decline in prevalence is a bit misleading because it
# mostly captures the shortening of the infected period
# rather than a reduction in force of infection
# as demonstrated by the incidence patterns
figX.1a <- plots1F$plot_type1_prevalence_ttype1_spec_FALSE_FALSE
figX.1b <- plots2F$plot_type1_prevalence_ttype2_spec_FALSE_FALSE
figX.2a <- plots1F$plot_type1_Incidence_ttype1_spec_FALSE_FALSE
figX.2b <- plots2F$plot_type1_Incidence_ttype2_spec_FALSE_FALSE

figX.1 <- (figX.1a + labs(title = "A") + ggtitle("Curative drug\n ") + plot_spacer() +
             figX.1b + labs(title = "B") + ggtitle("Long-lasting drug\n ")) +
  plot_layout(widths = c(1, 0.1, 1))
#figX.1

figX.2 <- (figX.2a + plot_spacer() + figX.2b) +
  plot_layout(widths = c(1, 0.1, 1))
#figX.2

figX <- figX.1 / figX.2 + plot_layout(guides = 'collect', axes = "collect") & 
  #plot_annotation(tag_levels = 'A') & 
  #theme_bw(base_size = 16) & 
  theme(legend.position = "bottom") 
figX
ggsave("output/ms_figs/pw2.pdf", figX, width = 11, height = 6.5) # best size
# ##############################################################################
# ##############################################################################
# Figure 2 shows the impact of controls on the epi variables prevalence and incidence
# Decline in prevalence is a bit misleading because it
# mostly captures the shortening of the infected period
# rather than a reduction in force of infection
# as demonstrated by the incidence patterns
figX.1a <- plots1F$plot_type1_RiskA_ttype1_spec_FALSE_FALSE
figX.1b <- plots2F$plot_type1_RiskA_ttype2_spec_FALSE_FALSE
figX.2a <- plots1F$plot_type1_RiskE_ttype1_spec_FALSE_FALSE
figX.2b <- plots2F$plot_type1_RiskE_ttype2_spec_FALSE_FALSE

figX.1 <- (figX.1a + labs(title = "A") + ggtitle("Curative drug\n ") + plot_spacer() +
             figX.1b + labs(title = "B") + ggtitle("Long-lasting drug\n ")) +
  plot_layout(widths = c(1, 0.1, 1))
#figX.1

figX.2 <- (figX.2a + plot_spacer() + figX.2b) +
  plot_layout(widths = c(1, 0.1, 1))
#figX.2

figX <- figX.1 / figX.2 + plot_layout(guides = 'collect', axes = "collect") & 
  #plot_annotation(tag_levels = 'A') & 
  #theme_bw(base_size = 16) & 
  theme(legend.position = "bottom") 
figX
ggsave("output/ms_figs/pw2_options.pdf", figX, width = 11, height = 6.5) # best size
# ##############################################################################
# Figure S1
figX.1a <- plots1F$plot_type1_R0sen_ttype1_spec_FALSE_FALSE
figX.1b <- plots2F$plot_type1_R0sen_ttype2_spec_FALSE_FALSE
figX.2a <- plots1F$plot_type1_No_trt_cat_ttype1_spec_FALSE_FALSE
figX.2b <- plots2F$plot_type1_No_trt_cat_ttype2_spec_FALSE_FALSE

figX.1 <- (figX.1a + labs(title = "A") + ggtitle("Curative drug\n ") + plot_spacer() +
             figX.1b + labs(title = "B") + ggtitle("Long-lasting drug\n ")) +
  plot_layout(widths = c(1, 0.1, 1))
#figX.1

figX.2 <- (figX.2a + plot_spacer() + figX.2b) +
  plot_layout(widths = c(1, 0.1, 1))
#figX.2

figX <- figX.1 / figX.2 + plot_layout(guides = 'collect', axes = "collect") & 
  #plot_annotation(tag_levels = 'A') & 
  #theme_bw(base_size = 16) & 
  theme(legend.position = "bottom") 
figX
ggsave("output/ms_figs/pw1_small.pdf", figX, width = 11, height = 6.5) # best size

# ##############################################################################


# ##############################################################################
# Figure 3 shows the impact of insecticide resistance
figX.1a <- plots1F$plot_type4_R0sen_ttype1_spec_FALSE_FALSE
figX.1b <- plots2F$plot_type4_R0sen_ttype2_spec_FALSE_FALSE
figX.1a <- plots1F$plot_type4_No_trt_cat_ttype1_spec_FALSE_FALSE
figX.1b <- plots2F$plot_type4_No_trt_cat_ttype2_spec_FALSE_FALSE
figX.1a <- plots1F$plot_type4_prevalence_ttype1_spec_FALSE_FALSE
figX.1b <- plots2F$plot_type4_prevalence_ttype2_spec_FALSE_FALSE
figX.2a <- plots1F$plot_type4_RiskA_ttype1_spec_FALSE_FALSE
figX.2b <- plots2F$plot_type4_RiskA_ttype2_spec_FALSE_FALSE

figX.1 <- (figX.1a + labs(title = "A") + ggtitle("Curative drug\n ") + plot_spacer() +
             figX.1b + labs(title = "B") + ggtitle("Long-lasting drug\n ")) +
  plot_layout(widths = c(1, 0.1, 1))
#figX.1

figX.2 <- (figX.2a + plot_spacer() + figX.2b) +
  plot_layout(widths = c(1, 0.1, 1))
#figX.2

figX <- figX.1 / figX.2 + plot_layout(guides = 'collect', axes = "collect") & 
  #plot_annotation(tag_levels = 'A') & 
  #theme_bw(base_size = 16) & 
  theme(legend.position = "bottom") 
figX
ggsave("output/ms_figs/pw3.pdf", figX, width = 11, height = 6.5) # best size

# ##############################################################################
# Figure 4 shows the impact of insecticide resistance
figX.1 <- plots1F$plot_type4vert_RiskA_ttype1_spec_FALSE_FALSE + ggtitle(my_title("curative")) 
figX.2 <- plots2F$plot_type4vert_RiskA_ttype2_spec_FALSE_FALSE + ggtitle(my_title("longlasting")) +
  labs(y = NULL) +  # remove axis label
  theme(
    axis.text.y = element_blank(),     # remove tick labels
    axis.ticks.y = element_blank()     # remove tick marks
  )
figX.2
figX.3 <- plots3F$plot_type4vert_RiskA_ttype3_spec_FALSE_FALSE + ggtitle(my_title("ongoing")) +
  labs(y = NULL) +  # remove axis label
  theme(
    axis.text.y = element_blank(),     # remove tick labels
    axis.ticks.y = element_blank()     # remove tick marks
  )

figX <- (figX.1 | figX.2 | plot_spacer() | figX.3) + plot_layout(guides = 'collect', axes = "collect", widths = c(1, 1, 0.1, 1)) & 
  #plot_annotation(tag_levels = 'A') & 
  #theme_bw(base_size = 16) & 
  theme(legend.position = "bottom") 
ggsave("output/ms_figs/pw4.pdf", figX, width = 8.5, height = 8.5) # best size

# ##############################################################################
# Figure 3 shows the impact of insecticide resistance
figX.1 <- plots1F$plot_type5_RiskA_ttype1_spec_FALSE_FALSE + ggtitle(my_title("curative", split_across_lines = "other")) + guides(shape = "none")
figX.2 <- plots2F$plot_type5_RiskA_ttype2_spec_FALSE_FALSE + ggtitle(my_title("longlasting", split_across_lines = "other")) + guides(shape = "none")
figX.3 <- plots3F$plot_type5_RiskA_ttype3_spec_FALSE_FALSE + ggtitle(my_title("ongoing", split_across_lines = "other")) + guides(shape = "none")


figA <- ( (figX.1) / figX.2 ) + plot_layout(guides = 'collect') & 
  theme(legend.position = "right") 
figB <- figX.3 & 
  theme(legend.position = "right") 
figA
figB
fig4 <- figA / figB 
ggsave("output/ms_figs/use_as_fig4A.pdf", figA, width = 8, height = 6.5) # best size
ggsave("output/ms_figs/use_as_fig4B.pdf", figB, width = 8, height = 3.5) # best size
ggsave("output/ms_figs/use_as_fig4.pdf", fig4, width = 8, height = 10) # best size

# ##############################################################################
# Figure 5 

figX.1 <- plots1F$plot_type4_Rres_final_ttype1_spec_FALSE_FALSE + 
  ggtitle(my_title("curative", split_across_lines = "other")) + #guides(shape = "none") + 
  coord_cartesian(ylim = c(0, 5)) +
  geom_segment(x = 0.0, y = 1.0, xend = 1.0, yend = 1.0, colour = "red", linewidth = 0.5)
figX.2 <- plots2F$plot_type4_Rres_final_ttype2_spec_FALSE_FALSE + 
  ggtitle(my_title("longlasting", split_across_lines = "other")) + #guides(shape = "none") + 
  coord_cartesian(ylim = c(0, 5)) +
  geom_segment(x = 0.0, y = 1.0, xend = 1.0, yend = 1.0, colour = "red", linewidth = 0.5)
figX.3 <- plots3F$plot_type4_Rres_final_ttype3_spec_FALSE_FALSE + 
  ggtitle(my_title("ongoing", split_across_lines = "other")) + #guides(shape = "none") + 
  coord_cartesian(ylim = c(0, 5)) +
  geom_segment(x = 0.0, y = 1.0, xend = 12.0, yend = 1.0, colour = "red", linewidth = 0.5)


figA <- ( (figX.1) / figX.2 ) + plot_layout(guides = 'collect') & 
  theme(legend.position = "right") 
figB <- figX.3 & 
  theme(legend.position = "right") 
figA
figB
ggsave("output/ms_figs/use_as_fig5A.pdf", figA, width = 8, height = 6.5) # best size
ggsave("output/ms_figs/use_as_fig5B.pdf", figB, width = 8, height = 3.5) # best size

# ##############################################################################
# ##############################################################################
# Figure 5 

figX.1 <- plots1F$plot_type4v2_Rres_final_ttype1_spec_FALSE_FALSE + 
  ggtitle(my_title("curative", split_across_lines = "other")) + #guides(shape = "none") + 
coord_cartesian(ylim = c(0, 10)) #+
  #geom_segment(x = 0.0, y = 1.0, xend = 1.0, yend = 1.0, colour = "red", linewidth = 0.5)
figX.2 <- plots2F$plot_type4v2_Rres_final_ttype2_spec_FALSE_FALSE + 
  ggtitle(my_title("longlasting", split_across_lines = "other")) + #guides(shape = "none") + 
coord_cartesian(ylim = c(0, 10)) #+
  #geom_segment(x = 0.0, y = 1.0, xend = 1.0, yend = 1.0, colour = "red", linewidth = 0.5)
figX.3 <- plots3F$plot_type4v2_Rres_final_ttype3_spec_FALSE_FALSE + 
  ggtitle(my_title("ongoing", split_across_lines = "other")) + #guides(shape = "none") + 
coord_cartesian(ylim = c(0, 10)) #+
  #geom_segment(x = 0.0, y = 1.0, xend = 12.0, yend = 1.0, colour = "red", linewidth = 0.5)


figA <- ( (figX.1) / figX.2 ) + plot_layout(guides = 'collect') & 
  theme(legend.position = "right") 
figB <- figX.3 & 
  theme(legend.position = "right") 
figA
figB
ggsave("output/ms_figs/use_as_fig5Av2.pdf", figA, width = 8, height = 6.5) # best size
ggsave("output/ms_figs/use_as_fig5Bv2.pdf", figB, width = 8, height = 3.5) # best size

# ##############################################################################