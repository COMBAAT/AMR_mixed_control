# Generating plots for the manuscript
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
ggsave("output/ms_figs/figS1_large.pdf", figX, width = 11, height = 13, dpi = 150) 
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
ggsave("output/ms_figs/figS2.pdf", figX, width = 11, height = 13) # best size

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
ggsave("output/ms_figs/fig2.pdf", figX, width = 11, height = 6.5) # best size
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
ggsave("output/ms_figs/fig2_options.pdf", figX, width = 11, height = 6.5) # best size
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
ggsave("output/ms_figs/figS1_small.pdf", figX, width = 11, height = 6.5) # best size

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
ggsave("output/ms_figs/fig3.pdf", figX, width = 11, height = 6.5) # best size