# my_theme <- function () { 
#   theme_grey(base_size=16) +
#     theme(plot.title = element_text(hjust = 0.5),
#           plot.subtitle = element_text(hjust = 0.5),
#           plot.caption = element_text(),
#           axis.text.x = element_text(angle=45, hjust=1, size = 12),
#           axis.text.y = element_text( size = 12),
#           panel.grid.major = element_blank(), 
#           panel.grid.minor = element_blank())
# }

# my_theme2 <- function () { 
#   theme_grey(base_size=12) +
#     theme(plot.title = element_text(hjust = 0.5),
#           plot.subtitle = element_text(hjust = 0.5),
#           plot.caption = element_text(),
#           axis.text.x = element_text(angle=45, hjust=1, size = 14),
#           panel.grid.major = element_blank(), 
#           panel.grid.minor = element_blank())
# }

########################################################
create_K_plots <- function(df, ymax, label, this_K){
    
    plot2a <- df %>% filter(W == 0, K == this_K) %>% ggplot(aes(x = treat_prop, y = this_y, colour = prop_insect, group = interaction(prop_insect, K))) + 
      geom_point(aes(shape = K), size = 3) + ylim(0,ymax) + xlab("Proportion treated") + xlab("\n") + ylab("") +
      geom_line() + my_theme2()
    plot2a
    
    plot2b <- df %>% filter(W == 100, K == this_K) %>% ggplot(aes(x = treat_prop, y = this_y, colour = prop_insect, group = interaction(prop_insect, K))) + 
      geom_point(aes(shape = K), size = 3) + ylim(0,ymax) + xlab("Proportion treated") +xlab("\n") + ylab("") +
      geom_line() + my_theme2()
    plot2b
    
    plot2c <- df %>% filter(W == 250, K == this_K) %>% ggplot(aes(x = treat_prop, y = this_y, colour = prop_insect, group = interaction(prop_insect, K))) + 
      geom_point(aes(shape = K), size = 3) + ylim(0,ymax) + xlab("Proportion treated") +xlab("\n") + ylab("") +
      geom_line() + my_theme2()
    plot2c
    
    legend <- get_legend(plot2a + theme(legend.box.margin = margin(0, 0, 0, 12), legend.title=element_text(size=10)) )
    
    print(plot_grid(plot2a + theme(legend.position="none") +ylab(label), 
                    plot2b + theme(legend.position="none") + xlab("Proportion\nTreated"), 
                    plot2c + theme(legend.position="none"), legend, ncol = 4, rel_widths = c(1,1,1,0.5)))
    # pdf_name = paste0(folder_name, "extra_plot2_K_", this_K, ".pdf")
    # pdf(file = pdf_name)
    # print(plot_grid(plot2a + theme(legend.position="none") +ylab(label), 
    #                 plot2b + theme(legend.position="none") + xlab("Proportion\nTreated"), 
    #                 plot2c + theme(legend.position="none"), legend, ncol = 4, rel_widths = c(1,1,1,0.5)))
    # dev.off()
}
########################################################
df <- test1 %>% filter(prop_insecticide < 0.5, prop_insecticide != 0.025, K == 6000) %>% 
  mutate(W = as.factor(W_st), prop_insect = as.factor(prop_insecticide), K = as.factor(K)) %>% 
  filter(dose ==1)

df$this_y <- df$Incidence
ymax2 <- max(df$Incidence) + 20
for (this_K in unique(df$K)) {
  plot <- create_K_plots(df, ymax2, "Incidence", this_K)
  pdf_name = paste0(folder_name, "extra_plot2_K_", this_K, ".pdf")
  pdf(file = pdf_name, height = pdf_height, width = pdf_width)
  create_K_plots(df, ymax2, "Incidence", this_K)
  dev.off()
}

df$this_y <- df$prevalence
ymax2 <- 1
for (this_K in unique(df$K)) {
  plot <- create_K_plots(df, ymax2, "Prevalence", this_K)
  pdf_name = paste0(folder_name, "extra_plot3_K_", this_K, ".pdf")
  pdf(file = pdf_name, height = pdf_height, width = pdf_width)
  create_K_plots(df, ymax2, "Prevalence", this_K)
  dev.off()
}

df$this_y <- df$No_trt_cat
ymax2 <- max(df$No_trt_cat) + 20
for (this_K in unique(df$K)) {
  plot <- create_K_plots(df, ymax2, "Number treated cattle", this_K)
  pdf_name = paste0(folder_name, "extra_plot4_K_", this_K, ".pdf")
  pdf(file = pdf_name, height = pdf_height, width = pdf_width)
  create_K_plots(df, ymax2, "Number treated cattle", this_K)
  dev.off()
}

df$this_y <- df$RiskE
ymax2 <- 8
for (this_K in unique(df$K)) {
  plot <- create_K_plots(df, ymax2, "Risk of emergence and spread", this_K)
  pdf_name = paste0(folder_name, "extra_plot5_K_", this_K, ".pdf")
  pdf(file = pdf_name, height = pdf_height, width = pdf_width)
  create_K_plots(df, ymax2, "Risk of emergence and spread", this_K)
  dev.off()
}


# for (this_K in unique(test1$K)) {
#   
#   plot2a <- df %>% filter(W == 0, K == this_K) %>% ggplot(aes(x = treat_prop, y = Incidence, colour = prop_insect, group = interaction(prop_insect, K))) + 
#     geom_point(aes(shape = K), size = 3) + ylim(0,ymax2) + xlab("Proportion treated") + xlab("\n") + ylab("") +
#     geom_line() + my_theme2()
#   plot2a
#   
#   plot2b <- df %>% filter(W == 100, K == this_K) %>% ggplot(aes(x = treat_prop, y = Incidence, colour = prop_insect, group = interaction(prop_insect, K))) + 
#     geom_point(aes(shape = K), size = 3) + ylim(0,ymax2) + xlab("Proportion treated") +xlab("\n") + ylab("") +
#     geom_line() + my_theme2()
#   plot2b
#   
#   plot2c <- df %>% filter(W == 250, K == this_K) %>% ggplot(aes(x = treat_prop, y = Incidence, colour = prop_insect, group = interaction(prop_insect, K))) + 
#     geom_point(aes(shape = K), size = 3) + ylim(0,ymax2) + xlab("Proportion treated") +xlab("\n") + ylab("") +
#     geom_line() + my_theme2()
#   plot2c
#   
#   legend <- get_legend(plot2a + theme(legend.box.margin = margin(0, 0, 0, 12), legend.title=element_text(size=10)) )
#   
#   print(plot_grid(plot2a + theme(legend.position="none") +ylab("Incidence"), 
#                   plot2b + theme(legend.position="none") + xlab("Proportion\nTreated"), 
#                   plot2c + theme(legend.position="none"), legend, ncol = 4, rel_widths = c(1,1,1,0.5)))
#   pdf_name = paste0(folder_name, "extra_plot2_K_", this_K, ".pdf")
#   pdf(file = pdf_name)
#   print(plot_grid(plot2a + theme(legend.position="none") +ylab("Incidence"), 
#                   plot2b + theme(legend.position="none") + xlab("Proportion\nTreated"), 
#                   plot2c + theme(legend.position="none"), legend, ncol = 4, rel_widths = c(1,1,1,0.5)))
#   dev.off()
# }
# 
# for (this_K in unique(test1$K)) {
#   
#   plot3a <- df %>% filter(W == 0, K == this_K) %>% ggplot(aes(x = treat_prop, y = prevalence, colour = prop_insect, group = interaction(prop_insect, K))) + 
#     geom_point(aes(shape = K), size = 3) + ylim(0, 1) + xlab("Proportion treated") + ylab("Prevalence") +
#     geom_line() + my_theme2()
#   plot3a
#   
#   plot3b <- df %>% filter(W == 100, K == this_K) %>% ggplot(aes(x = treat_prop, y = prevalence, colour = prop_insect, group = interaction(prop_insect, K))) + 
#     geom_point(aes(shape = K), size = 3) + ylim(0, 1) + xlab("Proportion treated") + ylab("Prevalence") +
#     geom_line() + my_theme2()
#   plot3b
#   
#   plot3c <- df %>% filter(W == 250, K == this_K) %>% ggplot(aes(x = treat_prop, y = prevalence, colour = prop_insect, group = interaction(prop_insect, K))) + 
#     geom_point(aes(shape = K), size = 3) + ylim(0, 1) + xlab("Proportion treated") + ylab("Prevalence") +
#     geom_line() + my_theme2()
#   plot3c
#   
#   print(plot_grid(plot3a, plot3b, plot3c))
#   pdf_name = paste0(folder_name, "extra_plot3_K_", this_K, ".pdf")
#   pdf(file = pdf_name)
#   print(plot_grid(plot3a, plot3b, plot3c))
#   dev.off()
# }
# 
# ymax2 <- 8 #max(df$RiskE) + 2
# 
# for (this_K in unique(test1$K)) {
#   
#   plot4a <- df %>% filter(W == 0, K == this_K) %>% ggplot(aes(x = treat_prop, y = RiskE, colour = prop_insect, group = interaction(prop_insect, K))) + 
#     geom_point(aes(shape = K), size = 3) + ylim(0, ymax2) + xlab("Proportion treated") + ylab("Risk of emergence and spread") +
#     geom_line() + my_theme2()
#   plot4a
#   
#   plot4b <- df %>% filter(W == 100, K == this_K) %>% ggplot(aes(x = treat_prop, y = RiskE, colour = prop_insect, group = interaction(prop_insect, K))) + 
#     geom_point(aes(shape = K), size = 3) + ylim(0, ymax2) + xlab("Proportion treated") + ylab("Risk of emergence and spread") +
#     geom_line() + my_theme2()
#   plot4b
#   
#   plot4c <- df %>% filter(W == 250, K == this_K) %>% ggplot(aes(x = treat_prop, y = RiskE, colour = prop_insect, group = interaction(prop_insect, K))) + 
#     geom_point(aes(shape = K), size = 3) + ylim(0, ymax2) + xlab("Proportion treated") + ylab("Risk of emergence and spread") +
#     geom_line() + my_theme2()
#   plot4c
#   
#   print(plot_grid(plot4a, plot4b, plot4c))
#   pdf_name = paste0(folder_name, "extra_plot4_K_", this_K, ".pdf")
#   pdf(file = pdf_name)
#   print(plot_grid(plot4a, plot4b, plot4c))
#   dev.off()
# }


####################################################
#Generate plots with insecticide treatment on x axis

###########
plot_versus_insecticide <- function(df, this_K, max.ins, y_label){
  this_W_st = 0
  plot4a <- df3 %>% filter(W_st == this_W_st & prop_insecticide < 0.51) %>%
    ggplot() +
    theme(axis.text.x = element_text(angle=45, hjust=1, size = 12),
          axis.text.y = element_text( size = 12)) + xlab("\n") + ylab("") +
    geom_point(aes(y = this_y, x = prop_insecticide, colour = treat_prop),size = 2) +
    geom_line(aes(y = this_y, x = prop_insecticide, colour = treat_prop), size = 1.1) + 
    ggtitle(paste0("Wildlife = ", this_W_st)) + my_theme2() +
    ylim(0, max.ins)
  plot4a
  legend4 <- get_legend(plot4a + theme(legend.box.margin = margin(0, 0, 0, 12), 
                        legend.title=element_text(size=12)) )
  
  this_W_st = 100
  plot4b <- df3 %>% filter(W_st == this_W_st & prop_insecticide < 0.51) %>%
    ggplot()  +
    theme(axis.text.x = element_text(angle=45, hjust=1, size = 12),
          axis.text.y = element_text( size = 12)) +  xlab("\n") + ylab("") +
    geom_point(aes(y = this_y, x = prop_insecticide, colour = treat_prop), size = 2) +
    geom_line(aes(y = this_y, x = prop_insecticide, colour = treat_prop), size = 1.1) + 
    ggtitle(paste0("Wildlife = ", this_W_st)) + my_theme2()+
    ylim(0, max.ins)
  plot4b
  
  this_W_st = 250
  plot4c <- df3 %>% filter(W_st == this_W_st & prop_insecticide < 0.51) %>%
    ggplot()  +
    theme(axis.text.x = element_text(angle=45, hjust=1, size = 12),
          axis.text.y = element_text( size = 12)) + xlab("\n") + ylab("") +
    geom_point(aes(y = this_y, x = prop_insecticide, colour = treat_prop), size = 2) +
    geom_line(aes(y = this_y, x = prop_insecticide, colour = treat_prop), size = 1.1) + 
    ggtitle(paste0("Wildlife = ", this_W_st)) + my_theme2()+
    ylim(0, max.ins)
  plot4c
  
  print(plot_grid(plot4a + theme(legend.position="none") + ylab(y_label), 
                  plot4b + theme(legend.position="none") + xlab("Proportion\nInsecticide"), 
                  plot4c + theme(legend.position="none"),
                  legend4, ncol=4) ) #, rel_widths = c(1,1,1,0.5)))
  #pdf_name = paste0(folder_name, "extra_plot4_K_", this_K, ".pdf")
  #pdf(file = pdf_name)
  #print(plot_grid(plot4a + theme(legend.position="none"), 
                 # plot4b + theme(legend.position="none"), 
                 # plot4c + theme(legend.position="none"),
                 # legend4, ncol=4, rel_widths = c(1,1,1,0.5)))
  #dev.off()
}
###########
this_K = 6000
df3 <- test %>% mutate(treat_prop = pmax(treat_prop_q, treat_prop_p)) %>% 
  filter(treat_prop < 0.93, K == this_K, dose == 1) %>% 
  mutate(treat_prop = as.factor(treat_prop))

df3 <- df3 %>% mutate(this_y = Incidence)
my_ylab = "Incidence"
my_xlab = "Prop insecticide"
y_max = 300 #1.1*max(df3$this_y)
plot_versus_insecticide_Incidence <- plot_versus_insecticide(df3, this_K, y_max, my_ylab)
pdf_name = paste0(folder_name, "extra_plot6_insecticide_Incidence_K_", this_K, ".pdf")
pdf(file = pdf_name, height = pdf_height, width = pdf_width)
plot_versus_insecticide(df3, this_K, y_max, my_ylab)
dev.off()

df3 <- df3 %>% mutate(this_y = No_trt_cat)
my_ylab = "Num treated cattle"
my_xlab = "Prop insecticide"
y_max = 300 #1.1*max(df3$this_y)
plot_versus_insecticide_No_trt_cat <- plot_versus_insecticide(df3, this_K, y_max, my_ylab)
pdf_name = paste0(folder_name, "extra_plot6_insecticide_No_trt_cat_K_", this_K, ".pdf")
pdf(file = pdf_name, height = pdf_height, width = pdf_width)
plot_versus_insecticide(df3, this_K, y_max, my_ylab)
dev.off()

df3 <- df3 %>% mutate(this_y = prevalence)
my_ylab = "Prevalence"
my_xlab = "Prop insecticide"
y_max = 1 #1.1*max(df3$this_y)
plot_versus_insecticide_Prevalence <- plot_versus_insecticide(df3, this_K, y_max, my_ylab)
pdf_name = paste0(folder_name, "extra_plot6_insecticide_prevalence_", this_K, ".pdf")
pdf(file = pdf_name, height = pdf_height, width = pdf_width)
plot_versus_insecticide(df3, this_K, y_max, my_ylab)
dev.off()

df3 <- df3 %>% mutate(this_y = RiskE)
my_ylab = "Relative risk of emergence and spread"
my_xlab = "Prop insecticide"
y_max = 8 #1.1*max(df3$this_y)
plot_versus_insecticide_RiskE <- plot_versus_insecticide(df3, this_K, y_max, my_ylab)
pdf_name = paste0(folder_name, "extra_plot6_insecticide_riskE_", this_K, ".pdf")
pdf(file = pdf_name, height = pdf_height, width = pdf_width)
plot_versus_insecticide(df3, this_K, y_max,my_ylab)
dev.off()
