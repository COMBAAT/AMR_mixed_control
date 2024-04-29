plot_PT.Y.dose <- function(df2, Wn, fitadj, my_theme, my_par, this_var, y_label, prev_threshold, ymin, ymax, label, this_prop){
  
  ggplotColours <- function(n = 6, h = c(0, 360) + 15){
    if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
    hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
  }
  my_cols_vec <- ggplotColours(n=3)
  my_cols <- my_cols_vec[1:3]
  
  df2 <- df2 %>% filter(fit.adj == fitadj)
  names(df2)[names(df2) == this_var] <- "y"
  
  plot_this <- df2 %>% filter(W_st %in% Wn) %>% mutate(W_st = as.factor(W_st), K = as.factor(K))
  
  p <- ggplot(plot_this, aes(x = dose, y = y, colour = W_st, group = interaction(W_st, K)) )  + my_theme() +
    geom_point(aes(shape = K), size = 3) + geom_line() +
    xlab("\n") + ylab("") +
    guides(col = guide_legend("Wildlife"), shape = guide_legend("Carrying \nCapacity (K)"))
  
  plots <- list()
  for (i in 1:3){
    plots[[i]] <- plot_this %>% filter(W_st == Wn[i]) %>% 
      ggplot(aes(x = dose, y = y, group = interaction(W_st, K)) ) + my_theme() +
      geom_point(aes(shape = K), size = 3, colour = my_cols[i] ) + geom_line(colour = my_cols[i]  ) + ylim(ymin, ymax) +xlab("\n") + ylab("") +
      if (this_var != "prevalence"){gghighlight(prevalence < prev_threshold)}
  }
  
  legend <- get_legend(p + theme(legend.box.margin = margin(0, 0, 0, 12))) 
  
  if(label == "TRUE"){plot_grid(plots[[1]] + theme(legend.position="none") + ylab(y_label) +
                                  draw_figure_label(label = "Figure 1", fontface = "bold", position = "top.left"), 
                                plots[[2]] + theme(legend.position="none") + xlab("Dose\n"), 
                                plots[[3]] + theme(legend.position="none"), legend, ncol = 4)}else
                                {plot_grid(plots[[1]] + theme(legend.position="none") + ylab(y_label), 
                                           plots[[2]] + theme(legend.position="none") + xlab("Dose\n"), 
                                           plots[[3]] + theme(legend.position="none"), legend, ncol = 4)}
  
}


treat.prop.vec


test1 <- test %>% signif(4)
p.ins <- unique(test1$prop.insecticide)
this_prop <- 0.0
ins1.dat <- test1 %>% filter(prop.insecticide == this_prop & treat_prop == 0.8) 





this_var <- "Risk"
y_label <- "Relative risk of AMR emergence"
prev_threshold <- 1
axislims <- plotlims[which (plotlims[,1]==this_var),]
ymin <- as.numeric(axislims["plotlims.min"])
ymax <- 1.1*max(ins1.dat$Risk) #as.numeric(axislims["plotlims.max"])
label <- "FALSE"
#plot_PT.Y(ins1.dat, Wn = c(0,100,250), fitadj = fitadj, my_theme, my_par, this_var, y_label, prev_threshold, ymin, ymax, label = label, this_prop = this_prop)
pdf(file="output/plots_fast/dose_plot_tp0.8.pdf")
plot_PT.Y.dose(ins1.dat, Wn = c(0,100,250), fitadj = fitadj, my_theme, my_par, this_var, y_label, prev_threshold, ymin, ymax, label = label)
dev.off()
## Grid Plot -----

