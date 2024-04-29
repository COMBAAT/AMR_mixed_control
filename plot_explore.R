##                                                                            ##
##                                                                            ##
##                    Explore Plot Functions // VERSION 2                     ##
##                                                                            ##
##                                                                            ##

## Required packages & files ----- 

library(ggplot2)
library(gghighlight)
library(cowplot)
library(dplyr)

source("funcs/plot_functions.R")
load("output/test_P.Rda")

defaultW <- getOption("warn") 
options(warn = -1) 

## Load data and prep plot inputs -----

#load("output/test.Rda")
test1 <- test %>% signif(4)
p.ins <- unique(test1$prop.insecticide)
this_prop <- 0.05
ins2.dat <- test1 %>% filter(prop.insecticide == this_prop) 
this_prop <- 0.0
ins1.dat <- test1 %>% filter(prop.insecticide == this_prop) 
ins1.dat <- ins1.dat %>% filter(dose == 0.7)



plotlims.min <- c(0,0,0,0,0.6,0)
plotlims.max <- c(0.8,650,0.08,650,1, 65)
plotlims <- cbind(plotlims.min, plotlims.max)
names <- c("prevalence", "Incidence", "Risk", "No_trt_cat", "Prob_onward_tran", "R0_sen")
plotlims <- cbind(names, plotlims)
fitadj <- 1


## Run plots -----

## R0 sensitive (Y), number of wildlife (X), plot by treatment proportion
plot_R0_Sen_dd(ins1.dat, c(0, 0.6,0.95), fitadj = fitadj)


plot_res_v_sen_dd(ins1.dat, c(0,100,250), fitadj = fitadj)

## y options 
## R0_sen  R0_res R_eq_sen R_eq_res
## Prob_onward_tran Risk prevalence Incidence No_trt_cat
this_var <- "R0_sen"
y_label <- "R0 sensitive"
prev_threshold <- 1
axislims <- plotlims[which (plotlims[,1]==this_var),]
ymin <- as.numeric(axislims["plotlims.min"])
ymax <- as.numeric(axislims["plotlims.max"])
label <- "FALSE"
plot_PT.Y(ins1.dat, Wn = c(0,100,250), fitadj = fitadj, my_theme, my_par, this_var, y_label, label = label, prev_threshold, ymin, ymax)


this_var <- "prevalence"
y_label <- "Prevalence"
prev_threshold <- 1
axislims <- plotlims[which (plotlims[,1]==this_var),]
ymin <- as.numeric(axislims["plotlims.min"])
ymax <- as.numeric(1)
label <- "FALSE"
plot_PT.Y(ins1.dat, Wn = c(0,100,250), fitadj = fitadj, my_theme, my_par, this_var, y_label, prev_threshold, ymin, ymax, label = label)


this_var <- "Incidence"
y_label <- this_var
prev_threshold <- 1
axislims <- plotlims[which (plotlims[,1]==this_var),]
ymin <- as.numeric(axislims["plotlims.min"])
ymax <- as.numeric(axislims["plotlims.max"])
label <- "FALSE"
plot_PT.Y(ins1.dat, Wn = c(0,100,250), fitadj = fitadj, my_theme, my_par, this_var, y_label, prev_threshold, ymin, ymax, label = label)


this_var <- "No_trt_cat"
y_label <- "Number of Treated Cattle"
prev_threshold <- 1
axislims <- plotlims[which (plotlims[,1]==this_var),]
ymin <- as.numeric(axislims["plotlims.min"])
ymax <- as.numeric(axislims["plotlims.max"])
label <- "FALSE"
plot_PT.Y(ins1.dat, Wn = c(0,100,250), fitadj = fitadj, my_theme, my_par, this_var, y_label, prev_threshold, ymin, ymax, label = label)



this_var <- "Prob_onward_tran"
y_label <- "Probability of Onward Transmission"
prev_threshold <- 1
axislims <- plotlims[which (plotlims[,1]==this_var),]
ymin <- as.numeric(axislims["plotlims.min"])
ymax <- as.numeric(axislims["plotlims.max"])
label <- "FALSE"
plot_PT.Y(ins1.dat, Wn = c(0,100,250), fitadj = fitadj, my_theme, my_par, this_var, y_label, prev_threshold, ymin, ymax, label = label)



this_var <- "Risk"
y_label <- "Relative risk of AMR emergence"
prev_threshold <- 1
axislims <- plotlims[which (plotlims[,1]==this_var),]
ymin <- as.numeric(axislims["plotlims.min"])
ymax <- 1.1*max(ins1.dat$Risk) #as.numeric(axislims["plotlims.max"])
label <- "FALSE"
#plot_PT.Y(ins1.dat, Wn = c(0,100,250), fitadj = fitadj, my_theme, my_par, this_var, y_label, prev_threshold, ymin, ymax, label = label, this_prop = this_prop)
plot_PT.Y(ins1.dat, Wn = c(0,100,250), fitadj = fitadj, my_theme, my_par, this_var, y_label, prev_threshold, ymin, ymax, label = label)
## Grid Plot -----

#Extra_plot
#source("Extra_plots.R")

##
## NEW MULTIPLOT //
## 
## 
##


K.10000 <- test1 %>% filter(K == 10000) 
this_var <- "Risk"
y_label <- "Relative risk of AMR emergence"
threshold <- "prevalence"
prev_threshold <- 1
inc_threshold <- max(K.10000$Incidence)
axislims <- plotlims[which (plotlims[,1]==this_var),]
ymin <- as.numeric(axislims["plotlims.min"])
ymax <- 1.1*max(ins1.dat$Risk)*0.0001 #as.numeric(axislims["plotlims.max"])
col.var = "prop.insecticide"
shape.var = "W_st"
plot_multiplot.insecticide(K.10000, Wn = c(0,100,250), p.ins,
                           col.var, shape.var,
                           fitadj = fitadj, my_theme, this_var, y_label, threshold, prev_threshold, inc_threshold, ymin, ymax , label = label)


K.10000 <- test1 %>% filter(K == 10000) 
this_var <- "Risk"
y_label <- "Relative risk of AMR emergence"
prev_threshold <- 0.1
inc_threshold <- max(K.10000$Incidence)
axislims <- plotlims[which (plotlims[,1]==this_var),]
ymin <- as.numeric(axislims["plotlims.min"])
ymax <- 1.1*max(ins1.dat$Risk)*0.0001 #as.numeric(axislims["plotlims.max"])
col.var = "prop.insecticide"
shape.var = "W_st"
plot_multiplot.insecticide(K.10000, Wn = c(0,100,250), p.ins,
                           col.var, shape.var,
                           fitadj = fitadj, my_theme, this_var, y_label, threshold, prev_threshold, inc_threshold, ymin, ymax , label = label)




K.10000 <- test1 %>% filter(K == 10000) 
this_var <- "Risk"
y_label <- "Relative risk of AMR emergence"
prev_threshold <- 0.05
inc_threshold <- max(K.10000$Incidence)
axislims <- plotlims[which (plotlims[,1]==this_var),]
ymin <- as.numeric(axislims["plotlims.min"])
ymax <- 1.1*max(ins1.dat$Risk)*0.0001 #as.numeric(axislims["plotlims.max"])
col.var = "prop.insecticide"
shape.var = "W_st"
plot_multiplot.insecticide(K.10000, Wn = c(0,100,250), p.ins,
                           col.var, shape.var,
                           fitadj = fitadj, my_theme, this_var, y_label, threshold, prev_threshold, inc_threshold, ymin, ymax , label = label)




K.10000 <- test1 %>% filter(K == 10000) 
this_var <- "Risk"
y_label <- "Relative risk of AMR emergence"
threshold <- "Incidence"
prev_threshold <- 1
inc_threshold <- max(K.10000$Incidence)
axislims <- plotlims[which (plotlims[,1]==this_var),]
ymin <- as.numeric(axislims["plotlims.min"])
ymax <- 1.1*max(ins1.dat$Risk)*0.0001 #as.numeric(axislims["plotlims.max"])
col.var = "prop.insecticide"
shape.var = "W_st"
plot_multiplot.insecticide(K.10000, Wn = c(0,100,250), p.ins,
                           col.var, shape.var,
                           fitadj = fitadj, my_theme, this_var, y_label, threshold, prev_threshold, inc_threshold, ymin, ymax , label = label)


K.10000 <- test1 %>% filter(K == 10000) 
this_var <- "Risk"
y_label <- "Relative risk of AMR emergence"
prev_threshold <- 0.1
inc_threshold <- 100#max(K.10000$Incidence)
axislims <- plotlims[which (plotlims[,1]==this_var),]
ymin <- as.numeric(axislims["plotlims.min"])
ymax <- 1.1*max(ins1.dat$Risk)*0.0001 #as.numeric(axislims["plotlims.max"])
col.var = "prop.insecticide"
shape.var = "W_st"
plot_multiplot.insecticide(K.10000, Wn = c(0,100,250), p.ins,
                           col.var, shape.var,
                           fitadj = fitadj, my_theme, this_var, y_label, threshold, prev_threshold, inc_threshold, ymin, ymax , label = label)




K.10000 <- test1 %>% filter(K == 10000) 
this_var <- "Risk"
y_label <- "Relative risk of AMR emergence"
prev_threshold <- 0.05
inc_threshold <- 10#max(K.10000$Incidence)
axislims <- plotlims[which (plotlims[,1]==this_var),]
ymin <- as.numeric(axislims["plotlims.min"])
ymax <- 1.1*max(ins1.dat$Risk)*0.0001 #as.numeric(axislims["plotlims.max"])
col.var = "prop.insecticide"
shape.var = "W_st"
plot_multiplot.insecticide(K.10000, Wn = c(0,100,250), p.ins,
                           col.var, shape.var,
                           fitadj = fitadj, my_theme, this_var, y_label, threshold, prev_threshold, inc_threshold, ymin, ymax , label = label)





pdf(file = "My Plot.pdf",   # The directory you want to save the file in
    width = 12, # The width of the plot in inches
    height = 7) # The height of the plot in inches

# Step 2: Create the plot with R code
plot_R0_Sen_dd(ins1.dat, c(0,0.6,0.95), fitadj = fitadj)

# Step 3: Run dev.off() to create the file!
dev.off()
options(warn = defaultW)

source("Extra_plots.R")
