library(ggplot2)
library(gghighlight)
library(dplyr)
library(patchwork)
library(stringr)
library(tidyr)


# Source files and function
source("funcs/plot_helper.R")

quick_plot <- function(df){
  
  par(mfrow = c(2, 2))
  plot(df$CS ~ df$time, type = 'l', ylim = c(0, 55), 
       lwd = 3, col = 'blue', main = "Cattle (no Prophylaxis)", 
       xlab = "Time", ylab = "Number")
  lines(df$CEs ~ df$time, lwd = 3, col = 'orange') # Exposed
  lines(df$CEr ~ df$time, lwd = 3, col = 'orange', lty = 2) # Exposed
  lines(df$CIs ~ df$time, lwd = 3, col = 'red') # Infected
  lines(df$CIr ~ df$time, lwd = 3, col = 'red', lty = 2) # Infected
  lines(df$CTs ~ df$time, lwd = 3, col = 'green') # Treated
  lines(df$CTr ~ df$time, lwd = 3, col = 'green', lty = 2) # Treated
  lines(( df$CEs + df$CEr + df$CIs + df$CIr + df$CTs + df$CTr + df$CS) ~df$time,lty = 2)
  lines(( df$CEs + df$CEr + df$CIs + df$CIr + df$CTs + df$CTr + df$CS +
            df$PEs + df$PEr + df$PIs + df$PIr + df$PTs + df$PTr + df$PS + df$PF+ df$PPr + df$PPs) ~df$time,lty = 2, col = "pink")
  #legend("topright", col = c("blue", "orange", "darkorange", "red","darkred", "green", "darkgreen", "grey"),
  #       y = c("CS", "CEs", "CEr", "CIs", "CIr","CTs", "CTr"), pch = 15)
  
  plot( df$PF ~ df$time, type = 'l', ylim = c(0, 55),col = 'purple',
        lwd = 3,main = "Cattle (with Prophylaxis)",xlab = "Time", ylab = "Number")
  lines(df$PS ~ df$time, lwd = 3, col = 'blue') # Exposed
  lines(df$PEs ~ df$time, lwd = 3, col = 'orange') # Exposed
  lines(df$PEr ~ df$time, lwd = 3, col = 'orange', lty = 2) # Exposed
  lines(df$PIs ~ df$time, lwd = 3, col = 'red') # Infected
  lines(df$PIr ~ df$time, lwd = 3, col = 'red', lty = 2) # Infected
  lines(df$PTs ~ df$time, lwd = 3, col = 'green') # Treated
  lines(df$PTr ~ df$time, lwd = 3, col = 'green', lty = 2) # Treated
  lines(df$PPs ~ df$time, lwd = 3, col = 'grey') # Treated
  lines(df$PPr ~ df$time, lwd = 3, col = 'grey', lty = 2) # Treated
  lines((df$PEs + df$PEr + df$PIs + df$PIr + df$PTs + df$PTr  + df$PS + df$PF + df$PPr + df$PPs) ~df$time, lty = 2)
  lines(( df$CEs + df$CEr + df$CIs + df$CIr + df$CTs + df$CTr + df$CS +
            df$PEs + df$PEr + df$PIs + df$PIr + df$PTs + df$PTr + df$PS+ df$PF+ df$PPr + df$PPs) ~df$time,lty = 2, col = "pink")
  #legend("topright", col = c("purple","blue", "orange", "darkorange", "red","darkred", "green", "darkgreen", "grey", "black", "grey"),
  #       y = c("PF","PS", "PEs", "PEr", "PIs", "PIr","PTs", "PTr", "PPs", "PPr"), pch = 15)
  
  plot(df$WS ~ df$time,type = 'l', ylim = c(0, max(df$WS+10)),col = 'blue',lwd = 3,
       main = "Wildlife", xlab = "Time", ylab = "Number")
  lines(df$WEs ~ df$time, lwd = 3, col = 'orange') # Exposed
  lines(df$WEr ~ df$time, lwd = 3, col = 'orange', lty = 2) # Exposed
  lines(df$WIs ~ df$time, lwd = 3, col = 'red') # Infected
  lines(df$WIr ~ df$time, lwd = 3, col = 'red', lty = 2) # Infected
  lines((df$WEs + df$WEr + df$WIs + df$WIr + df$WS) ~ df$time,lty = 2)
  #legend("topright", col = c("blue", "orange", "darkorange", "red","darkred"),
  #       y = c("WS", "WEs", "WEr", "WIs", "WIr"), pch = 15)
  
  plot(df$VSt ~ df$time, type = 'l', ylim = c(0, max(df$VSt)+100), col = 'blue',
       lwd = 3, main = "Vector", xlab = "Time", ylab = "Number")
  lines(df$VSf ~ df$time, lwd = 3, col = 'lightblue') # Exposed
  lines(df$VEs ~ df$time, lwd = 3, col = 'orange') # Exposed
  lines(df$VEr ~ df$time, lwd = 3, col = 'orange', lty = 2) # Exposed
  lines(df$VIs ~ df$time, lwd = 3, col = 'red') # Infected
  lines(df$VIr ~ df$time, lwd = 3, col = 'red', lty = 2) # Infected
  lines((df$VEs + df$VEr + df$VIs + df$VIr + df$VSt + df$VSf) ~
          df$time, lty = 2)
  #legend("topright", col = c("blue", "lightblue","orange", "darkorange", "red","darkred"),
  #  €   y = c("VSt","VSf", "VEs", "VEr", "VIs", "VIr"), pch = 15)
  
}



quick_plot2 <- function(df) {
  ggplot(df) +
    geom_line(aes(x = time, y = CS), colour = "blue") +
    geom_line(aes(x = time, y = CEs), colour = "orange") +
    geom_line(aes(x = time, y = CEr), colour = "orange") +
    geom_line(aes(x = time, y = CIs), colour = "red") +
    geom_line(aes(x = time, y = CIr), colour = "red") +
    geom_line(aes(x = time, y = CTs), colour = "green") +
    geom_line(aes(x = time, y = CTr), colour = "green") +
    geom_line(aes(x = time, y = Cattle_total), colour = "pink") +
    ylab("Number") +
    xlab("Time, days") +
    ggtitle("Cattle, no prophylaxis")

  this_linewidth <- 1.5
  p1 <- ggplot(df) +
    geom_line(aes(x = time, y = CS, colour = "Sus"), linewidth = this_linewidth) +
    geom_line(aes(x = time, y = CEs, colour = "Exp s"), linewidth = this_linewidth) +
    geom_line(aes(x = time, y = CEr, colour = "Exp r"), linewidth = this_linewidth) +
    geom_line(aes(x = time, y = CIs, colour = "Inf s"), linewidth = this_linewidth) +
    geom_line(aes(x = time, y = CIr, colour = "Inf r"), linewidth = this_linewidth) +
    geom_line(aes(x = time, y = CTs, colour = "Trt s"), linewidth = this_linewidth) +
    geom_line(aes(x = time, y = CTr, colour = "Trt r"), linewidth = this_linewidth) +
    geom_line(aes(x = time, y = Cattle_total), colour = "pink") +
    ylab("Number") +
    xlab("Time (days)") +
    ggtitle("Cattle, no prophylaxis") +
    my_theme() +
    scale_color_manual(" ",
      values = c(
        "Full" = "purple",
        "Partial" = "skyblue",
        "Sus" = "blue",
        "Exp s" = "orange",
        "Exp r" = "darkorange",
        "Inf s" = "red",
        "Inf r" = "darkred",
        "Trt s" = "green",
        "Trt r" = "darkgreen"
      )
    )

  p2 <- ggplot(df) +
    geom_line(aes(x = time, y = PF, colour = "Full"), linewidth = this_linewidth) +
    geom_line(aes(x = time, y = PS, colour = "Partial"), linewidth = this_linewidth) +
    geom_line(aes(x = time, y = PEs, colour = "Exp s"), linewidth = this_linewidth) +
    geom_line(aes(x = time, y = PEr, colour = "Exp r"), linewidth = this_linewidth) +
    geom_line(aes(x = time, y = PIs, colour = "Inf s"), linewidth = this_linewidth) +
    geom_line(aes(x = time, y = PIr, colour = "Inf r"), linewidth = this_linewidth) +
    geom_line(aes(x = time, y = PTs, colour = "Trt s"), linewidth = this_linewidth) +
    geom_line(aes(x = time, y = PTr, colour = "Trt r"), linewidth = this_linewidth) +
    geom_line(aes(x = time, y = Prophylactic_total), colour = "black") +
    ylab("Number") +
    xlab("Time (days)") +
    ggtitle("Cattle, with prophylaxis") +
    my_theme() +
    scale_color_manual(" ",
      values = c(
        "Full" = "purple",
        "Partial" = "skyblue",
        "Sus" = "blue",
        "Exp s" = "orange",
        "Exp r" = "darkorange",
        "Inf s" = "red",
        "Inf r" = "darkred",
        "Trt s" = "green",
        "Trt r" = "darkgreen"
      )
    )

  p3 <- ggplot(df) +
    geom_line(aes(x = time, y = WS, colour = "Sus"), linewidth = this_linewidth) +
    geom_line(aes(x = time, y = WEs, colour = "Exp s"), linewidth = this_linewidth) +
    geom_line(aes(x = time, y = WEr, colour = "Exp r"), linewidth = this_linewidth) +
    geom_line(aes(x = time, y = WIs, colour = "Inf s"), linewidth = this_linewidth) +
    geom_line(aes(x = time, y = WIr, colour = "Inf r"), linewidth = this_linewidth) +
    geom_line(aes(x = time, y = Wildlife_total), colour = "black") +
    ylab("Number") +
    xlab("Time (days)") +
    ggtitle("Wildlife") +
    my_theme() +
    scale_color_manual(" ",
      values = c(
        "Full" = "purple",
        "Partial" = "skyblue",
        "Sus" = "blue",
        "Exp s" = "orange",
        "Exp r" = "darkorange",
        "Inf s" = "red",
        "Inf r" = "darkred",
        "Trt s" = "green",
        "Trt r" = "darkgreen"
      )
    )

  p4 <- ggplot(df) +
    geom_line(aes(x = time, y = VSf, colour = "Ten"), linewidth = this_linewidth) +
    geom_line(aes(x = time, y = VSt, colour = "Sus"), linewidth = this_linewidth) +
    geom_line(aes(x = time, y = VEs, colour = "Exp s"), linewidth = this_linewidth) +
    geom_line(aes(x = time, y = VEr, colour = "Exp r"), linewidth = this_linewidth) +
    geom_line(aes(x = time, y = VIs, colour = "Inf s"), linewidth = this_linewidth) +
    geom_line(aes(x = time, y = VIr, colour = "Inf r"), linewidth = this_linewidth) +
    geom_line(aes(x = time, y = Vector_total), colour = "black") +
    ylab("Number") +
    xlab("Time (days)") +
    ggtitle("Vector") +
    my_theme() +
    scale_color_manual(" ",
      values = c(
        "Full" = "purple",
        "Partial" = "skyblue",
        "Sus" = "blue",
        "Exp s" = "orange",
        "Exp r" = "darkorange",
        "Inf s" = "red",
        "Inf r" = "darkred",
        "Trt s" = "green",
        "Trt r" = "darkgreen"
      )
    )

  p1 + p2 + p3 + p4 + plot_layout(nrow = 2)
}



quick_plot3 <- function(df) {
  new_df <- pivot_longer(df, !time, names_to = "Status", values_to = "Number")
  new_df <- new_df %>% mutate(Animal_type = str_sub(Status, 1, 1))

  these_colours <- c(
    "CS" = "blue", "CEs" = "orange", "CEr" = "orange",
    "CIs" = "red", "CIr" = "red", "CTs" = "green", "CTr" = "green",
    "PF" = "purple", "PS" = "skyblue", "PEs" = "orange", "PEr" = "orange",
    "PIs" = "red", "PIr" = "red", "PPs" = "grey", "PPr" = "grey", "PTs" = "green", "PTr" = "green",
    "WS" = "blue", "WEs" = "orange", "WEr" = "orange", "WIs" = "red", "WIr" = "red",
    "VSt" = "purple", "Vsf" = "blue", "VEs" = "orange", "VEr" = "orange",
    "VIs" = "red", "VIr" = "red", "Cattle_total" = "pink", "Vector_total" = "black", 
    "Wildlife_total" = "black"
  )
  plot_C <- new_df %>%
    filter(Animal_type == "C") %>%
    ggplot() +
    geom_line(aes(x = time, y = Number, colour = Status), linewidth = 1.5) +
    xlab("Time (days)") +
    scale_color_manual(" ", values = these_colours) +
    ggtitle("Cattle, no prophylaxis") 

  plot_P <- new_df %>%
    filter(Animal_type == "P") %>%
    ggplot() +
    geom_line(aes(x = time, y = Number, colour = Status), linewidth = 1.5) +
    xlab("Time (days)") +
    scale_color_manual(" ", values = these_colours) +
    ggtitle("Cattle with prophylaxis") 

  plot_W <- new_df %>%
    filter(Animal_type == "W") %>%
    ggplot() +
    geom_line(aes(x = time, y = Number, colour = Status), linewidth = 1.5) +
    xlab("Time (days)") +
    scale_color_manual(" ", values = these_colours) +
    ggtitle("Wildlife") 

  plot_V <- new_df %>%
    filter(Animal_type == "V") %>%
    ggplot() +
    geom_line(aes(x = time, y = Number, colour = Status), linewidth = 1.5) +
    xlab("Time (days)") +
    scale_color_manual(" ", values = these_colours) +
    ggtitle("Vector") 

  plot_C + plot_P + plot_W + plot_V
}


R0_plot <- function(df){
  
  ggplot(df) +
    geom_line(aes(x = time, y = R0sen), colour = "black") +
    geom_line(aes(x = time, y = R0res), colour = "red") +
    geom_line(aes(x = time, y = Rsen), colour = "black") +
    geom_line(aes(x = time, y = Rres), colour = "red") +
    geom_hline(yintercept = 1, linetype = "dashed")
}
