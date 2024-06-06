library(ggplot2)
library(gghighlight)
library(dplyr)
library(patchwork)
library(tidyr)
library(stringr)

# Source files and function
source("funcs/plot_helper.R")

quick_plot <- function(ODEinput){
  
  
  par(mfrow = c(2, 2))
  plot(ODEinput$CS ~ ODEinput$times, type = 'l', ylim = c(0, 55), 
       lwd = 3, col = 'blue', main = "Cattle (no Prophylaxis)", 
       xlab = "Time", ylab = "Number")
  lines(ODEinput$CEs ~ ODEinput$times, lwd = 3, col = 'orange') # Exposed
  lines(ODEinput$CEr ~ ODEinput$times, lwd = 3, col = 'orange', lty = 2) # Exposed
  lines(ODEinput$CIs ~ ODEinput$times, lwd = 3, col = 'red') # Infected
  lines(ODEinput$CIr ~ ODEinput$times, lwd = 3, col = 'red', lty = 2) # Infected
  lines(ODEinput$CTs ~ ODEinput$times, lwd = 3, col = 'green') # Treated
  lines(ODEinput$CTr ~ ODEinput$times, lwd = 3, col = 'green', lty = 2) # Treated
  lines(( ODEinput$CEs + ODEinput$CEr + ODEinput$CIs + ODEinput$CIr + ODEinput$CTs + ODEinput$CTr + ODEinput$CS) ~ODEinput$times,lty = 2)
  lines(( ODEinput$CEs + ODEinput$CEr + ODEinput$CIs + ODEinput$CIr + ODEinput$CTs + ODEinput$CTr + ODEinput$CS +
            ODEinput$PEs + ODEinput$PEr + ODEinput$PIs + ODEinput$PIr + ODEinput$PTs + ODEinput$PTr + ODEinput$PS + ODEinput$PF+ ODEinput$PPr + ODEinput$PPs) ~ODEinput$times,lty = 2, col = "pink")
  #legend("topright", col = c("blue", "orange", "darkorange", "red","darkred", "green", "darkgreen", "grey"),
  #       y = c("CS", "CEs", "CEr", "CIs", "CIr","CTs", "CTr"), pch = 15)
  
  plot( ODEinput$PF ~ ODEinput$times, type = 'l', ylim = c(0, 55),col = 'purple',
        lwd = 3,main = "Cattle (with Prophylaxis)",xlab = "Time", ylab = "Number")
  lines(ODEinput$PS ~ ODEinput$times, lwd = 3, col = 'blue') # Exposed
  lines(ODEinput$PEs ~ ODEinput$times, lwd = 3, col = 'orange') # Exposed
  lines(ODEinput$PEr ~ ODEinput$times, lwd = 3, col = 'orange', lty = 2) # Exposed
  lines(ODEinput$PIs ~ ODEinput$times, lwd = 3, col = 'red') # Infected
  lines(ODEinput$PIr ~ ODEinput$times, lwd = 3, col = 'red', lty = 2) # Infected
  lines(ODEinput$PTs ~ ODEinput$times, lwd = 3, col = 'green') # Treated
  lines(ODEinput$PTr ~ ODEinput$times, lwd = 3, col = 'green', lty = 2) # Treated
  lines(ODEinput$PPs ~ ODEinput$times, lwd = 3, col = 'grey') # Treated
  lines(ODEinput$PPr ~ ODEinput$times, lwd = 3, col = 'grey', lty = 2) # Treated
  lines((ODEinput$PEs + ODEinput$PEr + ODEinput$PIs + ODEinput$PIr + ODEinput$PTs + ODEinput$PTr  + ODEinput$PS + ODEinput$PF + ODEinput$PPr + ODEinput$PPs) ~ODEinput$times, lty = 2)
  lines(( ODEinput$CEs + ODEinput$CEr + ODEinput$CIs + ODEinput$CIr + ODEinput$CTs + ODEinput$CTr + ODEinput$CS +
            ODEinput$PEs + ODEinput$PEr + ODEinput$PIs + ODEinput$PIr + ODEinput$PTs + ODEinput$PTr + ODEinput$PS+ ODEinput$PF+ ODEinput$PPr + ODEinput$PPs) ~ODEinput$times,lty = 2, col = "pink")
  #legend("topright", col = c("purple","blue", "orange", "darkorange", "red","darkred", "green", "darkgreen", "grey", "black", "grey"),
  #       y = c("PF","PS", "PEs", "PEr", "PIs", "PIr","PTs", "PTr", "PPs", "PPr"), pch = 15)
  
  plot(ODEinput$WS ~ ODEinput$times,type = 'l', ylim = c(0, max(ODEinput$WS+10)),col = 'blue',lwd = 3,
       main = "Wildlife", xlab = "Time", ylab = "Number")
  lines(ODEinput$WEs ~ ODEinput$times, lwd = 3, col = 'orange') # Exposed
  lines(ODEinput$WEr ~ ODEinput$times, lwd = 3, col = 'orange', lty = 2) # Exposed
  lines(ODEinput$WIs ~ ODEinput$times, lwd = 3, col = 'red') # Infected
  lines(ODEinput$WIr ~ ODEinput$times, lwd = 3, col = 'red', lty = 2) # Infected
  lines((ODEinput$WEs + ODEinput$WEr + ODEinput$WIs + ODEinput$WIr + ODEinput$WS) ~ ODEinput$times,lty = 2)
  #legend("topright", col = c("blue", "orange", "darkorange", "red","darkred"),
  #       y = c("WS", "WEs", "WEr", "WIs", "WIr"), pch = 15)
  
  plot(ODEinput$VSt ~ ODEinput$times, type = 'l', ylim = c(0, max(ODEinput$VSt)+100), col = 'blue',
       lwd = 3, main = "Vector", xlab = "Time", ylab = "Number")
  lines(ODEinput$VSf ~ ODEinput$times, lwd = 3, col = 'lightblue') # Exposed
  lines(ODEinput$VEs ~ ODEinput$times, lwd = 3, col = 'orange') # Exposed
  lines(ODEinput$VEr ~ ODEinput$times, lwd = 3, col = 'orange', lty = 2) # Exposed
  lines(ODEinput$VIs ~ ODEinput$times, lwd = 3, col = 'red') # Infected
  lines(ODEinput$VIr ~ ODEinput$times, lwd = 3, col = 'red', lty = 2) # Infected
  lines((ODEinput$VEs + ODEinput$VEr + ODEinput$VIs + ODEinput$VIr + ODEinput$VSt + ODEinput$VSf) ~
          ODEinput$times, lty = 2)
  #legend("topright", col = c("blue", "lightblue","orange", "darkorange", "red","darkred"),
  #  €   y = c("VSt","VSf", "VEs", "VEr", "VIs", "VIr"), pch = 15)
  
}



quick_plot2 <- function(df) {
  ggplot(df) +
    geom_line(aes(x = times, y = CS), colour = "blue") +
    geom_line(aes(x = times, y = CEs), colour = "orange") +
    geom_line(aes(x = times, y = CEr), colour = "orange") +
    geom_line(aes(x = times, y = CIs), colour = "red") +
    geom_line(aes(x = times, y = CIr), colour = "red") +
    geom_line(aes(x = times, y = CTs), colour = "green") +
    geom_line(aes(x = times, y = CTr), colour = "green") +
    ylab("Number") +
    xlab("Time, days") +
    ggtitle("Cattle, no prophylaxis")

  this_linewidth <- 1.5
  p1 <- ggplot(df) +
    geom_line(aes(x = times, y = CS, colour = "Sus"), linewidth = this_linewidth) +
    geom_line(aes(x = times, y = CEs, colour = "Exp s"), linewidth = this_linewidth) +
    geom_line(aes(x = times, y = CEr, colour = "Exp r"), linewidth = this_linewidth) +
    geom_line(aes(x = times, y = CIs, colour = "Inf s"), linewidth = this_linewidth) +
    geom_line(aes(x = times, y = CIr, colour = "Inf r"), linewidth = this_linewidth) +
    geom_line(aes(x = times, y = CTs, colour = "Trt s"), linewidth = this_linewidth) +
    geom_line(aes(x = times, y = CTr, colour = "Trt r"), linewidth = this_linewidth) +
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
    geom_line(aes(x = times, y = PF, colour = "Full"), linewidth = this_linewidth) +
    geom_line(aes(x = times, y = PS, colour = "Partial"), linewidth = this_linewidth) +
    geom_line(aes(x = times, y = PEs, colour = "Exp s"), linewidth = this_linewidth) +
    geom_line(aes(x = times, y = PEr, colour = "Exp r"), linewidth = this_linewidth) +
    geom_line(aes(x = times, y = PIs, colour = "Inf s"), linewidth = this_linewidth) +
    geom_line(aes(x = times, y = PIr, colour = "Inf r"), linewidth = this_linewidth) +
    geom_line(aes(x = times, y = PTs, colour = "Trt s"), linewidth = this_linewidth) +
    geom_line(aes(x = times, y = PTr, colour = "Trt r"), linewidth = this_linewidth) +
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
    geom_line(aes(x = times, y = WS, colour = "Sus"), linewidth = this_linewidth) +
    geom_line(aes(x = times, y = WEs, colour = "Exp s"), linewidth = this_linewidth) +
    geom_line(aes(x = times, y = WEr, colour = "Exp r"), linewidth = this_linewidth) +
    geom_line(aes(x = times, y = WIs, colour = "Inf s"), linewidth = this_linewidth) +
    geom_line(aes(x = times, y = WIr, colour = "Inf r"), linewidth = this_linewidth) +
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
    geom_line(aes(x = times, y = VSf, colour = "Ten"), linewidth = this_linewidth) +
    geom_line(aes(x = times, y = VSt, colour = "Sus"), linewidth = this_linewidth) +
    geom_line(aes(x = times, y = VEs, colour = "Exp s"), linewidth = this_linewidth) +
    geom_line(aes(x = times, y = VEr, colour = "Exp r"), linewidth = this_linewidth) +
    geom_line(aes(x = times, y = VIs, colour = "Inf s"), linewidth = this_linewidth) +
    geom_line(aes(x = times, y = VIr, colour = "Inf r"), linewidth = this_linewidth) +
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
  new_df <- pivot_longer(df, !times, names_to = "Status", values_to = "Number")
  new_df <- new_df %>% mutate(Animal_type = str_sub(Status, 1, 1))

  these_colours <- c(
    "CS" = "blue", "CEs" = "orange", "CEr" = "orange",
    "CIs" = "red", "CIr" = "red", "CTs" = "green", "CTr" = "green",
    "PF" = "purple", "PS" = "skyblue", "PEs" = "orange", "PEr" = "orange",
    "PIs" = "red", "PIr" = "red", "PPs" = "grey", "PPr" = "grey", "PTs" = "green", "PTr" = "green",
    "WS" = "blue", "WEs" = "orange", "WEr" = "orange", "WIs" = "red", "WIr" = "red",
    "VSt" = "purple", "Vsf" = "blue", "VEs" = "orange", "VEr" = "orange",
    "VIs" = "red", "VIr" = "red"
  )
  plot_C <- new_df %>%
    filter(Animal_type == "C") %>%
    ggplot() +
    geom_line(aes(x = times, y = Number, colour = Status), linewidth = 1.5) +
    xlab("Time (days)") +
    scale_color_manual(" ", values = these_colours) +
    ggtitle("Cattle, no prophylaxis") +
    my_theme()

  plot_P <- new_df %>%
    filter(Animal_type == "P") %>%
    ggplot() +
    geom_line(aes(x = times, y = Number, colour = Status), linewidth = 1.5) +
    xlab("Time (days)") +
    scale_color_manual(" ", values = these_colours) +
    ggtitle("Cattle with prophylaxis") +
    my_theme()

  plot_W <- new_df %>%
    filter(Animal_type == "W") %>%
    ggplot() +
    geom_line(aes(x = times, y = Number, colour = Status), linewidth = 1.5) +
    xlab("Time (days)") +
    scale_color_manual(" ", values = these_colours) +
    ggtitle("Wildlife") +
    my_theme()

  plot_V <- new_df %>%
    filter(Animal_type == "V") %>%
    ggplot() +
    geom_line(aes(x = times, y = Number, colour = Status), linewidth = 1.5) +
    xlab("Time (days)") +
    scale_color_manual(" ", values = these_colours) +
    ggtitle("Vector") +
    my_theme()

  plot_C + plot_P + plot_W + plot_V
}
