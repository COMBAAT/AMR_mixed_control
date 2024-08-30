
# =========================================================
# Function Names: quick_plot, quick_plot2, quick_plot3, R0_and_R_trajectories
# Description: This script provides functions for generating quick visualizations of epidemiological data, including time series
#              plots of disease stages and treatments as well as R0 and reproduction number trajectories. These functions utilize
#              both base R plotting and `ggplot2` for detailed visual representations.
#
# Parameters:
#   df - A dataframe containing time series data and various epidemiological measures to be plotted.
#
# Returns:
#   Graphical outputs displayed in an R plotting window, showing the progress of disease stages and treatments over time, along
#   with R0 and reproduction number trajectories.
#
# Example of use:
#   data <- read.csv("path/to/data.csv")
#   quick_plot(data)
#   quick_plot2(data)
#   quick_plot3(data)
#   R0_and_R_trajectories(data)
#
# Dependencies: Requires 'ggplot2', 'gghighlight', 'dplyr', 'patchwork', 'stringr', and 'tidyr' for data manipulation and visualization.
#
# Author: Shaun Keegan & Louise Matthews
# Date Created: August 2024
# Last Modified: August 2024
# =========================================================
library(ggplot2)
library(gghighlight)
library(dplyr)
library(patchwork)
library(stringr)
library(tidyr)


# Source files and function
source("funcs/plot_helper.R")



#-------------------------------------------------------------------------------
# Function Name: quick_plot
#
# Description:
#   This function generates a series of time series plots for various stages of disease 
#   progression across different population groups, including cattle without prophylaxis, 
#   cattle with prophylaxis, wildlife, and vectors. It visualizes susceptible, exposed, 
#   infected, treated, and total populations over time, with distinct colors and line types 
#   for easy differentiation.
#
# Parameters:
#   df - A dataframe containing time series data for different population groups and their 
#        respective stages of disease progression. Required columns include 'time', 'CS', 
#        'CEs', 'CEr', 'CIs', 'CIr', 'CTs', 'CTr', 'PF', 'PS', 'PEs', 'PEr', 'PIs', 'PIr', 
#        'PTs', 'PTr', 'PPs', 'PPr', 'WS', 'WEs', 'WEr', 'WIs', 'WIr', 'VSt', 'VSf', 'VEs', 
#        'VEr', 'VIs', 'VIr'.
#
# Returns:
#   This function does not return a value. It outputs plots to the R plotting window, showing 
#   the progression of disease across different populations and treatments over time.
#
# Example of use:
#   df <- read.csv("path/to/data.csv")
#   quick_plot(df)
#
# Dependencies:
#   Base R plotting functions are used, with no additional package dependencies.
#
#-------------------------------------------------------------------------------


quick_plot <- function(df) {
  par(mfrow = c(2, 2))
  plot(df$CS ~ df$time,
    type = "l", ylim = c(0, 55),
    lwd = 3, col = "blue", main = "Cattle (no Prophylaxis)",
    xlab = "Time", ylab = "Number"
  )
  lines(df$CEs ~ df$time, lwd = 3, col = "orange") # Exposed
  lines(df$CEr ~ df$time, lwd = 3, col = "orange", lty = 2) # Exposed
  lines(df$CIs ~ df$time, lwd = 3, col = "red") # Infected
  lines(df$CIr ~ df$time, lwd = 3, col = "red", lty = 2) # Infected
  lines(df$CTs ~ df$time, lwd = 3, col = "green") # Treated
  lines(df$CTr ~ df$time, lwd = 3, col = "green", lty = 2) # Treated
  lines((df$CEs + df$CEr + df$CIs + df$CIr + df$CTs + df$CTr + df$CS) ~ df$time, lty = 2)
  lines((df$CEs + df$CEr + df$CIs + df$CIr + df$CTs + df$CTr + df$CS +
    df$PEs + df$PEr + df$PIs + df$PIr + df$PTs + df$PTr + df$PS + df$PF + df$PPr + df$PPs) ~ df$time, lty = 2, col = "pink")
  # legend("topright", col = c("blue", "orange", "darkorange", "red","darkred", "green", "darkgreen", "grey"),
  #       y = c("CS", "CEs", "CEr", "CIs", "CIr","CTs", "CTr"), pch = 15)

  plot(df$PF ~ df$time,
    type = "l", ylim = c(0, 55), col = "purple",
    lwd = 3, main = "Cattle (with Prophylaxis)", xlab = "Time", ylab = "Number"
  )
  lines(df$PS ~ df$time, lwd = 3, col = "blue") # Exposed
  lines(df$PEs ~ df$time, lwd = 3, col = "orange") # Exposed
  lines(df$PEr ~ df$time, lwd = 3, col = "orange", lty = 2) # Exposed
  lines(df$PIs ~ df$time, lwd = 3, col = "red") # Infected
  lines(df$PIr ~ df$time, lwd = 3, col = "red", lty = 2) # Infected
  lines(df$PTs ~ df$time, lwd = 3, col = "green") # Treated
  lines(df$PTr ~ df$time, lwd = 3, col = "green", lty = 2) # Treated
  lines(df$PPs ~ df$time, lwd = 3, col = "grey") # Treated
  lines(df$PPr ~ df$time, lwd = 3, col = "grey", lty = 2) # Treated
  lines((df$PEs + df$PEr + df$PIs + df$PIr + df$PTs + df$PTr + df$PS + df$PF + df$PPr + df$PPs) ~ df$time, lty = 2)
  lines((df$CEs + df$CEr + df$CIs + df$CIr + df$CTs + df$CTr + df$CS +
    df$PEs + df$PEr + df$PIs + df$PIr + df$PTs + df$PTr + df$PS + df$PF + df$PPr + df$PPs) ~ df$time, lty = 2, col = "pink")
  # legend("topright", col = c("purple","blue", "orange", "darkorange", "red","darkred", "green", "darkgreen", "grey", "black", "grey"),
  #       y = c("PF","PS", "PEs", "PEr", "PIs", "PIr","PTs", "PTr", "PPs", "PPr"), pch = 15)

  plot(df$WS ~ df$time,
    type = "l", ylim = c(0, max(df$WS + 10)), col = "blue", lwd = 3,
    main = "Wildlife", xlab = "Time", ylab = "Number"
  )
  lines(df$WEs ~ df$time, lwd = 3, col = "orange") # Exposed
  lines(df$WEr ~ df$time, lwd = 3, col = "orange", lty = 2) # Exposed
  lines(df$WIs ~ df$time, lwd = 3, col = "red") # Infected
  lines(df$WIr ~ df$time, lwd = 3, col = "red", lty = 2) # Infected
  lines((df$WEs + df$WEr + df$WIs + df$WIr + df$WS) ~ df$time, lty = 2)
  # legend("topright", col = c("blue", "orange", "darkorange", "red","darkred"),
  #       y = c("WS", "WEs", "WEr", "WIs", "WIr"), pch = 15)

  plot(df$VSt ~ df$time,
    type = "l", ylim = c(0, max(df$VSt) + 100), col = "blue",
    lwd = 3, main = "Vector", xlab = "Time", ylab = "Number"
  )
  lines(df$VSf ~ df$time, lwd = 3, col = "lightblue") # Exposed
  lines(df$VEs ~ df$time, lwd = 3, col = "orange") # Exposed
  lines(df$VEr ~ df$time, lwd = 3, col = "orange", lty = 2) # Exposed
  lines(df$VIs ~ df$time, lwd = 3, col = "red") # Infected
  lines(df$VIr ~ df$time, lwd = 3, col = "red", lty = 2) # Infected
  lines((df$VEs + df$VEr + df$VIs + df$VIr + df$VSt + df$VSf) ~
    df$time, lty = 2)
  # legend("topright", col = c("blue", "lightblue","orange", "darkorange", "red","darkred"),
  #  €   y = c("VSt","VSf", "VEs", "VEr", "VIs", "VIr"), pch = 15)
}



#-------------------------------------------------------------------------------
# Function Name: quick_plot2
#
# Description:
#   This function generates a series of detailed time series plots for different stages of disease 
#   progression across various animal groups, including cattle with and without prophylaxis, wildlife, 
#   and vectors. It uses `ggplot2` to create layered line plots, with different colors representing 
#   susceptible, exposed, infected, treated, and total population states. The function produces a 
#   combined plot layout for easy comparison across groups.
#
# Parameters:
#   df - A dataframe containing time series data for different animal groups and their respective 
#        stages of disease progression. Required columns include 'time', 'CS', 'CEs', 'CEr', 
#        'CIs', 'CIr', 'CTs', 'CTr', 'PF', 'PS', 'PEs', 'PEr', 'PIs', 'PIr', 'PTs', 'PTr', 
#        'Prophylactic_total', 'WS', 'WEs', 'WEr', 'WIs', 'WIr', 'VSt', 'VSf', 'VEs', 'VEr', 
#        'VIs', 'VIr', 'Vector_total', 'Wildlife_total', 'Cattle_total'.
#
# Returns:
#   This function returns a combined plot object using the `patchwork` package, which displays the 
#   disease progression over time across different animal groups and stages, rendered in an R plotting window.
#
# Example of use:
#   df <- read.csv("path/to/data.csv")
#   quick_plot2(df)
#
# Dependencies:
#   Requires the following R packages:
#   - ggplot2: For creating the plots.
#   - patchwork: For arranging the multiple plots into a single layout.
#
#-------------------------------------------------------------------------------


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


#-------------------------------------------------------------------------------
# Function Name: quick_plot3
#
# Description:
#   This function generates a series of time series plots for various stages of disease 
#   progression across different animal groups, including cattle with and without prophylaxis, 
#   wildlife, and vectors. It first reshapes the input dataframe from a wide format to a long 
#   format, creating a new column for status and assigning appropriate colors for each status. 
#   The function then plots each animal group in separate panels with distinct colors for each 
#   disease state.
#
# Parameters:
#   df - A dataframe containing time series data for different animal groups and their respective 
#        stages of disease progression. Required columns include 'time' and various status 
#        indicators such as 'CS', 'CEs', 'CEr', 'CIs', 'CIr', 'CTs', 'CTr', 'PF', 'PS', 'PEs', 
#        'PEr', 'PIs', 'PIr', 'PPs', 'PPr', 'PTs', 'PTr', 'WS', 'WEs', 'WEr', 'WIs', 'WIr', 
#        'VSt', 'VSf', 'VEs', 'VEr', 'VIs', 'VIr', 'Cattle_total', 'Vector_total', and 
#        'Wildlife_total'.
#
# Returns:
#   This function returns a combined plot object using `ggplot2`, showing the progression of 
#   disease over time across different animal groups, rendered in an R plotting window. The 
#   plots are arranged in separate panels for each animal group (Cattle, Prophylactic Cattle, 
#   Wildlife, Vector).
#
# Example of use:
#   df <- read.csv("path/to/data.csv")
#   quick_plot3(df)
#
# Dependencies:
#   Requires the following R packages:
#   - ggplot2: For creating the plots.
#   - tidyr: For reshaping the dataframe from wide to long format.
#   - stringr: For manipulating strings within the dataframe.
#
#-------------------------------------------------------------------------------

quick_plot3 <- function(df) {
  new_df <- pivot_longer(df, !time, names_to = "Status", values_to = "Number")
  new_df <- new_df %>% mutate(Animal_type = str_sub(Status, 1, 1))

  these_colours <- c(
    "CS" = "blue", "CEs" = "orange", "CEr" = "orange",
    "CIs" = "red", "CIr" = "red", "CTs" = "green", "CTr" = "green",
    "PF" = "purple", "PS" = "skyblue", "PEs" = "orange", "PEr" = "orange",
    "PIs" = "red", "PIr" = "red", "PPs" = "grey", "PPr" = "grey", "PTs" = "green", "PTr" = "green",
    "WS" = "blue", "WEs" = "orange", "WEr" = "orange", "WIs" = "red", "WIr" = "red",
    "VSt" = "purple", "VSf" = "blue", "VEs" = "orange", "VEr" = "orange",
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


#-------------------------------------------------------------------------------
# Function Name: R0_and_R_trajectories
#
# Description:
#   This function generates a time series plot of the basic reproduction number (R0) and the 
#   reproduction number (R) trajectories over time. It visualizes both sensitive (R0sen, Rsen) 
#   and resistant (R0res, Rres) strains using different colors and line types. The function 
#   reshapes the input dataframe from wide to long format and then plots the trajectories using 
#   `ggplot2`. A horizontal line at y = 1 is added to indicate the critical threshold for 
#   disease spread.
#
# Parameters:
#   df - A dataframe containing time series data for R0 and R values, with columns 'time', 
#        'R0res', 'R0sen', 'Rres', and 'Rsen'.
#
# Returns:
#   This function returns a ggplot object that displays the R0 and R trajectories over time. 
#   The plot is rendered in an R plotting window, with distinct colors and line types for each 
#   type of reproduction number.
#
# Example of use:
#   df <- read.csv("path/to/data.csv")
#   R0_and_R_trajectories(df)
#
# Dependencies:
#   Requires the following R packages:
#   - ggplot2: For creating the plots.
#   - tidyr: For reshaping the dataframe from wide to long format.
#   - dplyr: For data manipulation and selection.
#
#-------------------------------------------------------------------------------

R0_and_R_trajectories <- function(df) {
  my_colours <- c("R0sen" = "black", "R0res" = "red", "Rsen" = "black", "Rres" = "red")
  my_lines <- c("R0sen" = "dotted", "R0res" = "dotted", "Rsen" = "solid", "Rres" = "solid")

  df_long <- df %>%
    select(time, R0res, R0sen, Rres, Rsen) %>%
    pivot_longer(!time, names_to = "R_type", values_to = "R0_R_value")

  p <- ggplot(df_long) +
    geom_line(aes(x = time, y = R0_R_value, colour = R_type, linetype = R_type)) +
    scale_color_manual(values = my_colours) +
    scale_linetype_manual(values = my_lines) +
    geom_hline(yintercept = 1, linetype = "dotted")
  ylab("R0 or R value") +
    guides(
      color = guide_legend(title = ""),
      linetype = guide_legend(title = "")
    )
  p
}
