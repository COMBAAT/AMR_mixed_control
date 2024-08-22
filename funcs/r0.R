
# =========================================================
# Function Names: R_calc_sen_or_res, calculate_R0, calculate_R_from_row_of_df, add_R_trajectories, add_R0
# Description: This script provides functions for calculating the basic reproduction number (R0) and other related
#              reproduction metrics for sensitive or resistant strains of a disease. These functions account for various
#              epidemiological parameters, treatment effects, and population compartments to determine the potential
#              spread of an infectious disease within a population.
#
# Parameters:
#   params - A named list of parameters including rates of transmission, infection, and other disease dynamics.
#   Nc, Npf, Nps, Nw, Nv - Numeric values representing different compartments of the population.
#   is_strain_sensitive - Boolean indicating if the strain is sensitive.
#   basic - Boolean indicating if the calculation is for basic R0 or includes additional factors.
#
# Returns:
#   Numeric values representing calculated R0 or reproduction trajectories.
#
# Example of use:
#   params <- list(...)
#   R0_value <- R_calc_sen_or_res(params, Nc, Npf, Nps, Nw, Nv, TRUE, FALSE)
#
# Dependencies: Requires the 'codetools' package for managing code properties.
#
# Author: Shaun Keegan & Louise Matthews
# Date Created: August 2024
# Last Modified: August 2024
# =========================================================


library(codetools)

#-------------------------------------------------------------------------------
# Function Name: R_calc_sen_or_res
#
# Description:
#   This function calculates the basic reproduction number (R0) or the reproduction number (R)
#   for either sensitive or resistant strains of a disease within a host-vector population model.
#   It incorporates various epidemiological parameters to compute the transmission potential
#   via different routes (cattle, prophylactic cattle, and wildlife) and integrates the impact
#   of treatments and prophylaxis.
#
# Parameters:
#   params - A named vector or list containing the model parameters, including transmission rates,
#            treatment efficacy, and demographic factors.
#   Nc - Number of cattle susceptible to infection.
#   Npf - Number of prophylactic cattle fully protected from infection.
#   Nps - Number of prophylactic cattle partially protected from infection.
#   Nw - Number of wildlife susceptible to infection.
#   Nv - Number of vectors capable of transmitting the infection.
#   is_strain_sensitive - A string ("yes" or "no") indicating whether the strain is sensitive or resistant.
#   basic - A string ("yes" or "no") indicating whether to calculate the basic reproduction number (R0) or the reproduction number (R).
#
# Returns:
#   A numeric value representing the calculated reproduction number (R0 or R) for the specified strain
#   within the host-vector population.
#
# Example of use:
#   params <- c(NC = 1000, NW = 500, biterate = 0.5, prob_infection_to_host = 0.1, ...)
#   R0sen <- R_calc_sen_or_res(params, Nc = 1000, Npf = 500, Nps = 300, Nw = 200, Nv = 100, is_strain_sensitive = "yes", basic = "yes")
#
# Dependencies:
#   The function relies on the presence of the necessary epidemiological parameters within the `params`
#   vector or list. It assumes that all required parameters are provided and correctly named.
#
#-------------------------------------------------------------------------------


R_calc_sen_or_res <- function(params, Nc, Npf, Nps, Nw, Nv, is_strain_sensitive, basic) {
  Nh <- params["NC"] + params["NW"]

  biterate <- params["biterate"]
  prob_infection_to_host <- params["prob_infection_to_host"]
  partial_susceptibility_proph_cattle <- params["partial_susceptibility_proph_cattle"]
  prob_infection_to_vector <- params["prob_infection_to_vector"]
  fit_adj <- params["fit_adj"]

  treatment_p <- params["treatment_p"]
  treatment_q <- params["treatment_q"]
  waning <- params["waning"]

  gamma_c <- params["gamma_c"]
  death_c <- params["death_c"]
  sigma_c <- params["sigma_c"]
  sigma_st <- params["sigma_st"]

  gamma_p <- params["gamma_c"]
  death_p <- params["death_c"]
  sigma_p <- params["sigma_c"]

  gamma_w <- params["gamma_w"]
  death_w <- params["death_w"]
  sigma_w <- params["sigma_w"]

  gamma_v <- params["gamma_v"]
  death_v <- params["death_v"]
  proph_ongoing <- params["proph_ongoing"]


  if (is_strain_sensitive == "yes") {
    sigma_treated <- sigma_st
  }
  if (is_strain_sensitive == "no") {
    sigma_treated <- sigma_c
  }
  if (is_strain_sensitive == "no") {
    prob_infection_to_host <- prob_infection_to_host * fit_adj
  }

  

  # Probability of I -> Tp
  p1c <- (treatment_p + proph_ongoing) / (treatment_p + treatment_q + sigma_c + death_c + proph_ongoing)

  # Probability of Tp -> I
  # p2c <- waning / (treatment_p + treatment_q + sigma_st + death_c)
  p2c <- waning / (waning + sigma_treated + death_c) # LM corrected


  rate_vectors_infected <- biterate * prob_infection_to_vector * Nv / Nh * gamma_v / (gamma_v + death_v)
  prob_CI_from_CE <- gamma_c / (gamma_c + death_c + proph_ongoing)
  prob_PI_from_PE <- gamma_p / (gamma_p + death_p + proph_ongoing + waning)
  time_in_CI <- 1 / (treatment_p + treatment_q + sigma_c + death_c + proph_ongoing)
  time_in_CT <- 1 / (sigma_treated + death_c)
  time_in_PI <- 1 / (treatment_p + treatment_q + sigma_p + death_p + waning + proph_ongoing)
  time_in_PT <- 1 / (sigma_treated + death_p + waning)
  time_in_PP <- 1 / (sigma_treated + death_p + waning)

  prob_CI_treat_q <- treatment_q / (treatment_p + treatment_q + sigma_c + death_c + proph_ongoing)
  prob_CI_treat_p <- treatment_p / (treatment_p + treatment_q + sigma_c + death_c + proph_ongoing)
  prob_PI_treat_q <- treatment_q / (treatment_p + treatment_q + sigma_p + death_p + waning + proph_ongoing)
  prob_PI_treat_p <- treatment_p / (treatment_p + treatment_q + sigma_p + death_p + waning + proph_ongoing)
  prob_waning_from_PE <- waning / (gamma_p + death_p + waning + proph_ongoing)
  prob_waning_from_PI <- waning / (treatment_p + treatment_q + sigma_p + death_p + waning + proph_ongoing)
  prob_waning_from_PT <- waning / (sigma_treated + death_p + waning)
  prob_waning_from_PP <- waning / (sigma_treated + death_p + waning)
  prob_proph_from_CI <- proph_ongoing / (treatment_p + treatment_q + sigma_c + death_c + proph_ongoing)
  prob_proph_from_PI <- proph_ongoing / (treatment_p + treatment_q + sigma_p + death_p + waning + proph_ongoing)
  prob_proph_from_CE <- proph_ongoing / (gamma_c + death_c + proph_ongoing)
  prob_proph_from_PE <- proph_ongoing / (gamma_p + death_p + proph_ongoing + waning)
  prob_disease_from_CEX <- gamma_c / (gamma_c + death_c + sigma_treated)

  time_infectious_route1 <- (time_in_CI + 
                              prob_CI_treat_q * time_in_CT +
                              prob_CI_treat_p * time_in_PP +
                              prob_proph_from_CI * time_in_PP) / (1 - p1c * p2c)

  
  prob_PP_from_CE <- prob_proph_from_CE * prob_disease_from_CEX
  
  # transmission via C
  R1 <- rate_vectors_infected * time_infectious_route1 
  RVC <- prob_CI_from_CE * R1  + prob_PP_from_CE * prob_waning_from_PP * R1 + prob_PP_from_CE * time_in_PP * rate_vectors_infected
  RVC <- as.numeric(RVC)
  
  RCV <- biterate * prob_infection_to_host * Nc / Nh * 1 / (death_v)
  RCV <- as.numeric(RCV)

  # transmission via P
  prob_proph_from_PE <- proph_ongoing / (gamma_p + death_p + proph_ongoing + waning) * 
    gamma_p / (gamma_p + death_p + sigma_treated)
  
  RVP1 <- rate_vectors_infected * time_in_PI + prob_waning_from_PI * R1 + # contribution from PIs
    rate_vectors_infected * prob_PI_treat_q * time_in_PT +
    rate_vectors_infected * prob_PI_treat_q * prob_waning_from_PT * time_in_CT +
    
    rate_vectors_infected * prob_PI_treat_p * time_in_PP + # contrib from PPs
    # contribution from waning back to CIS
    prob_PI_treat_p * prob_waning_from_PP * R1 +
    
    rate_vectors_infected * prob_proph_from_PI * time_in_PP +
    prob_proph_from_PI * prob_waning_from_PP * R1
  
  
  
  RVP2 <- prob_proph_from_PE * time_in_PP * rate_vectors_infected + 
    prob_proph_from_PE * prob_waning_from_PP * R1
  
  
  RVP <- RVP1 * prob_PI_from_PE + RVP2 + prob_waning_from_PE * RVC
  RVP <- as.numeric(RVP)
  
  if (is_strain_sensitive == "yes") {
    RPV <- biterate * partial_susceptibility_proph_cattle * prob_infection_to_host * Nps / Nh * 1 / (death_v)
  }
  if (is_strain_sensitive == "no") {
    RPV <- biterate * prob_infection_to_host * (Nps + Npf) / Nh * 1 / (death_v)
  }
  RPV <- as.numeric(RPV)

  # transmission via W
  RVW <- biterate * prob_infection_to_vector * Nv / Nh * 1 / (sigma_w + death_w) * gamma_v / (gamma_v + death_v)
  RVW <- as.numeric(RVW)
  
  RWV <- biterate * prob_infection_to_host * Nw / Nh * gamma_w / (gamma_w + death_w) * 1 / (death_v)
  RWV <- as.numeric(RWV)
  

  reproduction_number <- RCV * RVC + RPV * RVP + RWV * RVW
  reproduction_number
}


#-------------------------------------------------------------------------------
# Function Name: calculate_R0
#
# Description:
#   This function calculates the basic reproduction number (R0) for both sensitive and resistant 
#   strains of a disease within a host-vector population model. It utilizes the `R_calc_sen_or_res` 
#   function to compute R0 values based on the provided epidemiological parameters.
#
# Parameters:
#   params - A named vector or list containing the model parameters, including:
#            - PF: Number of fully protected prophylactic cattle.
#            - PS: Number of partially protected prophylactic cattle.
#            - CS: Number of cattle susceptible to infection.
#            - NW: Number of wildlife susceptible to infection.
#            - NV: Number of vectors capable of transmitting the infection.
#
# Returns:
#   A named numeric vector with two elements:
#   - R0sen: The basic reproduction number for the sensitive strain.
#   - R0res: The basic reproduction number for the resistant strain.
#
# Example of use:
#   params <- c(PF = 500, PS = 300, CS = 1000, NW = 200, NV = 100, ...)
#   R0_values <- calculate_R0(params)
#   print(R0_values)
#
# Dependencies:
#   This function relies on the `R_calc_sen_or_res` function to compute the R0 values for both 
#   sensitive and resistant strains. The `params` vector or list must contain all necessary 
#   parameters with the correct names.
#
#-------------------------------------------------------------------------------


calculate_R0 <- function(params) {
  Npf <- params["PF"]
  Nps <- params["PS"]
  Nc <- params["CS"]
  Nw <- params["NW"]
  Nv <- params["NV"]
  R0sen <- R_calc_sen_or_res(params, Nc, Npf, Nps, Nw, Nv, is_strain_sensitive = "yes", basic = "yes")
  R0res <- R_calc_sen_or_res(params, Nc, Npf, Nps, Nw, Nv, is_strain_sensitive = "no", basic = "yes")
  c("R0sen" = R0sen, "R0res" = R0res)
}


#-------------------------------------------------------------------------------
# Function Name: calculate_R_from_row_of_df
#
# Description:
#   This function calculates the reproduction number (R) for both sensitive and resistant strains 
#   of a disease based on a single row of data from a dataframe. The function extracts the relevant 
#   population counts from the provided row and uses the `R_calc_sen_or_res` function to compute 
#   the R values.
#
# Parameters:
#   params - A named vector or list containing the model parameters necessary for calculating the 
#            reproduction number (R).
#   this_row - A single row from a dataframe containing the population counts for:
#              - CS: Number of cattle susceptible to infection.
#              - PS: Number of partially protected prophylactic cattle.
#              - PF: Number of fully protected prophylactic cattle.
#              - WS: Number of wildlife susceptible to infection.
#              - VSt: Number of susceptible vectors.
#              - VSf: Number of fully protected vectors.
#
# Returns:
#   A named numeric vector with two elements:
#   - Rsen: The reproduction number for the sensitive strain.
#   - Rres: The reproduction number for the resistant strain.
#
# Example of use:
#   df <- read.csv("path/to/data.csv")
#   this_row <- df[1, ]
#   R_values <- calculate_R_from_row_of_df(params, this_row)
#   print(R_values)
#
# Dependencies:
#   This function relies on the `R_calc_sen_or_res` function to compute the R values for both 
#   sensitive and resistant strains. The `this_row` dataframe row must contain all necessary 
#   population counts with the correct column names.
#
#-------------------------------------------------------------------------------


calculate_R_from_row_of_df <- function(params, this_row) {
  Nc <- this_row$CS
  Nps <- this_row$PS
  Npf <- this_row$PF
  Nw <- this_row$WS
  Nv <- this_row$VSt + this_row$VSf
  Rsen <- R_calc_sen_or_res(params, Nc, Npf, Nps, Nw, Nv, is_strain_sensitive = "yes", basic = "no")
  Rres <- R_calc_sen_or_res(params, Nc, Npf, Nps, Nw, Nv, is_strain_sensitive = "no", basic = "no")
  c("Rsen" = Rsen, "Rres" = Rres)
}


#-------------------------------------------------------------------------------
# Function Name: add_R_trajectories
#
# Description:
#   This function computes the reproduction number (R) trajectories for both sensitive and resistant 
#   strains across all rows of a dataframe. It iterates through each row of the dataframe, calculates 
#   the R values using the `calculate_R_from_row_of_df` function, and appends these values as new columns 
#   (Rsen and Rres) to the dataframe.
#
# Parameters:
#   params - A named vector or list containing the model parameters necessary for calculating the 
#            reproduction number (R).
#   df - A dataframe containing time series data for different population groups and their respective 
#        stages of disease progression. The dataframe must include columns for:
#        - CS: Number of cattle susceptible to infection.
#        - PS: Number of partially protected prophylactic cattle.
#        - PF: Number of fully protected prophylactic cattle.
#        - WS: Number of wildlife susceptible to infection.
#        - VSt: Number of susceptible vectors.
#        - VSf: Number of fully protected vectors.
#
# Returns:
#   The input dataframe `df`, with two additional columns:
#   - Rsen: The reproduction number trajectory for the sensitive strain.
#   - Rres: The reproduction number trajectory for the resistant strain.
#
# Example of use:
#   df <- read.csv("path/to/data.csv")
#   df_with_R <- add_R_trajectories(params, df)
#   head(df_with_R)
#
# Dependencies:
#   This function relies on the `calculate_R_from_row_of_df` function to compute the R values for each 
#   row in the dataframe. The input dataframe `df` must contain all necessary population counts with 
#   the correct column names.
#
#-------------------------------------------------------------------------------


add_R_trajectories <- function(params, df) {
  Rsen_vec <- c()
  Rres_vec <- c()
  for (i in 1:nrow(df)) {
    this_row <- df[i, ]
    Rsen_and_Rres <- calculate_R_from_row_of_df(params, this_row)
    Rsen <- Rsen_and_Rres["Rsen"]
    Rres <- Rsen_and_Rres["Rres"]
    Rsen_vec <- c(Rsen_vec, Rsen)
    Rres_vec <- c(Rres_vec, Rres)
  }

  df$Rsen <- Rsen_vec
  df$Rres <- Rres_vec
  df
}

#-------------------------------------------------------------------------------
# Function Name: add_R0
#
# Description:
#   This function calculates the basic reproduction number (R0) for both sensitive and resistant 
#   strains of a disease using the `calculate_R0` function. It then appends these R0 values as 
#   new columns (`R0sen` and `R0res`) to the provided dataframe.
#
# Parameters:
#   params - A named vector or list containing the model parameters necessary for calculating the 
#            basic reproduction number (R0).
#   df - A dataframe to which the calculated R0 values will be added as new columns.
#
# Returns:
#   The input dataframe `df`, with two additional columns:
#   - R0sen: The basic reproduction number for the sensitive strain.
#   - R0res: The basic reproduction number for the resistant strain.
#
# Example of use:
#   df <- read.csv("path/to/data.csv")
#   df_with_R0 <- add_R0(params, df)
#   head(df_with_R0)
#
# Dependencies:
#   This function relies on the `calculate_R0` function to compute the R0 values. The `params` vector 
#   or list must contain all necessary parameters with the correct names.
#
#-------------------------------------------------------------------------------


add_R0 <- function(params, df) {
  R0sen_and_R0res <- calculate_R0(params)
  R0sen <- R0sen_and_R0res["R0sen"]
  R0res <- R0sen_and_R0res["R0res"]
  df$R0sen <- R0sen
  df$R0res <- R0res
  df
}


findGlobals(fun = R_calc_sen_or_res, merge = FALSE)$variables
findGlobals(fun = calculate_R0, merge = FALSE)$variables
findGlobals(fun = add_R0, merge = FALSE)$variables
# findGlobals(fun = add_R0, merge = FALSE)$variables
findGlobals(fun = calculate_R_from_row_of_df, merge = FALSE)$variables
findGlobals(fun = add_R_trajectories, merge = FALSE)$variables
