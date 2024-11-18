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

## --------------------- R0



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
  NH <- params["NH"]

  biterate <- params["biterate"]
  prob_infection_to_host <- params["prob_infection_to_host"]
  partial_susceptibility_proph_cattle <- params["partial_susceptibility_proph_cattle"]
  prob_infection_to_vector <- params["prob_infection_to_vector"]
  fit_adj <- params["fit_adj"]
  sigma_c <- params["sigma_c"]
  sigma_st <- params["sigma_st"]
  gamma_w <- params["gamma_w"]
  death_w <- params["death_w"]
  sigma_w <- params["sigma_w"]
  gamma_v <- params["gamma_v"]
  death_v <- params["death_v"]

  if (is_strain_sensitive == "yes") {
    sigma_treated <- sigma_st
  }
  if (is_strain_sensitive == "no") {
    sigma_treated <- sigma_c
  }
  if (is_strain_sensitive == "no") {
    prob_infection_to_host <- prob_infection_to_host * fit_adj
  }

  rate_vectors_infected <- biterate * prob_infection_to_vector * Nv / NH * gamma_v / (gamma_v + death_v)

  #transition_probabilities <- create_named_vector_of_transition_probabilities(params, is_strain_sensitive)
  transition_probabilities <- create_named_vector_of_all_transition_probabilities(params, is_strain_sensitive)
  time_in_state <- create_named_vector_of_times_in_state(params, is_strain_sensitive)

  # transmission via C - cattle with no prophylaxis
  # from exposed host to infected vector
  RVC <- calculate_RVC(rate_vectors_infected, time_in_state, transition_probabilities)
  RVC <- as.numeric(RVC)

  # from infected vector to exposed host
  RCV <- biterate * prob_infection_to_host * (Nc / NH) * 1 / (death_v)
  RCV <- as.numeric(RCV)

  # transmission via P - cattle with prophylaxis
  # from exposed P to infected vector
  RVP <- calculate_RVP(rate_vectors_infected, time_in_state, transition_probabilities)
  RVP <- as.numeric(RVP)

  # from infected vector to exposed P
  if (is_strain_sensitive == "yes") {
    RPV <- biterate * partial_susceptibility_proph_cattle * prob_infection_to_host * Nps / NH * 1 / (death_v)
  }
  if (is_strain_sensitive == "no") {
    RPV <- biterate * prob_infection_to_host * ((Nps + Npf) / NH) * 1 / (death_v)
  }
  RPV <- as.numeric(RPV)

  # transmission via W
  # from infected wildlife to infected vector
  RVW <- biterate * prob_infection_to_vector * Nv / NH * 1 / (sigma_w + death_w) * gamma_v / (gamma_v + death_v)
  RVW <- as.numeric(RVW)

  # from infected vector to infected wildlife
  RWV <- biterate * prob_infection_to_host * Nw / NH * gamma_w / (gamma_w + death_w) * 1 / (death_v)
  RWV <- as.numeric(RWV)

  reproduction_number <- RCV * RVC + RPV * RVP + RWV * RVW
  reproduction_number
}

#################################################################################
create_named_vector_of_all_transition_probabilities <- function(params, is_strain_sensitive) {
  transition_probs1 <- create_named_vector_of_transition_probabilities(params, is_strain_sensitive)
  #transition_probs2 <- calculate_loop_probabilities(params, is_strain_sensitive)
  transition_probs2 <- calculate_loop_probabilities_alt(transition_probs1, is_strain_sensitive)
  transition_probs <- c(transition_probs1, transition_probs2)
  transition_probs
}

create_named_vector_of_transition_probabilities <- function(params, is_strain_sensitive) {
  transition_probabilities <- with(as.list(params, is_strain_sensitive), {
    if (is_strain_sensitive == "yes") {
      sigma_treated <- sigma_st
    }
    if (is_strain_sensitive == "no") {
      sigma_treated <- sigma_c
    }
    
    #loop_probabilities <- calculate_loop_probabilities(params, is_strain_sensitive)
    #p1c <- loop_probabilities$p1c
    #p2c <- loop_probabilities$p2c
    
    prob_CI_from_CE <- gamma_c / (gamma_c + death_c + proph_ongoing)
    prob_PI_from_PE <- gamma_c / (gamma_c + death_c + proph_ongoing + waning_from_partial_protection)
    prob_CI_treat_q <- treatment_q / (treatment_p + treatment_q + sigma_c + death_c + proph_ongoing)
    prob_CI_treat_p <- treatment_p / (treatment_p + treatment_q + sigma_c + death_c + proph_ongoing)
    prob_PI_treat_q <- treatment_q / (treatment_p + treatment_q + sigma_c + death_c + waning_from_partial_protection + proph_ongoing)
    prob_PI_treat_p <- treatment_p / (treatment_p + treatment_q + sigma_c + death_c + waning_from_partial_protection + proph_ongoing)
    prob_waning_from_partial_protection_from_PE <- waning_from_partial_protection / (gamma_c + death_c + waning_from_partial_protection + proph_ongoing)
    prob_waning_from_partial_protection_from_PI <- waning_from_partial_protection / (treatment_p + treatment_q + sigma_c + death_c + waning_from_partial_protection + proph_ongoing)
    prob_waning_from_partial_protection_from_PT <- waning_from_partial_protection / (sigma_treated + death_c + waning_from_partial_protection)
    prob_waning_from_partial_protection_from_PP <- waning_from_partial_protection / (sigma_treated + death_c + waning_from_partial_protection)
    prob_proph_from_CI <- proph_ongoing / (treatment_p + treatment_q + sigma_c + death_c + proph_ongoing)
    prob_proph_from_PI <- proph_ongoing / (treatment_p + treatment_q + sigma_c + death_c + waning_from_partial_protection + proph_ongoing)
    prob_proph_from_CE <- proph_ongoing / (gamma_c + death_c + proph_ongoing)
    prob_proph_from_PE <- proph_ongoing / (gamma_c + death_c + proph_ongoing + waning_from_partial_protection)
    prob_disease_from_CEX <- gamma_c / (gamma_c + death_c + sigma_treated)
    prob_disease_from_PEX <- gamma_c / (gamma_c + death_c + sigma_treated)
    
    probs <- cbind(
      #p1c, p2c,
      prob_CI_from_CE, prob_PI_from_PE, prob_CI_treat_q, prob_CI_treat_p, prob_PI_treat_q, prob_PI_treat_p,
      prob_waning_from_partial_protection_from_PE, prob_waning_from_partial_protection_from_PI,
      prob_waning_from_partial_protection_from_PT, prob_waning_from_partial_protection_from_PP,
      prob_proph_from_CI, prob_proph_from_PI, prob_proph_from_CE, prob_proph_from_PE, prob_disease_from_CEX,
      prob_disease_from_PEX
    )
    probs <- convert_array_to_named_vector(probs)
    probs
  })
  
  transition_probabilities
}


create_named_vector_of_times_in_state <- function(params, is_strain_sensitive) {
  times_in_state <- with(as.list(params), {
    if (is_strain_sensitive == "yes") {
      sigma_treated <- sigma_st
    }
    if (is_strain_sensitive == "no") {
      sigma_treated <- sigma_c
    }
    
    time_in_CI <- 1 / (treatment_p + treatment_q + sigma_c + death_c + proph_ongoing)
    time_in_CT <- 1 / (sigma_treated + death_c)
    time_in_PI <- 1 / (treatment_p + treatment_q + sigma_c + death_c + waning_from_partial_protection + proph_ongoing)
    time_in_PT <- 1 / (sigma_treated + death_c + waning_from_partial_protection)
    time_in_PP <- 1 / (sigma_treated + death_c + waning_from_partial_protection)
    times <- cbind(time_in_CI, time_in_CT, time_in_PI, time_in_PT, time_in_PP)
    times <- convert_array_to_named_vector(times)
    times
  })
  
  return(times_in_state)
}

#################################################################################


#################################################################################

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
findGlobals(fun = create_named_vector_of_transition_probabilities, merge = FALSE)$variables
findGlobals(fun = create_named_vector_of_times_in_state, merge = FALSE)$variables
