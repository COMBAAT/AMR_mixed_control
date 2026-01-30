
# =========================================================
# Function Names: append_epi_outputs_to_df, add_population_totals, calculate_epi_outputs
# Description: This script contains functions to manipulate and calculate epidemiological outputs
#              for a disease model. Functions include appending calculated metrics to data frames,
#              summing population totals for different categories, and calculating specific epidemiological outputs.
#              Metrics calculated include incidence, probability of onward transmission, and various risk assessments
#              based on the provided data.
#
# Parameters:
#   df - A dataframe containing initial epidemiological data.
#   treatment_type, params, final_state - Used in functions to specify treatment scenarios,
#                                         parameters for model calculations, and final states of the model.
#
# Returns:
#   Modified data frame with new columns for epidemiological metrics and population totals.
#
# Example of use:
#   data <- read.csv("path/to/data.csv")
#   data <- append_epi_outputs_to_df(data)
#   data <- add_population_totals(data)
#
# Dependencies: Requires the 'dplyr' package for data manipulation.
#
# Author: Shaun Keegan & Louise Matthews
# Date Created: August 2024
# Last Modified: August 2024
# =========================================================
library(codetools)
library(dplyr)


#-------------------------------------------------------------------------------
# Function Name: append_epi_outputs_to_df
#
# Parameters:
#   df - A dataframe containing initial epidemiological data.
#
# Outputs:
#   A modified dataframe with new columns for epidemiological metrics such as treatment categories,
#   incidence, probability of onward transmission, risk assessments, and prevalence.
#
# Dependencies:
#   dplyr, set_days_per_year()
#
#-------------------------------------------------------------------------------

append_epi_outputs_to_df <- function(df) {
  days_per_year <- set_days_per_year()

  df <- df %>% mutate(
    #No_trt_cat = (treatment_q * CIs_final + treatment_p * PIs_final) * days_per_year,
    No_trt_cat = (treatment_q * (CIs_final + PIs_final) + treatment_p * (CIs_final + PIs_final)) * days_per_year +
                  proph_ongoing * (All_cows_final - PPs_final - CTs_final - PTs_final) * days_per_year,
    Incidence = gamma_c * (PEs_final + CEs_final) * days_per_year,
    Prob_onward_tran = 1 - dpois(0, Rres_final),
    RiskA = PEs_final + PIs_final + PPs_final + CTs_final + PTs_final + CEXs_final + PEsX_final,
    RiskE = Prob_onward_tran * RiskA,
    prevalence = (PIs_final + CIs_final) / All_cows_final,
    prevalence_new = (PIs_final + PTs_final + PPs_final + CIs_final + CTs_final) / All_cows_final,
    Incidence_new = gamma_c * (PEs_final + CEs_final + CEXs_final + PEsX_final) * days_per_year,
    Incidence_C = gamma_c * (CEs_final + CEXs_final) * days_per_year,
    Incidence_P = gamma_c * (PEs_final + PEsX_final) * days_per_year,
    prevalence_wildlife = WIs_final / Wildlife_total_final, 
    prevalence_vectors = VIs_final / Vector_total_final,
    waning_from_PI =  waning_from_PI * PIs_final * days_per_year,
    Deaths_due_disease = death_dis * (CIs_final + PIs_final) * days_per_year,
    Deaths_due_disease2 = prob_death_from_disease * (1 - treat_prop) * Incidence_new
  )
  df
}

add_competition_and_invasion_columns <- function(df) {
  df <- df %>% 
    mutate(
    x1 = R0sen_final > 1,  # expect non-zero prevalence of sensitivte strain at equilibrium and Rsen = 1
    x2 = Rres_final > 1,   # Resistant strain is able to invade at equilibrium 
    x3 = Rres_final > Rsen_final) %>% # Resistant strain outcompetes sensitive strain
    mutate(Region = case_when(
      x1 & x3 ~ "Res outcompetes Sen",
      x1 & (!x3) ~ "Sen outcompetes Res",
      (!x1) & x2 ~ "No Sen & Res can invade",
      (!x1) & (!x2) ~ "No Sen & Res can't invade",
      TRUE ~ "NA"
    ))
}


#-------------------------------------------------------------------------------
# Function Name: add_population_totals
#
# Parameters:
#   df - A dataframe with epidemiological data needing population totals calculated.
#
# Outputs:
#   A modified dataframe with additional columns summing total population counts by category.
#
# Dependencies:
#   dplyr
#
#-------------------------------------------------------------------------------


add_population_totals <- function(df) {
  df_new <- df %>% mutate(
    Cattle_total = rowSums(select(., starts_with("C"))),
    Prophylactic_total = rowSums(select(., starts_with("P"))),
    Vector_total = rowSums(select(., starts_with("V"))),
    Wildlife_total = rowSums(select(., starts_with("W"))),
    All_cows = Cattle_total + Prophylactic_total
  )

  return(df_new)
}

findGlobals(fun = add_population_totals, merge = FALSE)$variables
findGlobals(fun = append_epi_outputs_to_df, merge = FALSE)$variables
