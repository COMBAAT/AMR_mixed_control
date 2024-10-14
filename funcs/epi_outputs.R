
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
    No_trt_cat = (treatment_q * CIs_final + treatment_p * PIs_final) * days_per_year,
    Incidence = gamma_c * (PEs_final + CEs_final) * days_per_year,
    Prob_onward_tran = 1 - dpois(0, Rres_final),
    RiskA = PEs_final + PIs_final + PPs_final + CTs_final + PTs_final + CEsX_final + PEsX_final,
    RiskE = Prob_onward_tran * RiskA,
    prevalence = (PIs_final + CIs_final) / All_cows_final
  )
  df
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
