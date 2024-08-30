
# =========================================================
# Function Names: get_baseline_parameters, calculate_vector_death_rate, set_parameters_NEW
# Description: This script provides functions to define and retrieve baseline parameters for an epidemiological model.
#              It includes setting up essential parameters such as lifespans, infection periods, and protection periods,
#              as well as calculating vector death rates and other specific parameters for different species involved in the model.
#
# Parameters:
#   None explicitly required for input; parameters are set within the functions.
#
# Returns:
#   A list or vector of baseline parameters, depending on the function called.
#
# Example of use:
#   baseline_params <- get_baseline_parameters()
#   vector_death_rate <- calculate_vector_death_rate(params)
#   new_params <- set_parameters_NEW(some_input)
#
# Dependencies: Requires the 'codetools' package for managing code properties.
#
# Author: Shaun Keegan & Louise Matthews
# Date Created: August 2024
# Last Modified: August 2024
# =========================================================
## --------------------- Params

library(codetools)

## Set parameters

#-------------------------------------------------------------------------------
# Function Name: get_baseline_parameters
#
# Description:
#   This function generates and returns a set of baseline parameters for an epidemiological model. 
#   These parameters include various biological and epidemiological metrics such as lifespan, 
#   incubation periods, infection periods, and probabilities related to disease transmission. 
#   The parameters are relevant for both cattle and wildlife populations, as well as vectors 
#   (e.g., insects) involved in disease transmission.
#
# Parameters:
#   None.
#
# Returns:
#   A named numeric vector containing baseline parameters, including:
#   - cattle_lifespan: Lifespan of cattle in days.
#   - cattle_incubation_period: Incubation period for the disease in cattle (days).
#   - cattle_infection_period: Duration of infection in cattle (days).
#   - cattle_proph_full_protection_period: Duration of full protection from prophylaxis in cattle (days).
#   - cattle_proph_partial_protection_period: Duration of partial protection from prophylaxis in cattle (days).
#   - cattle_treatment_period: Duration of treatment period in cattle (days).
#   - wildlife_lifespan: Lifespan of wildlife in days.
#   - wildlife_infection_period: Duration of infection in wildlife (days).
#   - wildlife_incubation_period: Incubation period for the disease in wildlife (days).
#   - prob_infection_to_host: Probability of infection being transmitted to a host.
#   - prob_infection_to_vector: Probability of infection being transmitted to a vector.
#   - vector_teneral_period: Teneral period of vectors (days).
#   - days_between_feeds: Average number of days between vector feeding events.
#   - prob_vector_surviving_feed: Probability of a vector surviving a feeding event.
#   - prob_vector_surviving_nonfeeding_day: Probability of a vector surviving a non-feeding day.
#   - vector_incubation_period: Incubation period for the disease in vectors (days).
#   - biterate_scaling: Scaling factor for the bite rate of vectors.
#
# Example of use:
#   baseline_params <- get_baseline_parameters()
#   print(baseline_params)
#
# Dependencies:
#   - set_days_per_year: To determine the number of days in a year, used for calculating various time-based parameters.
#   - convert_array_to_named_vector: To convert the baseline parameters from an array to a named vector.
#
#-------------------------------------------------------------------------------



get_baseline_parameters <- function() {
  days_per_year <- set_days_per_year()

  # Using day as unit of time
  cattle_lifespan <- 5 * days_per_year # 1000 for Hargrove
  cattle_incubation_period <- 15 # 0.0001 for Hargrove
  cattle_infection_period <- 100
  cattle_proph_full_protection_period <- 60
  cattle_proph_partial_protection_period <- 30
  cattle_treatment_period <- 3 # 0.1 for Hargrove

  wildlife_lifespan <- 1 * days_per_year
  wildlife_infection_period <- cattle_infection_period
  wildlife_incubation_period <- 20

  biterate_scaling <- 0.7 # as per Hargrove
  prob_infection_to_host <- 0.46
  prob_infection_to_vector <- 0.025
  vector_teneral_period <- 4
  days_between_feeds <- 4
  prob_vector_surviving_feed <- 0.96
  prob_vector_surviving_nonfeeding_day <- 0.98
  vector_incubation_period <- 20

  baseline_params <- cbind(
    cattle_lifespan, cattle_incubation_period, cattle_infection_period,
    cattle_proph_full_protection_period, cattle_proph_partial_protection_period,
    cattle_treatment_period, wildlife_lifespan, wildlife_infection_period,
    wildlife_incubation_period, prob_infection_to_host, prob_infection_to_vector,
    vector_teneral_period, days_between_feeds, prob_vector_surviving_feed,
    prob_vector_surviving_nonfeeding_day, vector_incubation_period, biterate_scaling
  )
  baseline_params <- convert_array_to_named_vector(baseline_params)
  baseline_params
}



#-------------------------------------------------------------------------------
# Function Name: calculate_vector_death_rate
#
# Description:
#   This function calculates the daily mortality rate of vectors (e.g., insects) using parameters 
#   from the Hargrove et al. (2012) model. The calculation is based on the probability of a vector 
#   surviving a feeding event and the probability of surviving a non-feeding day, adjusted by the 
#   proportion of animals that are insecticide-treated.
#
# Parameters:
#   d  - A numeric value representing the length of the feeding cycle (in days).
#   qf - A numeric value representing the probability of a vector surviving a feeding event.
#   qn - A numeric value representing the probability of a vector surviving a non-feeding day.
#   pi - A numeric value representing the proportion of animals that are insecticide-treated.
#
# Returns:
#   A numeric value representing the calculated daily mortality rate of vectors.
#
# Example of use:
#   daily_mortality <- calculate_vector_death_rate(d = 4, qf = 0.96, qn = 0.98, pi = 0.3)
#   print(daily_mortality)
#
# Dependencies:
#   This function does not have any external dependencies.
#
#-------------------------------------------------------------------------------


calculate_vector_death_rate <- function(d, qf, qn, pi) {
  # From Hargrove et al 2012, using exp( - daily_mortality * d) = qf * qn ^ d
  # pi is proportion of animals that are insecticide treated
  # qf is probability of a vector surviving a feed
  # qn is probability of vector surviving a non-feeding day
  # d is the length of the feeding cycle
  daily_mortality <- -log((1 - pi) * qf * qn^d) / d
  daily_mortality
}

#-------------------------------------------------------------------------------
# Function Name: set_parameters_NEW
#
# Description:
#   This function sets and calculates various parameters necessary for an epidemiological model based on 
#   a given scenario. It incorporates parameters for cattle, wildlife, and vector populations, taking into 
#   account the effects of different treatment types, prophylaxis, and other scenario-specific adjustments.
#   The function returns a comprehensive list of parameters that are used in the disease model.
#
# Parameters:
#   this_scenario - A dataframe row or list containing scenario-specific parameters, including:
#       - birth_adj: Adjustment factor for vector birth rate.
#       - fit_adj: Fitness adjustment factor.
#       - K: Carrying capacity.
#       - treat_prop: Proportion of the population receiving treatment.
#       - prop_cattle_with_insecticide: Proportion of cattle treated with insecticide.
#       - NW: Population size of wildlife.
#       - NC: Population size of cattle.
#       - prop_prophylaxis_at_birth: Proportion of animals receiving prophylaxis at birth.
#       - proph_ongoing: Proportion of ongoing prophylaxis.
#       - treatment_type: Type of treatment (e.g., "quick", "proph", "both").
#       - dose_adj: Dose adjustment factor.
#       - emergence: Emergence rate of resistance.
#       - rec_adj: Recovery adjustment factor.
#       - reversion: Rate of reversion to susceptibility.
#       - option: Any additional scenario-specific options.
#
# Returns:
#   A named numeric vector containing all parameters required for the epidemiological model, including:
#   - Parameters related to cattle (e.g., birth_c, death_c, gamma_c, sigma_c).
#   - Parameters related to wildlife (e.g., birth_w, death_w, gamma_w, sigma_w).
#   - Parameters related to vectors (e.g., birth_v, death_v, gamma_v, ten2fed).
#   - Equilibrium values (e.g., PF, PS, CS, NV).
#   - Treatment-related parameters (e.g., treatment_p, treatment_q, sigma_st).
#   - Scenario-specific adjustments.
#
# Example of use:
#   scenario_params <- set_parameters_NEW(this_scenario)
#   print(scenario_params)
#
# Dependencies:
#   - get_baseline_parameters: To retrieve baseline epidemiological parameters.
#   - get_disease_free_equilibrium_for_PF_PS_and_CS: To calculate equilibrium values for disease-free populations.
#   - calculate_vector_death_rate: To calculate the death rate of vectors based on feeding cycle and insecticide treatment.
#   - convert_array_to_named_vector: To convert arrays of parameters into named vectors.
#   - convert_df_row_to_named_vector: To convert scenario data into a named vector.
#   - qual_check_no0: To ensure there are no negative values in the parameters.
#
#-------------------------------------------------------------------------------



set_parameters_NEW <- function(this_scenario) {
  birth_adj <- this_scenario$birth_adj
  fit_adj <- this_scenario$fit_adj
  K <- this_scenario$K
  treat_prop <- this_scenario$treat_prop
  prop_cattle_with_insecticide <- this_scenario$prop_cattle_with_insecticide
  NW <- this_scenario$NW
  NC <- this_scenario$NC
  prop_prophylaxis_at_birth <- this_scenario$prop_prophylaxis_at_birth
  proph_ongoing <- this_scenario$proph_ongoing
  treatment_type <- this_scenario$treatment_type
  dose_adj <- this_scenario$dose_adj
  emergence <- this_scenario$emergence
  rec_adj <- this_scenario$rec_adj
  reversion <- this_scenario$reversion
  option <- this_scenario$option

  baseline_params <- get_baseline_parameters()

  ## Cattle -----
  birth_c <- 1 / baseline_params["cattle_lifespan"]
  gamma_c <- 1 / baseline_params["cattle_incubation_period"]
  sigma_c <- 1 / baseline_params["cattle_infection_period"]
  death_c <- birth_c
  death_p <- death_c


  treatment <- treat_prop * (sigma_c + death_c) / (1 - treat_prop)

  if (treatment_type == "quick") {
    treatment_q <- treatment
    treatment_p <- 0
    emergence_p <- 0
    emergence_q <- emergence
  }
  if (treatment_type == "proph") {
    treatment_q <- 0
    treatment_p <- treatment
    emergence_p <- emergence
    emergence_q <- 0
  }
  if (treatment_type == "both") {
    treatment_q <- treatment
    treatment_p <- treatment
    emergence_p <- emergence
    emergence_q <- emergence
  }

  sigma_st_full_dose <- (1 / baseline_params["cattle_treatment_period"])
  sigma_st <- sigma_st_full_dose * dose_adj + sigma_c * (1 - dose_adj)
  waning <- 1 / (baseline_params["cattle_proph_partial_protection_period"] * dose_adj)
  waning_f2s <- 1 / (baseline_params["cattle_proph_full_protection_period"] * dose_adj)


  equilibrium_values <- get_disease_free_equilibrium_for_PF_PS_and_CS(
    birth_c, prop_prophylaxis_at_birth, NC, death_c,
    waning_f2s, death_p, waning, proph_ongoing
  )

  PF <- equilibrium_values["PF"]
  PS <- equilibrium_values["PS"]
  CS <- equilibrium_values["CS"]

  ## ----- Wildlife
  birth_w <- 1 / baseline_params["wildlife_lifespan"]
  gamma_w <- 1 / baseline_params["wildlife_incubation_period"]
  sigma_w <- 1 / baseline_params["wildlife_infection_period"]
  death_w <- birth_w

  ## -----  Vectors
  biterate <- baseline_params["biterate_scaling"] / baseline_params["days_between_feeds"]
  ten2fed <- 1 / baseline_params["vector_teneral_period"]
  prop_hosts_with_insecticide <- prop_cattle_with_insecticide * NC / (NC + NW) # Proportion of insecticide adjusted for wildlife

  death_v <- calculate_vector_death_rate(
    d = baseline_params["days_between_feeds"],
    qf = baseline_params["prob_vector_surviving_feed"],
    qn = baseline_params["prob_vector_surviving_nonfeeding_day"],
    pi = prop_hosts_with_insecticide
  )
  death_v_no_insecticide <- calculate_vector_death_rate(
    d = baseline_params["days_between_feeds"],
    qf = baseline_params["prob_vector_surviving_feed"],
    qn = baseline_params["prob_vector_surviving_nonfeeding_day"],
    pi = 0.0
  )

  # death_v <- death_v_no_insecticide + baseline_params["biterate"] * prop_hosts_with_insecticide   # not best way if pi is close to 1
  #death_v_no_insecticide <- 0.03 # use for Hargrove
  #death_v <- 0.03 # use for Hargrove
  incubation <-  baseline_params["vector_incubation_period"]
  gamma_v <- death_v_no_insecticide * exp(-death_v_no_insecticide * incubation) / (1 - exp(-death_v_no_insecticide * incubation))  # fixed original formulation

  birth_v <- birth_adj * death_v_no_insecticide
  equil_vector_pop <- max(0, K * (1 - death_v / birth_v))
  NV <- equil_vector_pop
  VSt <- NV * death_v_no_insecticide / (death_v_no_insecticide + ten2fed)
  VSf <- NV - VSt

  ## ----- Parameters output
  derived_params <- cbind(
    biterate,
    NV, PF, PS, CS, VSt, VSf, equil_vector_pop,
    birth_c, death_c, gamma_c, sigma_c,
    birth_w, death_w, gamma_w, sigma_w,
    birth_v, death_v, gamma_v, ten2fed,
    treatment_p, treatment_q, sigma_st,
    emergence_p, emergence_q, waning, waning_f2s
  )
  derived_params <- convert_array_to_named_vector(derived_params)

  scenario_params <- convert_df_row_to_named_vector(this_scenario)
  all_params <- c(derived_params, scenario_params, baseline_params)
  qual_check_no0(all_params)

  return(all_params)
}


findGlobals(fun = get_baseline_parameters, merge = FALSE)$variables
findGlobals(fun = calculate_vector_death_rate, merge = FALSE)$variables
findGlobals(fun = set_parameters_NEW, merge = FALSE)$variables
