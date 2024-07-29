## --------------------- Params

library(codetools)

## Set parameters

get_baseline_parameters <- function() {
  
  days_per_year <- set_days_per_year()
  
  # Using day as unit of time
  cattle_lifespan <- 5 * days_per_year
  cattle_incubation_period <- 15
  cattle_infection_period <- 100
  cattle_proph_full_protection_period <- 60
  cattle_proph_partial_protection_period <- 30
  cattle_treatment_period <- 3

  wildlife_lifespan <- 1 * days_per_year
  wildlife_infection_period <- cattle_infection_period
  wildlife_incubation_period <- 20

  #biterate <- 0.8 / 4
  prob_infection_to_host <- 0.46
  prob_infection_to_vector <- 0.025
  vector_teneral_period <- 4
  days_between_feeds <- 4
  prob_vector_surviving_feed <- 0.96
  prob_vector_surviving_nonfeeding_day <- 0.98
  vector_incubation_period <- 20
  
  baseline_params <- cbind(cattle_lifespan, cattle_incubation_period, cattle_infection_period,
                           cattle_proph_full_protection_period, cattle_proph_partial_protection_period,
                           cattle_treatment_period, wildlife_lifespan, wildlife_infection_period,
                           wildlife_incubation_period, prob_infection_to_host, prob_infection_to_vector,
                           vector_teneral_period, days_between_feeds, prob_vector_surviving_feed, 
                           prob_vector_surviving_nonfeeding_day, vector_incubation_period
    
  )
  baseline_params <- convert_array_to_named_vector(baseline_params)
  baseline_params
}


calculate_vector_death_rate <- function(d, qf, qn, pi) {
  # From Hargrove et al 2012, using exp( - daily_mortality * d) = qf * qn ^ d
  # pi is proportion of animals that are insecticide treated
  # qf is probability of a vector surviving a feed
  # qn is probability of vector surviving a non-feeding day
  # d is the length of the feeding cycle
  daily_mortality <- -log( (1 - pi) * qf * qn ^ d) / d
  daily_mortality
}


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
  
  
  equilibrium_values <- get_disease_free_equilibrium_for_PF_PS_and_CS(birth_c, prop_prophylaxis_at_birth, NC, death_c, 
                            waning_f2s, death_p, waning, proph_ongoing) 

  PF <- equilibrium_values["PF"]
  PS <- equilibrium_values["PS"]
  CS <- equilibrium_values["CS"]
  
  ## ----- Wildlife
  birth_w <- 1 / baseline_params["wildlife_lifespan"]
  gamma_w <- 1 / baseline_params["wildlife_incubation_period"]
  sigma_w <- 1 / baseline_params["wildlife_infection_period"]
  death_w <- birth_w
  
  ## -----  Vectors
  biterate <- 1 / baseline_params["days_between_feeds"]
  ten2fed <- 1 / baseline_params["vector_teneral_period"]
  prop_hosts_with_insecticide <- prop_cattle_with_insecticide * NC / (NC + NW) # Proportion of insecticide adjusted for wildlife
  
  death_v <- calculate_vector_death_rate(d = baseline_params["days_between_feeds"],
                                         qf = baseline_params["prob_vector_surviving_feed"],
                                         qn = baseline_params["prob_vector_surviving_nonfeeding_day"],
                                         pi = prop_hosts_with_insecticide)
  death_v_no_insecticide <- calculate_vector_death_rate(d = baseline_params["days_between_feeds"],
                                                        qf = baseline_params["prob_vector_surviving_feed"],
                                                        qn = baseline_params["prob_vector_surviving_nonfeeding_day"],
                                                        pi = 0.0)
  
  # death_v <- death_v_no_insecticide + baseline_params["biterate"] * prop_hosts_with_insecticide   # not best way if pi is close to 1
  # incubation <- 20 # original formulation but incorrect
  # gamma_v <- death_v * exp(-death_v * incubation) / (1 - exp(-death_v * incubation))  # original formulation but incorrect
  # biterate <- 0.8/4   # original formulation but incorrect
  gamma_v <- 1 / baseline_params["vector_incubation_period"] # correct way
  
  birth_v <- birth_adj * death_v_no_insecticide 
  equil_vector_pop <- max(0, K * (1 - death_v / birth_v)) 
  NV <- equil_vector_pop 
  
  ## ----- Parameters output
  derived_params <- cbind(
    biterate,
    NV, PF, PS, CS, equil_vector_pop,
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

