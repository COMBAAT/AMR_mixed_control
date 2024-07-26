library(codetools)

set_days_per_year <- function() {
  days_per_year <- 365.25
  days_per_year
}

get_user_inputs <- function() {
  user_inputs <- list(
    multiple_scenarios = TRUE,
    use_root_functions = TRUE,
    append_current_time_to_output_file = FALSE,
    folder = "output/",
    general_descriptor = "simulation_set_",
    current_descriptor = "ORIGINAL_quick"
  )
  user_inputs
}

create_multiple_scenarios <- function() {
  max_time <- 10000
  treatment_type <- "quick" # quick or proph
  cattle_number <- 50
  #wildlife_number <- c(0, 50, 100, 150, 200, 250)
  wildlife_number <- c(0, 100, 250)
  treat_propA <- seq(0.0, 0.9, by = 0.2) 
  treat_propB <- seq(0.91, 0.99, by = 0.02)
  treat_prop <- c(treat_propA, treat_propB)
  carrying_capacity <- c(10000, 6000, 2000, 1000, 500)
  # do not set prop_cattle_with_insecticide to 1 as generates infinite mortality and an error
  prop_cattle_with_insecticide <- c(0.0, 0.025, 0.05, 0.10, 0.15, 0.2, 0.3, 0.4, 0.5)
  prop_prophylaxis_at_birth <- 0.9 #seq(0, 0.9, 0.1)
  proph_ongoing <- 0.0 #seq(0, 0.9, 0.1)
  fit_adj <- 0.95
  birth_adj <- 2.0
  dose_adj <- 1.0
  emergence <- 0.0
  rec_adj <- 1.0
  reversion <- 0.0

  df <- expand.grid(
    NC = cattle_number, reversion = reversion, rec_adj = rec_adj, emergence = emergence, 
    dose_adj = dose_adj, proph_ongoing = proph_ongoing,
    treat_prop = treat_prop, NW = wildlife_number, K = carrying_capacity,
    fit_adj = fit_adj, prop_cattle_with_insecticide = prop_cattle_with_insecticide,
    birth_adj = birth_adj, prop_prophylaxis_at_birth = prop_prophylaxis_at_birth,
    treatment_type = treatment_type, max_time = max_time
  )
  df
}

create_single_scenario <- function() {
  max_time <- 5000
  treatment_type <- "proph"
  cattle_number <- 50
  wildlife_number <- 250
  treat_prop <- 0.1
  carrying_capacity <- 2000
  prop_cattle_with_insecticide <- 0.0
  prop_prophylaxis_at_birth <- 0.0
  proph_ongoing <- 0.0 #seq(0, 0.9, 0.1)
  fit_adj <- 0.95
  birth_adj <- 2.0
  dose_adj <- 1.0
  emergence <- 0.0
  rec_adj <- 1.0
  reversion <- 0.0

  df <- expand.grid(
    NC = cattle_number, reversion = reversion, rec_adj = rec_adj, emergence = emergence, 
    dose_adj = dose_adj, proph_ongoing = proph_ongoing,
    treat_prop = treat_prop, NW = wildlife_number, K = carrying_capacity,
    fit_adj = fit_adj, prop_cattle_with_insecticide = prop_cattle_with_insecticide,
    birth_adj = birth_adj, prop_prophylaxis_at_birth = prop_prophylaxis_at_birth, 
    treatment_type = treatment_type, max_time = max_time 
  )
  df
}

findGlobals(fun = create_multiple_scenarios, merge = FALSE)$variables
findGlobals(fun = create_single_scenario, merge = FALSE)$variables
findGlobals(fun = set_days_per_year, merge = FALSE)$variables
findGlobals(fun = get_user_inputs, merge = FALSE)$variables
