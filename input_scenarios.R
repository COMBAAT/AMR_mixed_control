
create_multiple_scenarios <- function() {
  max_time <- 5
  treatment_type <- "quick"
  #wildlife_number <- c(0, 50, 100, 150, 200, 250)
  wildlife_number <- c(0, 100, 250)
  treat_propA <- seq(0, 0.9, by = 0.2) # full from 0-1
  treat_propB <- seq(0.91, 0.99, by = 0.02)
  treat_prop <- c(treat_propA, treat_propB)
  carrying_capacity <- c(10000, 6000, 2000, 1000, 500)
  # do not set prop_insecticide to 1 as generates infinite mortality and an error
  prop_insecticide <- c(0.0, 0.05, 0.10, 0.15, 0.2, 0.5)
  #prop_insecticide <- c(0.0, 0.025, 0.05, 0.10, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5)
  prop_prophylaxis <- 0.0
  fit_adj <- c(0.95)
  birth_adj <- c(2)
  dose_adj <- 1 # c(1, 0.9, 0.7, 0.4, 0.2)
  emergence_adj <- 1

  df <- expand.grid(
    emergence_adj = emergence_adj, dose_adj = dose_adj,
    treat_prop = treat_prop, NW = wildlife_number, K = carrying_capacity,
    fit_adj = fit_adj, prop_insecticide = prop_insecticide,
    birth_adj = birth_adj, prop_prophylaxis = prop_prophylaxis,
    treatment_type = treatment_type, max_time = max_time
  )
  df
}

create_single_scenario <- function() {
  max_time <- 5000
  treatment_type <- "quick"
  wildlife_number <- 250
  treat_prop <- 0.25
  carrying_capacity <- 2000
  prop_insecticide <- 0.0
  prop_prophylaxis <- 0.0
  fit_adj <- c(0.95)
  birth_adj <- 2
  dose_adj <- 1.0
  emergence_adj <- 1

  df <- data.frame(
    emergence_adj = emergence_adj, dose_adj = dose_adj,
    treat_prop = treat_prop, NW = wildlife_number, K = carrying_capacity,
    fit_adj = fit_adj, prop_insecticide = prop_insecticide,
    birth_adj = birth_adj, prop_prophylaxis = prop_prophylaxis,
    treatment_type = treatment_type, max_time = max_time
  )
  df
}
