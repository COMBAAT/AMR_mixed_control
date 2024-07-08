
create_multiple_scenarios <- function() {
  max_time <- 10000
  treatment_type <- "F" # F this means quick treatment
  N_wl <- c(0, 50, 100, 150, 200, 250)
  treat.prop_vecA <- seq(0, 0.9, by = 0.2) # full from 0-1
  treat.prop_vecB <- seq(0.91, 0.99, by = 0.02)
  treat.prop_vec <- c(treat.prop_vecA, treat.prop_vecB)
  K.vec <- c(10000, 6000, 2000, 1000, 500)
  fit_adj.vec <- c(0.95)
  birth_adj.vec <- c(2)
  # do not set prop_insecticide to 1 as generates infinite mortality and an error
  #prop_insecticide.vec <- c(0.0, 0.05, 0.10, 0.15, 0.2, 0.5)
   prop_insecticide.vec <- c(0.0, 0.025, 0.05, 0.10, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5)
  prop_prophylaxis <- 0.0
  dose_adj.vec <- 1 # c(1, 0.9, 0.7, 0.4, 0.2)
  emergence_adj.vec <- 1

  df <- expand.grid(
    emergence_adj = emergence_adj.vec, dose_adj = dose_adj.vec,
    treat_prop = treat.prop_vec, NW = N_wl, K = K.vec,
    fit_adj = fit_adj.vec, prop_insecticide = prop_insecticide.vec,
    birth_adj = birth_adj.vec, prop_prophylaxis = prop_prophylaxis,
    treatment_type = treatment_type, max_time = max_time
  )
  df
}

create_single_scenario <- function() {
  max_time <- 5000
  treatment_type <- "F"
  N_wl <- 250
  treat.prop_vec <- 0.25
  fit_adj.vec <- c(0.95)
  K.vec <- 2000
  prop_insecticide.vec <- 0.0
  birth_adj.vec <- 2
  prop_prophylaxis <- 0.0
  dose_adj.vec <- 1.0
  emergence_adj.vec <- 1

  df <- data.frame(
    emergence_adj = emergence_adj.vec, dose_adj = dose_adj.vec,
    treat_prop = treat.prop_vec, NW = N_wl, K = K.vec,
    fit_adj = fit_adj.vec, prop_insecticide = prop_insecticide.vec,
    birth_adj = birth_adj.vec, prop_prophylaxis = prop_prophylaxis,
    treatment_type = treatment_type, max_time = max_time
  )
  df
}
