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
    general_descriptor = "Jan23_",
    current_descriptor = "proph_quick"
  )
  user_inputs
}

create_multiple_scenarios <- function() {
  days_per_year <- set_days_per_year()
  max_time <- 10000
  treatment_type <- c("proph", "quick") # quick, proph or both
  cattle_number <- 100
  wildlife_number <- c(0, 100, 300)
  treat_propA <- seq(0.0, 0.9, by = 0.2)
  treat_propB <- seq(0.91, 0.99, by = 0.02)
  treat_prop <- c(treat_propA, treat_propB)
  maintain_vector_pop <- TRUE
  # do not set prop_cattle_with_insecticide to 1 as generates infinite mortality and an error
  prop_cattle_with_insecticide <- seq(0.0, 0.5, by = 0.05)
  prop_prophylaxis_at_birth <- c(0.0) 
  proph_ongoing <- 0 #c(0, 2, 4) / days_per_year
  fit_adj <- 0.8
  birth_adj <- 2.0
  dose_adj <- 1.0
  emergence <- 0.0
  partial_susceptibility_proph_cattle <- 0.5
  
  # choose whether to specify carrying capacity directly or via host vector ratio
  use_carrying_capacity <- TRUE
  carrying_capacity <- c(10000, 6000, 4000, 2000)
  host_vector_ratio <- seq(5, 50, by = 5)
  if (use_carrying_capacity == TRUE) {
    tb1a <- expand_grid(NC = cattle_number, NW = wildlife_number, K = carrying_capacity) %>% 
      mutate(hosts = NC + NW, host_vector_ratio = K / hosts)
    tb1a$use_carrying_capacity <- use_carrying_capacity
    tb1a
    tb1 <- tb1a
  } else {
    tb1b <- expand_grid(NC = cattle_number, NW = wildlife_number, host_vector_ratio = host_vector_ratio) %>% 
      mutate(hosts = NC + NW)
    tb1b$K <- tb1b$hosts * tb1b$host_vector_ratio
    tb1b$use_carrying_capacity <- use_carrying_capacity
    tb1b
    tb1 <- tb1b
  }

  #tb1 <- expand_grid(NW = wildlife_number, K = carrying_capacity)
  
  tb2 <- expand_grid(
    emergence = emergence,
    dose_adj = dose_adj, proph_ongoing = proph_ongoing, 
    partial_susceptibility_proph_cattle = partial_susceptibility_proph_cattle,
    treat_prop = treat_prop, maintain_vector_pop = maintain_vector_pop,
    fit_adj = fit_adj, prop_cattle_with_insecticide = prop_cattle_with_insecticide,
    birth_adj = birth_adj, prop_prophylaxis_at_birth = prop_prophylaxis_at_birth,
    treatment_type = treatment_type, max_time = max_time
  )
  tb <- expand_grid(tb1, tb2)
  
  df <- as.data.frame(tb) %>% mutate(treatment_type = as.factor(treatment_type))
  df
}

create_single_scenario <- function() {
  days_per_year <- set_days_per_year()
  max_time <- 5000
  treatment_type <- "quick" # quick or proph
  cattle_number <- 100
  wildlife_number <- 250
  treat_prop <- 0.5
  carrying_capacity <- 10000
  maintain_vector_pop <- TRUE
  prop_cattle_with_insecticide <- 0.0
  prop_prophylaxis_at_birth <- 0.1 
  proph_ongoing <- 0 / days_per_year
  fit_adj <- 0.95
  birth_adj <- 2.0
  dose_adj <- 1.0
  emergence <- 0.0
  partial_susceptibility_proph_cattle <- 0.5

  df <- expand.grid(
    NC = cattle_number, emergence = emergence,
    dose_adj = dose_adj, proph_ongoing = proph_ongoing, partial_susceptibility_proph_cattle = partial_susceptibility_proph_cattle,
    treat_prop = treat_prop, NW = wildlife_number, K = carrying_capacity, maintain_vector_pop = maintain_vector_pop,
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
