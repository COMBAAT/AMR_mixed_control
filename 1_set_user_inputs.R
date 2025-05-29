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
    general_descriptor = "May29",
    current_descriptor = "curative_longlasting"
  )
  user_inputs
}

create_multiple_scenarios <- function() {
  days_per_year <- set_days_per_year()
  max_time <- 5000
  treatment_type <- c("curative", "longlasting") # curative or longlasting
  cattle_number <- 100
  wildlife_number <- c(0, 100, 300)
  carrying_capacity <- c(10000, 6000, 2000)
  host_vector_ratio <- seq(10, 50, by = 20)
  treat_propA <- seq(0.0, 0.9, by = 0.05)
  treat_propB <- seq(0.91, 0.99, by = 0.04)
  treat_prop <- c(treat_propA, treat_propB)
  # do not set prop_cattle_with_insecticide to 1 as generates infinite mortality and an error
  prop_cattle_with_insecticide <- seq(0.0, 0.5, by = 0.025)
  maintain_vector_pop <- c(TRUE, FALSE) # whether to maintain vector population at carrying capacity or not)
  prop_prophylaxis_at_birth <- c(0.0)
  proph_ongoing <- 0 / days_per_year # c(0, 2, 4) / days_per_year
  fit_adj <- 0.8
  birth_adj <- 2.0
  dose_adj <- 1.0
  emergence <- 0.0
  partial_susceptibility_proph_cattle <- 0.5
  prob_death_from_disease <- 0.0
  
  # create grids of parameters combinations and then combine
  tb1 <- expand_grid(
    emergence = emergence,
    dose_adj = dose_adj, proph_ongoing = proph_ongoing,
    partial_susceptibility_proph_cattle = partial_susceptibility_proph_cattle,
    prob_death_from_disease = prob_death_from_disease,
    treat_prop = treat_prop, maintain_vector_pop = maintain_vector_pop,
    fit_adj = fit_adj, prop_cattle_with_insecticide = prop_cattle_with_insecticide,
    birth_adj = birth_adj, prop_prophylaxis_at_birth = prop_prophylaxis_at_birth,
    treatment_type = treatment_type, max_time = max_time
  )

  tb_hosts <- expand_grid(NC = cattle_number, NW = wildlife_number) %>% mutate(hosts = NC + NW)
  
  tb2a <- expand_grid(tb_hosts, K = carrying_capacity) %>%
    mutate(use_carrying_capacity = TRUE, host_vector_ratio = K / hosts) %>% 
    select(NC, NW, hosts, use_carrying_capacity, host_vector_ratio, K)
  
  tb2b <- expand_grid(tb_hosts, host_vector_ratio = host_vector_ratio) %>%
    mutate(use_carrying_capacity = FALSE, K = hosts * host_vector_ratio) %>% 
    select(NC, NW, hosts, use_carrying_capacity, host_vector_ratio, K)
  
  tb2 <- rbind(tb2a, tb2b)
  tb2

  tb <- expand_grid(tb2, tb1)

  df <- as.data.frame(tb) %>% mutate(treatment_type = as.factor(treatment_type))
  df
}


create_single_scenario <- function() {
  days_per_year <- set_days_per_year()
  max_time <- 3000
  treatment_type <- c("longlasting") # curative or longlasting
  cattle_number <- 100
  wildlife_number <- 100
  use_carrying_capacity <- TRUE # whether to use carrying capacity or host_vector_ratio
  carrying_capacity <- 6000
  host_vector_ratio <- 30
  treat_prop <- 0.1 + 0.1*runif(1) 
  # do not set prop_cattle_with_insecticide to 1 as generates infinite mortality and an error
  prop_cattle_with_insecticide <- 0.05
  maintain_vector_pop <- TRUE # whether to maintain vector population at carrying capacity or not)
  prop_prophylaxis_at_birth <- 0.0
  proph_ongoing <- 0 / days_per_year # c(0, 2, 4) / days_per_year
  fit_adj <- 0.8
  birth_adj <- 2.0
  dose_adj <- 1.0
  emergence <- 0.0
  partial_susceptibility_proph_cattle <- 0.5
  prob_death_from_disease <- 0.1
  
  # create grids of parameters combinations and then combine
  tb1 <- expand_grid(
    emergence = emergence,
    dose_adj = dose_adj, proph_ongoing = proph_ongoing,
    partial_susceptibility_proph_cattle = partial_susceptibility_proph_cattle,
    prob_death_from_disease = prob_death_from_disease,
    treat_prop = treat_prop, maintain_vector_pop = maintain_vector_pop,
    fit_adj = fit_adj, prop_cattle_with_insecticide = prop_cattle_with_insecticide,
    birth_adj = birth_adj, prop_prophylaxis_at_birth = prop_prophylaxis_at_birth,
    treatment_type = treatment_type, max_time = max_time
  )
  
  tb_hosts <- expand_grid(NC = cattle_number, NW = wildlife_number) %>% mutate(hosts = NC + NW)
  
  tb2a <- expand_grid(tb_hosts, K = carrying_capacity) %>%
    mutate(use_carrying_capacity = TRUE, host_vector_ratio = K / hosts) %>% 
    select(NC, NW, hosts, use_carrying_capacity, host_vector_ratio, K)
  
  tb2b <- expand_grid(tb_hosts, host_vector_ratio = host_vector_ratio) %>%
    mutate(use_carrying_capacity = FALSE, K = hosts * host_vector_ratio) %>% 
    select(NC, NW, hosts, use_carrying_capacity, host_vector_ratio, K)
  
  if (use_carrying_capacity == TRUE) {
    tb2 <- tb2a 
  } else {
    tb2 <- tb2b
  }
  
  tb <- expand_grid(tb2, tb1)
  
  df <- as.data.frame(tb) %>% mutate(treatment_type = as.factor(treatment_type))
  df
}




create_single_scenario_OLD <- function() {
  days_per_year <- set_days_per_year()
  max_time <- 5000
  treatment_type <- "curative" # quick or proph
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
