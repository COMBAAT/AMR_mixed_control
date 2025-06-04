library(codetools)

set_days_per_year <- function() {
  days_per_year <- 365.25
  days_per_year
}

get_user_inputs <- function() {
  user_inputs <- list(
    multiple_scenarios = TRUE,
    use_root_functions = FALSE,
    append_current_time_to_output_file = FALSE,
    folder = "output/",
    general_descriptor = "June4",
    current_descriptor = "curative_longlasting"
  )
  user_inputs
}

create_multiple_scenarios <- function() {
  max_time <- 2500
  treatment_type <- c("curative", "longlasting") # curative or longlasting
  cattle_number <- 100
  wildlife_number <- c(0, 100, 300)
  K_host_ratio <- 30 #seq(10, 50, by = 20)      # carrying capacity per host
  treat_propA <- seq(0.0, 0.9, by = 0.2)
  treat_propB <- seq(0.91, 0.99, by = 0.04)
  treat_prop <- c(treat_propA, treat_propB) # treatment proportion of cattle with trypanocides
  # do not set prop_cattle_with_insecticide to 1
  prop_cattle_with_insecticide <- 0.0 #seq(0.0, 0.5, by = 0.1)
  
  days_per_year <- set_days_per_year()
  maintain_vector_pop <- c(FALSE) # whether to maintain vector population at carrying capacity or not
  prop_prophylaxis_at_birth <- c(0.0)
  proph_ongoing <- 0 / days_per_year # c(0, 2, 4) / days_per_year
  fit_adj <- 0.8
  birth_adj <- 2.0
  dose_adj <- 1.0
  emergence <- 0.0
  partial_susceptibility_proph_cattle <- 0.5
  prob_death_from_disease <- 0.0
  use_carrying_capacity <- FALSE # whether to use carrying capacity or K_host_ratio
  carrying_capacity <- c(10000, 6000, 2000) # carrying capacity of vector population
  
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
    mutate(use_carrying_capacity = TRUE, K_host_ratio = K / hosts) %>% 
    select(NC, NW, hosts, use_carrying_capacity, K_host_ratio, K)
  
  tb2b <- expand_grid(tb_hosts, K_host_ratio = K_host_ratio) %>%
    mutate(use_carrying_capacity = FALSE, K = hosts * K_host_ratio) %>% 
    select(NC, NW, hosts, use_carrying_capacity, K_host_ratio, K)
  
  if (TRUE %in% use_carrying_capacity & FALSE %in% use_carrying_capacity) {
    tb2 <- rbind(tb2a, tb2b)
  } else {
    if (TRUE %in% use_carrying_capacity) {
      tb2 <- tb2a 
    } else {
      tb2 <- tb2b
    }
  } 

  tb <- expand_grid(tb2, tb1)

  df <- as.data.frame(tb) %>% mutate(treatment_type = as.factor(treatment_type))
  df
}


create_single_scenario <- function() {
  max_time <- 3000
  treatment_type <- c("curative") # curative or longlasting
  cattle_number <- 100
  wildlife_number <- 100
  K_host_ratio <- 30 # carrying capacity per host
  treat_prop <- 0.6
  # do not set prop_cattle_with_insecticide to 1
  prop_cattle_with_insecticide <- 0.0
  
  days_per_year <- set_days_per_year()
  maintain_vector_pop <- FALSE # whether to maintain vector population at carrying capacity or not)
  prop_prophylaxis_at_birth <- 0.0
  proph_ongoing <- 0 / days_per_year # c(0, 2, 4) / days_per_year
  fit_adj <- 0.8
  birth_adj <- 2.0
  dose_adj <- 1.0
  emergence <- 0.0
  partial_susceptibility_proph_cattle <- 0.5
  prob_death_from_disease <- 0.1
  use_carrying_capacity <- FALSE # whether to use carrying capacity or K_host_ratio
  carrying_capacity <- 6000 # carrying capacity of vector population
  
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
    mutate(use_carrying_capacity = TRUE, K_host_ratio = K / hosts) %>% 
    select(NC, NW, hosts, use_carrying_capacity, K_host_ratio, K)
  
  tb2b <- expand_grid(tb_hosts, K_host_ratio = K_host_ratio) %>%
    mutate(use_carrying_capacity = FALSE, K = hosts * K_host_ratio) %>% 
    select(NC, NW, hosts, use_carrying_capacity, K_host_ratio, K)
  
  if (use_carrying_capacity == TRUE) {
    tb2 <- tb2a 
  } else {
    tb2 <- tb2b
  }
  
  tb <- expand_grid(tb2, tb1)
  
  df <- as.data.frame(tb) %>% mutate(treatment_type = as.factor(treatment_type))
  df
}

findGlobals(fun = create_multiple_scenarios, merge = FALSE)$variables
findGlobals(fun = create_single_scenario, merge = FALSE)$variables
findGlobals(fun = set_days_per_year, merge = FALSE)$variables
findGlobals(fun = get_user_inputs, merge = FALSE)$variables
