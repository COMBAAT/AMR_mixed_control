library(codetools)

# comments on issues
# updates to INcidence and prevalence in append_epi_outputs, check Daniel's code
# R0 intutive does not properly capture mortality due to disease?

get_user_inputs <- function() {
  user_inputs <- list(
    multiple_scenarios = TRUE,
    use_root_functions = TRUE,
    append_current_time_to_output_file = FALSE,
    folder = "output/",
    general_descriptor = "07May2026",
    current_descriptor = "_n6_test1"
  )
  user_inputs
}



create_multiple_scenarios_new <- function() {
  max_time <- 3500
  treatment_type <- c("curative", "longlasting") # curative or longlasting
  cattle_number <- 100
  wildlife_number <- 100 #c(0, 100, 200)
  K_host_ratio <- c(20, 40, 60)     # carrying capacity per host
  treat_propA <- seq(0.0, 0.9, by = 0.05)
  treat_propB <- seq(0.91, 0.99, by = 0.04)
  treat_prop <- 0.2 #c(treat_propA, treat_propB) # treatment proportion of cattle with trypanocides
  # do not set prop_cattle_with_insecticide to 1
  #prop_cattle_with_insecticide <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6) #c(seq(0.0, 0.95, by = 0.05), 0.99)
  prop_cattle_with_insecticide <- 0.0 #c(seq(0, 0.95, by = 0.05), 0.99)
  
  days_per_year <- set_days_per_year()
  maintain_vector_pop <- c(TRUE, FALSE) # whether to maintain vector population at carrying capacity or not
  prop_prophylaxis_at_birth <- c(0.0)
  treatments_per_year <- seq(0, 9, by = 1)
  fit_adj <- 0.8
  birth_adj <- 2.0
  dose_adj <- 1.0
  emergence <- 0.0
  partial_susceptibility_proph_cattle <- 0.5
  prob_death_from_disease <- 0.01
  #use_carrying_capacity <- FALSE # whether to use carrying capacity or K_host_ratio
  #carrying_capacity <- c(5000) # carrying capacity of vector population
  
  # create grids of parameters combinations and then combine
  responsive_treatment <- expand_grid(treatment_type = treatment_type, treat_prop = treat_prop, treatments_per_year = 0) %>%
    mutate(laXbel = case_when(treatment_type == "curative" ~ "responsive_curative",
                              treatment_type == "longlasting" ~ "responsive_longlasting"),
           treatment_code = case_when(treatment_type == "curative" ~ 1, 
                             treatment_type == "longlasting" ~ 2))
  
  prophylatic_treatment <- expand_grid(treatment_type = "longlasting", treat_prop = 0, treatments_per_year = treatments_per_year) %>%
    mutate(laXbel = "proph_ongoing", treatment_code = 3)
  
  all_treatments <- rbind(responsive_treatment, prophylatic_treatment) %>% 
    mutate(proph_ongoing = treatments_per_year / days_per_year) 
  
  tb0 <- expand_grid(
    emergence = emergence,
    dose_adj = dose_adj,
    partial_susceptibility_proph_cattle = partial_susceptibility_proph_cattle,
    prob_death_from_disease = prob_death_from_disease,
    NC = cattle_number, NW = wildlife_number,
    K_host_ratio = K_host_ratio, 
    maintain_vector_pop = maintain_vector_pop,
    fit_adj = fit_adj, prop_cattle_with_insecticide = prop_cattle_with_insecticide,
    birth_adj = birth_adj, prop_prophylaxis_at_birth = prop_prophylaxis_at_birth,
    max_time = max_time
  )
  tb <- expand_grid(tb0, all_treatments) %>% 
    mutate(use_carrying_capacity = FALSE, hosts = NC + NW, K = hosts * K_host_ratio) 
  
  df <- as.data.frame(tb) %>% mutate(treatment_type = as.factor(treatment_type), laXbel = as.factor(laXbel))
  df
}


create_single_scenario_new <- function() {
  max_time <- 3500
  treatment_type <- c("curative") # curative or longlasting
  cattle_number <- 100
  wildlife_number <- 0
  K_host_ratio <- 40 #c(20, 40, 60)     # carrying capacity per host
  treat_prop <- 0.0
  prop_cattle_with_insecticide <- 0.2
  
  days_per_year <- set_days_per_year()
  maintain_vector_pop <- c(TRUE) # whether to maintain vector population at carrying capacity or not
  prop_prophylaxis_at_birth <- c(0.0)
  treatments_per_year <- 0
  fit_adj <- 0.8
  birth_adj <- 2.0
  dose_adj <- 1.0
  emergence <- 0.0
  partial_susceptibility_proph_cattle <- 0.5
  prob_death_from_disease <- 0.01
  #use_carrying_capacity <- FALSE # whether to use carrying capacity or K_host_ratio
  #carrying_capacity <- c(5000) # carrying capacity of vector population
  
  # create grids of parameters combinations and then combine
  responsive_treatment <- expand_grid(treatment_type = treatment_type, treat_prop = treat_prop, treatments_per_year = 0) %>%
    mutate(laXbel = case_when(treatment_type == "curative" ~ "responsive_curative",
                              treatment_type == "longlasting" ~ "responsive_longlasting"),
           treatment_code = case_when(treatment_type == "curative" ~ 1, 
                                      treatment_type == "longlasting" ~ 2))
  
  prophylatic_treatment <- expand_grid(treatment_type = "longlasting", treat_prop = 0, treatments_per_year = treatments_per_year) %>%
    mutate(laXbel = "proph_ongoing", treatment_code = 3)
  
  all_treatments <- rbind(responsive_treatment, prophylatic_treatment) %>% 
    mutate(proph_ongoing = treatments_per_year / days_per_year) 
  
  tb0 <- expand_grid(
    emergence = emergence,
    dose_adj = dose_adj,
    partial_susceptibility_proph_cattle = partial_susceptibility_proph_cattle,
    prob_death_from_disease = prob_death_from_disease,
    NC = cattle_number, NW = wildlife_number,
    K_host_ratio = K_host_ratio, 
    maintain_vector_pop = maintain_vector_pop,
    fit_adj = fit_adj, prop_cattle_with_insecticide = prop_cattle_with_insecticide,
    birth_adj = birth_adj, prop_prophylaxis_at_birth = prop_prophylaxis_at_birth,
    max_time = max_time
  )
  tb <- expand_grid(tb0, all_treatments) %>% 
    mutate(use_carrying_capacity = FALSE, hosts = NC + NW, K = hosts * K_host_ratio) 
  
  df <- as.data.frame(tb) %>% mutate(treatment_type = as.factor(treatment_type), laXbel = as.factor(laXbel))
  df <- df[1,]
  df
}




findGlobals(fun = create_multiple_scenarios_new, merge = FALSE)$variables
findGlobals(fun = create_single_scenario_new, merge = FALSE)$variables
findGlobals(fun = set_days_per_year, merge = FALSE)$variables
findGlobals(fun = get_user_inputs, merge = FALSE)$variables
