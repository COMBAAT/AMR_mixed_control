library(codetools)

messages_and_issues <- function() {
  message0 <- "teneral stage - needs susceptibility parameter - NOT DONE"
  message1 <- "vector incubation was been wrongly calculated - FIXED"
  message3 <- "removed reversion at nt currently used - DONE"
  message4 <- "partial susceptibility of PS animals - DONE"
  message5 <- "allow for bite preferences - cancelled for now"
  message6 <- "included new CEX and PEX stages for ongoing prophylatic treatment - DONE"
  message7 <- "fix plotting to allow for multiple treatment types - PARTIAL"
  message8 <- "check initial equilibrium being correctly calculated"
  message9 <- "why is R0 slightly higher for prophylactic versus curative treatment?"
  message10 <- "fix R0 to have PP state wane to PI not CI - DONE"
}

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
    general_descriptor = "Nov28_",
    current_descriptor = "proph_quick_proph_ongoing"
  )
  user_inputs
}

create_multiple_scenarios <- function() {
  days_per_year <- set_days_per_year()
  max_time <- 10000
  treatment_type <- c("proph", "quick") # quick, proph or both
  cattle_number <- 100
  wildlife_number <- 0 #c(0, 100, 250)
  treat_propA <- seq(0.0, 0.9, by = 0.2)
  treat_propB <- seq(0.91, 0.99, by = 0.02)
  treat_prop <- 0.6 #c(treat_propA, treat_propB)
  carrying_capacity <- 10000 #c(10000, 6000, 4000, 2000)
  maintain_vector_pop <- TRUE
  # do not set prop_cattle_with_insecticide to 1 as generates infinite mortality and an error
  prop_cattle_with_insecticide <- 0.0 #seq(0.0, 0.5, by = 0.05)
  prop_prophylaxis_at_birth <- c(0.0) 
  proph_ongoing <- 0 #c(0, 2, 4) / days_per_year
  fit_adj <- 0.8
  birth_adj <- 2.0
  dose_adj <- 1.0
  emergence <- 0.0
  partial_susceptibility_proph_cattle <- 0.8
  

  df <- expand.grid(
    NC = cattle_number, emergence = emergence,
    dose_adj = dose_adj, proph_ongoing = proph_ongoing, 
    partial_susceptibility_proph_cattle = partial_susceptibility_proph_cattle,
    treat_prop = treat_prop, NW = wildlife_number, K = carrying_capacity, maintain_vector_pop = maintain_vector_pop,
    fit_adj = fit_adj, prop_cattle_with_insecticide = prop_cattle_with_insecticide,
    birth_adj = birth_adj, prop_prophylaxis_at_birth = prop_prophylaxis_at_birth,
    treatment_type = treatment_type, max_time = max_time
  )
  
  df
}

create_single_scenario <- function() {
  days_per_year <- set_days_per_year()
  max_time <- 5000
  treatment_type <- sample(c("proph"), 1) # quick or proph
  cattle_number <- 100
  wildlife_number <- 0
  treat_prop <- 0.0
  carrying_capacity <- 6000
  maintain_vector_pop <- TRUE
  prop_cattle_with_insecticide <- 0.0
  prop_prophylaxis_at_birth <- 0.1 
  proph_ongoing <- 2 / days_per_year
  fit_adj <- 0.95
  birth_adj <- 2.0
  dose_adj <- 1.0
  emergence <- 0.0
  partial_susceptibility_proph_cattle <- 0.8

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
