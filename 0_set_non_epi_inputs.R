library(codetools)

set_days_per_year <- function() {
  days_per_year <- 365.25
  days_per_year
}

set_user_inputs <- function() {
  multiple_scenarios <- TRUE
  use_root_functions <- FALSE
  path <- "output/simulation_set_"
  current_descriptor = "play"
  append_current_time_to_output_file = TRUE
  
  filename <- get_filename2(path, current_descriptor, append_current_time_to_output_file)
  user_inputs <- list(multiple_scenarios = multiple_scenarios, 
                      use_root_functions = use_root_functions,
                      descriptor = current_descriptor, filename = filename)
  user_inputs
}

findGlobals(fun = set_days_per_year, merge = FALSE)$variables
findGlobals(fun = set_user_inputs, merge = FALSE)$variables