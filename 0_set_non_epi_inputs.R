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
    general_descriptor = "simulation_set_",
    current_descriptor = "code_testing"
  )
  user_inputs
}


findGlobals(fun = set_days_per_year, merge = FALSE)$variables
findGlobals(fun = get_user_inputs, merge = FALSE)$variables
