library(codetools)

set_days_per_year <- function() {
  days_per_year <- 365.25
  days_per_year
}


set_user_inputs <- function() {
  user_inputs <- list(
    multiple_scenarios = TRUE,
    use_root_functions = TRUE,
    append_current_time_to_output_file = FALSE,
    folder = "output/",
    general_descriptor = "simulation_set_",
    current_descriptor = "ORIGINAL"
  )
  user_inputs
}


get_full_path <- function() {
  user_inputs <- set_user_inputs()
  full_path <- paste0(user_inputs$folder, user_inputs$general_descriptor, user_inputs$current_descriptor)
  full_path
}


get_filename <- function() {
  user_inputs <- set_user_inputs()
  full_path  <- get_full_path()
  if (user_inputs$append_current_time_to_output_file == TRUE) {
    current_time <- get_formatted_time()
    filename <- paste0(full_path, "_", current_time, ".Rda")
  } else {
    filename <- paste0(full_path, ".Rda")
  }
  filename
}

findGlobals(fun = set_days_per_year, merge = FALSE)$variables
findGlobals(fun = get_filename, merge = FALSE)$variables
findGlobals(fun = get_full_path, merge = FALSE)$variables
findGlobals(fun = set_user_inputs, merge = FALSE)$variables
