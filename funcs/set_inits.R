
# =========================================================
# Function Names: get_variables, initialise_variables_with_zeros, set_inital_conditions2
# Description: This script provides functions to initialize variables and set initial conditions for an epidemiological model.
#              It includes functions to retrieve variable names for different model compartments, initialize these variables
#              with zeros, and set specific initial conditions based on model parameters and infection statuses.
#
# Parameters:
#   variable_names - List of variable names to be initialized.
#   params - Parameters specifying initial conditions and other model settings.
#   initial_sensitive_infections, initial_resistant_infections - Initial counts of infections, sensitive and resistant.
#
# Returns:
#   Depending on the function, returns lists of variable names, initialized variables, or modified parameters.
#
# Example of use:
#   vars <- get_variables()
#   zeros <- initialise_variables_with_zeros(vars$cattle_no_prophylaxis)
#   initial_conditions <- set_inital_conditions2(params, 100, 10)
#
# Dependencies: Requires the 'codetools' package for managing code properties.
#
# Author: Shaun Keegan & Louise Matthews
# Date Created: August 2024
# Last Modified: August 2024
# =========================================================
library(codetools)


get_variables <- function() {
  cattle_no_prophylaxis <- c("CS", "CEs", "CEr", "CIs", "CIr", "CTs", "CTr", "CEsX", "CErX")
  cattle_with_prophylaxis <- c("PF", "PS", "PEs", "PEr", "PIs", "PIr", "PTs", "PTr", "PPs", "PPr", "PEsX", "PErX")
  wildlife <- c("WS", "WEs", "WEr", "WIs", "WIr")
  vectors <- c("VSt", "VSf", "VEs", "VEr", "VIs", "VIr")

  variable_names <- list(
    "cattle_no_prophylaxis" = cattle_no_prophylaxis,
    "cattle_with_prophylaxis" = cattle_with_prophylaxis,
    "wildlife" = wildlife,
    "vectors" = vectors
  )
  variable_names
}


initialise_variables_with_zeros <- function(variable_names) {
  vector_of_zeros <- rep(0.0, length(variable_names))
  names(vector_of_zeros) <- variable_names
  vector_of_zeros
}


set_inital_conditions2 <- function(params, initial_sensitive_infections, initial_resistant_infections) {
  variables_names <- get_variables()

  cattle_no_prophylaxis <- initialise_variables_with_zeros(variables_names$cattle_no_prophylaxis)
  cattle_with_prophylaxis <- initialise_variables_with_zeros(variables_names$cattle_with_prophylaxis)
  wildlife <- initialise_variables_with_zeros(variables_names$wildlife)
  vectors <- initialise_variables_with_zeros(variables_names$vectors)

  cattle_no_prophylaxis["CS"] <- params["CS"]
  cattle_with_prophylaxis["PF"] <- params["PF"]
  cattle_with_prophylaxis["PS"] <- params["PS"]
  wildlife["WS"] <- params["NW"]
  #vectors["VSt"] <- params["equil_vector_pop"]
  vectors["VSt"] <- params["VSt"]
  vectors["VSf"] <- params["VSf"]
  

  cattle_no_prophylaxis["CIs"] <- initial_sensitive_infections # Infected (drug resistant strain)
  cattle_no_prophylaxis["CIr"] <- initial_resistant_infections
  cattle_no_prophylaxis["CS"] <- cattle_no_prophylaxis["CS"] - initial_sensitive_infections - initial_resistant_infections

  ## ----- Initial conditions output

  inits <- c(cattle_no_prophylaxis, cattle_with_prophylaxis, wildlife, vectors)
  qual_check_no0(inits) # ensure there are no negative values

  return(inits)
}

findGlobals(fun = get_variables, merge = FALSE)$variables
findGlobals(fun = initialise_variables_with_zeros, merge = FALSE)$variables
findGlobals(fun = set_inital_conditions2, merge = FALSE)$variables
