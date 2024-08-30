
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


#-------------------------------------------------------------------------------
# Function Name: get_variables
#
# Description:
#   This function defines and returns a list of variable names categorized into four groups: 
#   cattle without prophylaxis, cattle with prophylaxis, wildlife, and vectors. Each category 
#   contains specific stages of disease progression relevant to that group. These variables are 
#   used for epidemiological modeling and data analysis in a host-vector population framework.
#
# Parameters:
#   None.
#
# Returns:
#   A named list containing four elements, each of which is a character vector:
#   - cattle_no_prophylaxis: Variables representing stages of disease progression in cattle without prophylaxis.
#   - cattle_with_prophylaxis: Variables representing stages of disease progression in cattle with prophylaxis.
#   - wildlife: Variables representing stages of disease progression in wildlife.
#   - vectors: Variables representing stages of disease progression in vectors.
#
# Example of use:
#   variables <- get_variables()
#   print(variables$cattle_no_prophylaxis)
#   print(variables$wildlife)
#
# Dependencies:
#   This function does not have any external dependencies.
#
#-------------------------------------------------------------------------------

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

#-------------------------------------------------------------------------------
# Function Name: initialise_variables_with_zeros
#
# Description:
#   This function creates a named numeric vector initialized with zeros. The length of the vector 
#   is determined by the number of elements in the `variable_names` input. Each element of the 
#   vector is named according to the corresponding value in `variable_names`.
#
# Parameters:
#   variable_names - A character vector containing the names of the variables to be initialized 
#                    with zeros.
#
# Returns:
#   A named numeric vector where each element is initialized to 0.0, and the names are set 
#   according to the `variable_names` input.
#
# Example of use:
#   variable_names <- c("CS", "CEs", "CIs")
#   zero_vector <- initialise_variables_with_zeros(variable_names)
#   print(zero_vector)
#
# Dependencies:
#   This function does not have any external dependencies.
#
#-------------------------------------------------------------------------------

initialise_variables_with_zeros <- function(variable_names) {
  vector_of_zeros <- rep(0.0, length(variable_names))
  names(vector_of_zeros) <- variable_names
  vector_of_zeros
}


#-------------------------------------------------------------------------------
# Function Name: set_inital_conditions2
#
# Description:
#   This function sets the initial conditions for an epidemiological model by initializing the population 
#   states for cattle without prophylaxis, cattle with prophylaxis, wildlife, and vectors. It assigns 
#   initial population values based on the provided parameters and initial infection counts for sensitive 
#   and resistant strains. The function also ensures that there are no negative values in the initialized 
#   conditions.
#
# Parameters:
#   params - A named vector or list containing the model parameters, including:
#            - CS: Initial number of cattle susceptible to infection (no prophylaxis).
#            - PF: Initial number of fully protected prophylactic cattle.
#            - PS: Initial number of partially protected prophylactic cattle.
#            - NW: Initial number of wildlife susceptible to infection.
#            - VSt: Initial number of susceptible vectors (teneral).
#            - VSf: Initial number of susceptible vectors (fed).
#   initial_sensitive_infections - A numeric value representing the initial number of infections with 
#                                  the sensitive strain in cattle without prophylaxis.
#   initial_resistant_infections - A numeric value representing the initial number of infections with 
#                                  the resistant strain in cattle without prophylaxis.
#
# Returns:
#   A named numeric vector representing the initial conditions for all population states in the model, 
#   including cattle without prophylaxis, cattle with prophylaxis, wildlife, and vectors. 
#
# Example of use:
#   params <- c(CS = 1000, PF = 500, PS = 300, NW = 200, VSt = 50, VSf = 50)
#   inits <- set_inital_conditions2(params, initial_sensitive_infections = 10, initial_resistant_infections = 5)
#   print(inits)
#
# Dependencies:
#   - get_variables: To retrieve the list of variable names for each population group.
#   - initialise_variables_with_zeros: To initialize the variables with zeros.
#   - qual_check_no0: To ensure there are no negative values in the initialized conditions.
#
#-------------------------------------------------------------------------------

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
