library(codetools)

set_inital_conditions <- function(params, disease_present) {
  NC <- params["NC"]
  PF <- params["PF"]
  PS <- params["PS"]
  CS <- params["CS"]
  NW <- params["NW"]
  NV <- params["equil_vector_pop"]
  
  if (disease_present == TRUE) {
    CIr <- 0 # Infected (drug resistant strain)
    CIs <- 1 # Infected (drug sensitive strain)
  } else {
    CIr <- 0 # Infected (drug resistant strain)
    CIs <- 0 # Infected (drug sensitive strain)
  }
  
  CS <- CS - CIs - CIr # Susceptible
  CEs <- 0 # Exposed (drug sensitive strain)
  CEr <- 0 # Exposed (drug resistant strain)
  CTs <- 0 # Treated (drug sensitive strain)
  CTr <- 0 # Treated (drug resistant strain)
  
  PEs <- 0 # Exposed (drug sensitive strain)
  PEr <- 0 # Exposed (drug resistant strain)
  PIs <- 0 # Infected (drug sensitive strain)
  PIr <- 0 # Infected (drug resistant strain)
  PTs <- 0 # Treated (drug sensitive strain)
  PTr <- 0 # Treated (drug resistant strain)
  PR <- 0 # Recovered
  PPs <- 0
  PPr <- 0
  
  
  WIs <- 0 # Infected (drug sensitive strain)
  WS <- NW - WIs # Susceptible
  WEs <- 0 # Exposed (drug sensitive strain)
  WEr <- 0 # Exposed (drug resistant strain)
  WIr <- 0 # Infected (drug resistant strain)
  
  ## -----  Vectors
  VSt <- NV # Susceptible
  VSf <- 0
  VEs <- 0 # Exposed (drug sensitive strain)
  VEr <- 0 # Exposed (drug resistant strain)
  VIs <- 0 # Infected (drug sensitive strain)
  VIr <- 0 # Infected (drug resistant strain)
  
  ## ----- Initial conditions output
  
  inits <- cbind(
    CS, CEs, CEr, CIs, CIr, CTs, CTr, PF, PS, PEs, PEr, PIs,
    PIr, PTs, PTr, PPs, PPr, WS, WEs, WEr, WIs, WIr, VSt, VSf, VEs,
    VEr, VIs, VIr
  )
  names <- colnames(inits)
  inits <- as.vector(inits)
  names(inits) <- names
  
  qual_check_no0(inits) # ensure there are no negative values
  
  return(inits = inits)
}

get_variables <- function() {
  cattle_no_prophylaxis <- c("CS", "CEs", "CEr", "CIs", "CIr", "CTs", "CTr")
  cattle_with_prophylaxis <- c("PF", "PS", "PEs", "PEr", "PIs", "PIr", "PTs", "PTr", "PPs", "PPr")
  wildlife <- c("WS", "WEs", "WEr", "WIs", "WIr")
  vectors <- c("VSt", "VSf", "VEs", "VEr", "VIs", "VIr")
  
  variable_names <- list("cattle_no_prophylaxis" = cattle_no_prophylaxis,
                         "cattle_with_prophylaxis" = cattle_with_prophylaxis,
                         "wildlife" = wildlife, 
                         "vectors" = vectors)
  variable_names
}


initialise_variables_with_zeros <- function(variable_names) {
  vector_of_zeros <- rep(0.0, length(variable_names))
  names(vector_of_zeros) <- variable_names
  vector_of_zeros
}


set_inital_conditions2 <- function(params, number_initially_infected) {
  variables_names <- get_variables()
  
  cattle_no_prophylaxis <- initialise_variables_with_zeros(variables_names$cattle_no_prophylaxis)
  cattle_with_prophylaxis <- initialise_variables_with_zeros(variables_names$cattle_with_prophylaxis)
  wildlife <- initialise_variables_with_zeros(variables_names$wildlife)
  vectors <- initialise_variables_with_zeros(variables_names$vectors)
  
  cattle_no_prophylaxis["CS"] <- params["CS"]
  cattle_with_prophylaxis["PF"] <- params["PF"]
  cattle_with_prophylaxis["PS"] <- params["PS"]
  wildlife["WS"] <- params["NW"]
  vectors["VSt"] <- params["equil_vector_pop"]
  
  cattle_no_prophylaxis["CIs"] <- number_initially_infected # Infected (drug resistant strain)
  cattle_no_prophylaxis["CS"] <- cattle_no_prophylaxis["CS"] - number_initially_infected
  
  ## ----- Initial conditions output
  
  inits <- c(cattle_no_prophylaxis, cattle_with_prophylaxis, wildlife, vectors)
  qual_check_no0(inits) # ensure there are no negative values
  
  return(inits)
}

findGlobals(fun = set_inital_conditions, merge = FALSE)$variables
findGlobals(fun = get_variables, merge = FALSE)$variables
findGlobals(fun = initialise_variables_with_zeros, merge = FALSE)$variables
findGlobals(fun = set_inital_conditions2, merge = FALSE)$variables
