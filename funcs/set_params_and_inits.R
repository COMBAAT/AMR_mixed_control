## --------------------- Params & Inits

library(codetools)

## Set parameters & Initial Conditions ----

set_parameters <- function(this_scenario) {
  birth_adj <- this_scenario$birth_adj
  fit_adj <- this_scenario$fit_adj
  K <- this_scenario$K
  treat_prop <- this_scenario$treat_prop
  prop_insecticide <- this_scenario$prop_insecticide
  NW <- this_scenario$NW
  prop_prophylaxis <- this_scenario$prop_prophylaxis
  trt.type <- this_scenario$treatment_type
  dose_adj <- this_scenario$dose_adj
  emergence_adj <- this_scenario$emergence_adj

  ## Cattle -----
  birth_c <- 1 / (5 * 365)
  biterate <- 0.8 / 4
  prob_infection <- 0.46
  gamma_c <- 1 / 15
  resusceptible <- 10
  death_c <- birth_c
  death_p <- death_c
  sigma_c <- 1 / 100
  treatment <- 1 * treat_prop * (sigma_c + death_c) / (1 - treat_prop)
  emergence <- 0


  if (trt.type == "F") {
    treatment_q <- treatment
    treatment_p <- 0
    emergence_p <- 0
    emergence_f <- emergence * emergence_adj
  } else {
    if (trt.type == "P") {
      treatment_q <- 0
      treatment_p <- treatment
      emergence_p <- emergence * emergence_adj
      emergence_f <- 0
    } else {
      treatment_q <- treatment
      treatment_p <- treatment
      emergence_p <- emergence * emergence_adj
      emergence_f <- emergence * emergence_adj
    }
  }

  sigma_st <- (1 / 3) * dose_adj + sigma_c * (1 - dose_adj) #* 250 #LM: adjusted so that R0 drops below 1 when 99% treated to reflect Hargrove
  rec_adj <- 1
  waning <- (1 / 30) / dose_adj
  waning_f2s <- (1 / 60) / dose_adj
  new.prop <- 0


  NC <- 50 # Total cattle
  # figure out equilibrium in absence of infection
  # birth_c * prop_prophylaxis *NC - death_c * PF - waning_f2s*PF
  PF <- birth_c * prop_prophylaxis * NC / (death_c + waning_f2s)
  # waning_f2s * PF - death_p * PS - waning * PS
  PS <- waning_f2s * PF / (death_p + waning)
  # birth_c * (1-prop_prophylaxis) * NC - death_c * CS + waning * PS
  CS <- (birth_c * (1 - prop_prophylaxis) * NC + waning * PS) / death_c

  ## ----- Wildlife
  birth_w <- 1 / 365
  prob_infection.s_w <- 0.46
  prob_infection.r_w <- 0.46
  gamma_w <- 1 / 20
  resusceptible_w <- 1 / 100
  death_w <- birth_w
  sigma_w <- sigma_c
  reversion <- 0

  ## -----  Vectors

  ten2fed <- 1 / 4
  qf <- 0.96 # Probability of surviving on a feeding day
  qn <- 0.98 # Probability of surviving on a non-feeding day
  feed.cyc <- 4 # Days between feeding
  feed.frequency <- 0
  prob_infection.v <- 0.025
  incubation <- 20
  prop_insecticide.actual <- prop_insecticide * NC / (NC + NW) # Proportion of insecticide adjusted for wildlife
  death_v <- -1 * log((1 - prop_insecticide.actual) * qf * qn^feed.cyc) / feed.cyc # Vector death rate
  birth_v <- birth_adj * (-1) * log((1 - 0) * qf * qn^feed.cyc) / feed.cyc # Vector birth rate
  equil_vector_pop <- max(0, K * (1 - death_v / birth_v)) # Vector equilibrium population
  gamma_v <- death_v * exp(-death_v * incubation) / (1 - exp(-death_v * incubation)) # Rate from E to I

  NV <- equil_vector_pop # equil_vector_pop

  ## ----- Parameters & initial conditions output

  params <- cbind(
    NC, NV, NW, PF, PS, CS,
    birth_c, biterate, prob_infection, fit_adj, rec_adj, sigma_st,
    gamma_c, death_c, treatment_p, treatment_q, sigma_c, birth_v,
    death_v, feed.frequency, prob_infection.v, gamma_v, emergence_p, emergence_f,
    reversion, K, birth_w, gamma_w, death_w, sigma_w, equil_vector_pop,
    waning, waning_f2s, new.prop, ten2fed, prop_prophylaxis
  )
  names <- colnames(params)
  params <- as.vector(params)
  names(params) <- names

  qual_check_no0(params) # ensure there are no negative values

  return(params = params)
}

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

findGlobals(fun = set_inital_conditions, merge = FALSE)$variables
findGlobals(fun = set_parameters, merge = FALSE)$variables



