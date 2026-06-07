# =========================================================
# Function Name: AAT_AMR_dens_dep
# Description: This script models the dynamics of African Animal Trypanosomiasis (AAT)
#              incorporating the emergence, spread, and loss of antimicrobial resistance (AMR)
#              among cattle, tsetse flies, and wildlife. This version includes compartments
#              for the teneral phenomenon, enhancing the model's accuracy in depicting
#              disease transmission and resistance dynamics under various scenarios.
#
# Parameters:
#   No parameters are directly set in this script; it is designed to be sourced and used with
#   scenario-specific parameters set in separate script files.
#
# Returns:
#   The model does not return values directly but updates global variables and can be used
#   to simulate disease spread and intervention scenarios when called from other scripts.
#
#
# Dependencies: Requires deSolve
#               Assumes that scenario-specific settings are managed in separate
#               scripts within the repository.
#
# Author: Shaun Keegan & Louise Matthews
# Date Created: May 2022
# Last Modified: August 2024
# =========================================================


library(codetools)

AAT_AMR_dens_dep <- function(times, init, parms) {
  # C - Cattle
  CS <- init["CS"] # Susceptible
  CEs <- init["CEs"] # Exposed (drug sensitive strain)
  CEr <- init["CEr"] # Exposed (drug resistant strain)
  CIs <- init["CIs"] # Infected (drug sensitive strain)
  CIr <- init["CIr"] # Infected (drug resistant strain)
  CTs <- init["CTs"] # Treated (drug sensitive strain)
  CTr <- init["CTr"] # Treated (drug resistant strain)
  CEXs <- init["CEXs"] # Exposed (drug sensitive strain)
  CEXr <- init["CEXr"] # Exposed (drug resistant strain)
  # CR  <- init["CR"] # Recovered

  # P - Prophylactically treated cattle
  PF <- init["PF"] # Susceptible Fully protected
  PS <- init["PS"] # Susceptible
  PEs <- init["PEs"] # Exposed (drug sensitive strain)
  PEr <- init["PEr"] # Exposed (drug resistant strain)
  PIs <- init["PIs"] # Infected (drug sensitive strain)
  PIr <- init["PIr"] # Infected (drug resistant strain)
  PTs <- init["PTs"] # Treated (drug sensitive strain)
  PTr <- init["PTr"] # Treated (drug resistant strain)
  PPs <- init["PPs"] # Recovered
  PPr <- init["PPr"] # Recovered
  PEsX <- init["PEsX"] # Exposed (drug sensitive strain)
  PErX <- init["PErX"] # Exposed (drug resistant strain)

  # W - Wildlife
  WS <- init["WS"] # Susceptible
  WEs <- init["WEs"] # Exposed (drug sensitive strain)
  WEr <- init["WEr"] # Exposed (drug resistant strain)
  WIs <- init["WIs"] # Infected (drug sensitive strain)
  WIr <- init["WIr"] # Infected (drug resistant strain)
  # WR  <- init["WR"] # Recovered

  # V - Vectors
  VSt <- init["VSt"] # Susceptible teneral
  VSf <- init["VSf"] # Susceptible fed
  VEs <- init["VEs"] # Exposed (drug sensitive strain)
  VEr <- init["VEr"] # Exposed (drug resistant strain)
  VIs <- init["VIs"] # Infected (drug sensitive strain)
  VIr <- init["VIr"] # Infected (drug resistant strain)

  ## ----- Cattle
  birth_c <- parms["birth_c"]
  biterate <- parms["biterate"]
  prob_infection_to_host <- parms["prob_infection_to_host"]
  gamma_c <- parms["gamma_c"]
  death_c <- parms["death_c"]
  sigma_c <- parms["sigma_c"]
  treatment_q <- parms["treatment_q"]
  treatment_p <- parms["treatment_p"]
  sigma_st <- parms["sigma_st"]
  emergence_p <- parms["emergence_p"]
  emergence_q <- parms["emergence_q"]
  prop_prophylaxis_at_birth <- parms["prop_prophylaxis_at_birth"]
  proph_ongoing <- parms["proph_ongoing"]
  fit_adj <- parms["fit_adj"]
  waning_from_PE <- parms["waning_from_PE"]
  waning_from_PP <- parms["waning_from_PP"]
  waning_from_PI <- parms["waning_from_PI"]
  waning_from_PT <- parms["waning_from_PT"]
  waning_from_PS <- parms["waning_from_PS"]
  waning_F2S <- parms["waning_F2S"]
  partial_susceptibility_proph_cattle <- parms["partial_susceptibility_proph_cattle"]
  death_dis <- parms["death_dis"]

  ## ----- Wildlife
  birth_w <- parms["birth_w"]
  gamma_w <- parms["gamma_w"]
  death_w <- parms["death_w"]
  sigma_w <- parms["sigma_w"]

  ## ----- Vectors
  K <- parms["K"]
  feeding.rate <- parms["feeding.rate"]
  prob_infection_to_vector <- parms["prob_infection_to_vector"]
  death_v <- parms["death_v"]
  birth_v <- parms["birth_v"]
  gamma_v <- parms["gamma_v"]
  ten2fed <- parms["ten2fed"]

  # Population total ----
  Is_cattle <- CIs + CTs + PIs + PTs + PPs
  Ir_cattle <- CIr + CTr + PIr + PTr + PPr
  C <- CS + CEs + CEr + CIs + CIr + CTs + CTr + CEXs + CEXr
  P <- PF + PS + PEs + PEr + PIs + PIr + PTs + PTr + PPs + PPr + PEsX + PErX
  W <- WS + WEs + WEr + WIs + WIr
  V <- VSt + VSf + VEs + VEr + VIs + VIr
  NC <- P + C
  N <- C + P + W
  
  # Population fractions
  Is_cattle_frac <- calc_frac_with_zero(Is_cattle, NC)
  Ir_cattle_frac <- calc_frac_with_zero(Ir_cattle, NC)
  WS_frac <- calc_frac_with_zero(WS, W)
  WIs_frac <- calc_frac_with_zero(WIs, W)
  WIr_frac <- calc_frac_with_zero(WIr, W)
  CS_frac <- calc_frac_with_zero(CS, NC)
  PS_frac <- calc_frac_with_zero(PS, NC)
  PF_frac <- calc_frac_with_zero(PF, NC)
  
  

  # Cattle without long-lasting drug treatment ----

  dCS.dt <-
    birth_c * (1 - prop_prophylaxis_at_birth) * NC +
    waning_from_PS * PS -
    biterate * prob_infection_to_host * CS_frac * VIs * bite_frac_cattle(NC, N) -
    biterate * prob_infection_to_host * fit_adj * CS_frac * VIr * bite_frac_cattle(NC, N) -
    proph_ongoing * CS +
    death_dis * PIs +
    death_dis * PIr +
    death_dis * CIs +
    death_dis * CIr +
    sigma_c * CIs +
    sigma_c * CIr +
    sigma_st * CTs +
    sigma_c * CTr -
    death_c * CS
    

  dCEs.dt <-
    biterate * prob_infection_to_host * CS_frac * VIs * bite_frac_cattle(NC, N) -
    gamma_c * CEs +
    waning_from_PE * PEs -
    proph_ongoing * CEs -
    death_c * CEs

  dCEXs.dt <-
    proph_ongoing * CEs -
    sigma_st * CEXs -
    gamma_c * CEXs -
    death_c * CEXs

  dCEr.dt <-
    biterate * prob_infection_to_host * fit_adj * CS_frac * VIr * bite_frac_cattle(NC, N) -
    gamma_c * CEr +
    waning_from_PE * PEr -
    proph_ongoing * CEr -
    death_c * CEr

  dCEXr.dt <-
    proph_ongoing * CEr -
    sigma_c * CEXr -
    gamma_c * CEXr -
    death_c * CEXr

  dCIs.dt <- gamma_c * CEs -
    treatment_q * CIs -
    treatment_p * CIs -
    proph_ongoing * CIs -
    sigma_c * CIs +
    waning_from_PI * PIs -
    death_c * CIs -
    death_dis * CIs
    

  dCIr.dt <- gamma_c * CEr -
    treatment_q * CIr -
    treatment_p * CIr -
    proph_ongoing * CIr -
    sigma_c * CIr +
    waning_from_PI * PIr -
    death_c * CIr -
    death_dis * CIr
    

  dCTs.dt <- treatment_q * CIs -
    sigma_st * CTs -
    emergence_q * CTs +
    waning_from_PT * PTs -
    death_c * CTs

  dCTr.dt <- treatment_q * CIr -
    sigma_c * CTr +
    emergence_q * CTs +
    waning_from_PT * PTr -
    death_c * CTr

# Cattle with long lasting drug treatment ----
  
  dPF.dt <- birth_c * (prop_prophylaxis_at_birth) * NC - # Adding new prophylactically treated cattle
    biterate * prob_infection_to_host * fit_adj * PF_frac * VIr * bite_frac_cattle(NC, N) + 
    sigma_st * PPs + 
    sigma_c * PPr - 
    waning_F2S * PF + # waning from fully protected to partially protected
    proph_ongoing * PS +
    proph_ongoing * CS +
    sigma_st * CEXs +
    sigma_c * CEXr +
    sigma_st * PEsX +
    sigma_c * PErX -
    death_c * PF

  dPS.dt <- 
    waning_F2S * PF - # waning from fully protected to partially protected
    biterate * partial_susceptibility_proph_cattle * prob_infection_to_host * PS_frac * VIs * bite_frac_cattle(NC, N) - 
    biterate * prob_infection_to_host * fit_adj * PS_frac * VIr * bite_frac_cattle(NC, N) - 
    proph_ongoing * PS +
    #(1 - death_dis) * sigma_c * PIs + 
    #(1 - death_dis) * sigma_c * PIr + 
    sigma_c * PIs + 
    sigma_c * PIr + 
    sigma_st * PTs + 
    sigma_c * PTr - 
    waning_from_PS * PS - 
    death_c * PS
    

  dPEs.dt <-
    biterate * partial_susceptibility_proph_cattle * prob_infection_to_host * PS_frac * VIs * bite_frac_cattle(NC, N) - 
    gamma_c * PEs - 
    emergence_p * PEs -
    waning_from_PE * PEs - 
    proph_ongoing * PEs -
    death_c * PEs

  dPEsX.dt <-
    proph_ongoing * PEs -
    gamma_c * PEsX - 
    sigma_st * PEsX -
    # emergence_p * PEsX -
    death_c * PEsX 

  dPEr.dt <-
    biterate * prob_infection_to_host * fit_adj * PS_frac * VIr * bite_frac_cattle(NC, N) + 
    biterate * prob_infection_to_host * fit_adj * PF_frac * VIr * bite_frac_cattle(NC, N) - 
    gamma_c * PEr + 
    emergence_p * PEs -
    waning_from_PE * PEr - 
    proph_ongoing * PEr -
    death_c * PEr

  dPErX.dt <-
    proph_ongoing * PEr -
    gamma_c * PErX - 
    sigma_c * PErX -
    # emergence_p * PEsX -
    death_c * PErX 

  dPIs.dt <- gamma_c * PEs - 
    treatment_q * PIs - 
    treatment_p * PIs - 
    sigma_c * PIs - 
    emergence_p * PIs - 
    waning_from_PI * PIs +
    waning_from_PP * PPs - 
    proph_ongoing * PIs -
    death_c * PIs -
    death_dis * PIs

  dPIr.dt <- gamma_c * PEr - 
    treatment_q * PIr - 
    treatment_p * PIr - 
    sigma_c * PIr + 
    emergence_p * PIs - 
    waning_from_PI * PIr + 
    waning_from_PP * PPr - 
    proph_ongoing * PIr -
    death_c * PIr -
    death_dis * PIr

  dPTs.dt <- treatment_q * PIs - 
    sigma_st * PTs - 
    emergence_p * PTs -
    emergence_q * PTs - 
    waning_from_PT * PTs - 
    death_c * PTs 

  dPTr.dt <- treatment_q * PIr - 
    sigma_c * PTr + 
    emergence_p * PTs +
    emergence_q * PTs - 
    waning_from_PT * PTr - 
    death_c * PTr 

  dPPs.dt <- treatment_p * PIs + 
    treatment_p * CIs - 
    emergence_p * PPs -
    sigma_st * PPs - 
    waning_from_PP * PPs +
    proph_ongoing * PIs +
    proph_ongoing * CIs +
    gamma_c * CEXs +
    gamma_c * PEsX -
    death_c * PPs

  dPPr.dt <- treatment_p * PIr + 
    treatment_p * CIr + 
    emergence_p * PPs -
    sigma_c * PPr - 
    waning_from_PP * PPr + 
    proph_ongoing * PIr +
    proph_ongoing * CIr +
    gamma_c * CEXr +
    gamma_c * PErX -
    death_c * PPr

  
  # Wildlife ----

  dWS.dt <- birth_w * W -
    #biterate * prob_infection_to_host * WS * VIs / N -
    #biterate * (prob_infection_to_host * fit_adj) * WS * VIr / N +
    biterate * prob_infection_to_host * WS_frac * VIs * bite_frac_wildlife(W, N) -
    biterate * (prob_infection_to_host * fit_adj) * WS_frac * VIr * bite_frac_wildlife(W, N) +
    sigma_w * WIs +
    sigma_w * WIr -
    death_w * WS

  dWEs.dt <-
    biterate * prob_infection_to_host * WS_frac * VIs * bite_frac_wildlife(W, N) -
    gamma_w * WEs -
    death_w * WEs

  dWEr.dt <-
    biterate * (prob_infection_to_host * fit_adj) * WS_frac * VIr * bite_frac_wildlife(W, N) -
    gamma_w * WEr -
    death_w * WEr

  dWIs.dt <- gamma_w * WEs - sigma_w * WIs - death_w * WIs

  dWIr.dt <- gamma_w * WEr - sigma_w * WIr - death_w * WIr

  # Tsetse ----

  dVSt.dt <- birth_v * V * (1 - V / K) -
    #prob_infection_to_vector * biterate * VSt * Is / N -
    #prob_infection_to_vector * biterate * VSt * Ir / N -
    prob_infection_to_vector * biterate * VSt * Is_cattle_frac * bite_frac_cattle(NC, N) -
    prob_infection_to_vector * biterate * VSt * Ir_cattle_frac * bite_frac_cattle(NC, N) -
    prob_infection_to_vector * biterate * VSt * WIs_frac * bite_frac_wildlife(W, N) -
    prob_infection_to_vector * biterate * VSt * WIr_frac * bite_frac_wildlife(W, N) -
    ten2fed * VSt -
    death_v * VSt

  dVSf.dt <- ten2fed * VSt -
    prob_infection_to_vector * biterate * VSf * Is_cattle_frac * bite_frac_cattle(NC, N) -
    prob_infection_to_vector * biterate * VSf * Ir_cattle_frac * bite_frac_cattle(NC, N) -
    prob_infection_to_vector * biterate * VSf * WIs_frac * bite_frac_wildlife(W, N) -
    prob_infection_to_vector * biterate * VSf * WIr_frac * bite_frac_wildlife(W, N) -
    death_v * VSf

  dVEs.dt <- 
    prob_infection_to_vector * biterate * VSt * Is_cattle_frac * bite_frac_cattle(NC, N) +
    prob_infection_to_vector * biterate * VSf * Is_cattle_frac * bite_frac_cattle(NC, N) +
    prob_infection_to_vector * biterate * VSt * WIs_frac * bite_frac_wildlife(W, N) +
    prob_infection_to_vector * biterate * VSf * WIs_frac * bite_frac_wildlife(W, N) -
    gamma_v * VEs - death_v * VEs

  dVEr.dt <-
    prob_infection_to_vector * biterate * VSt * Ir_cattle_frac * bite_frac_cattle(NC, N) +
    prob_infection_to_vector * biterate * VSf * Ir_cattle_frac * bite_frac_cattle(NC, N) +
    prob_infection_to_vector * biterate * VSt * WIr_frac * bite_frac_wildlife(W, N) +
    prob_infection_to_vector * biterate * VSf * WIr_frac * bite_frac_wildlife(W, N) -
    gamma_v * VEr - death_v * VEr

  dVIs.dt <- gamma_v * VEs - death_v * VIs

  dVIr.dt <- gamma_v * VEr - death_v * VIr

  # Model output ----
  # dX <- c(
  #   dCS.dt, dCEs.dt, dCEr.dt, dCIs.dt, dCIr.dt, dCTs.dt, dCTr.dt, dCEXs.dt, dCEXr.dt,
  #   dPF.dt, dPS.dt, dPEs.dt, dPEr.dt, dPIs.dt, dPIr.dt, dPTs.dt, dPTr.dt, dPPs.dt, dPPr.dt, dPEsX.dt, dPErX.dt,
  #   dWS.dt, dWEs.dt, dWEr.dt, dWIs.dt, dWIr.dt,
  #   dVSt.dt, dVSf.dt, dVEs.dt, dVEr.dt, dVIs.dt, dVIr.dt
  # )
  dX <- c(
    dCS.dt, dCEs.dt, 0, dCIs.dt, 0, dCTs.dt, 0, dCEXs.dt, 0,
    dPF.dt, dPS.dt, dPEs.dt, 0, dPIs.dt, 0, dPTs.dt, 0, dPPs.dt, 0, dPEsX.dt, 0,
    dWS.dt, dWEs.dt, 0, dWIs.dt, 0,
    dVSt.dt, dVSf.dt, dVEs.dt, 0, dVIs.dt, 0
  )
  list(dX)
}


findGlobals(fun = AAT_AMR_dens_dep, merge = FALSE)$variables
