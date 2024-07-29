## This is an ordinary differential equation model of African Animal 
## Trypanosomiasis (AAT) that incorporates the emergence, spread and loss of 
## antimicrobial resistance (AMR) between cattle, tsetse fly vectors and 
## wildlife. 

## VERSION 4 - May 2022
##
## This version includes compartments to account for the teneral phenomenon.


## Authors:   Shaun Keegan (shaun.keegan@glasgow.ac.uk)
##            Louise Matthews (louise.mattthews@glasgow.ac.uk)


## FORMAT: This file uses plain text descriptions of model parameters for user
##         accessibility. Mathematical model descriptions and corresponding 
##         parameter tables can be found at: 
##         http://github.com/shaunkeegan/AAT_AMR_main/model

## USAGE:  This file has been designed to be run and sourced from other files 
##         in the git repository, so that the model file is left untouched when 
##         exploring scenarios which are included at: 
##         http://github.com/shaunkeegan/AAT_AMR_main/scenarios

library(codetools)

AAT_AMR_dens_dep <- function(times, init, parms){
  
  # C - Cattle
  CS  <- init["CS"] # Susceptible
  CEs <- init["CEs"] # Exposed (drug sensitive strain)
  CEr <- init["CEr"] # Exposed (drug resistant strain)
  CIs <- init["CIs"] # Infected (drug sensitive strain)
  CIr <- init["CIr"] # Infected (drug resistant strain)
  CTs <- init["CTs"] # Treated (drug sensitive strain)
  CTr <- init["CTr"] # Treated (drug resistant strain)
  #CR  <- init["CR"] # Recovered
  
  # P - Prophylactically treated cattle
  PF  <- init["PF"]  # Susceptible Fully protected
  PS  <- init["PS"]  # Susceptible
  PEs <- init["PEs"] # Exposed (drug sensitive strain)
  PEr <- init["PEr"] # Exposed (drug resistant strain)
  PIs <- init["PIs"] # Infected (drug sensitive strain)
  PIr <- init["PIr"] # Infected (drug resistant strain)
  PTs <- init["PTs"] # Treated (drug sensitive strain)
  PTr <- init["PTr"] # Treated (drug resistant strain)
  PPs <- init["PPs"] # Recovered
  PPr <- init["PPr"] # Recovered
  
  # W - Wildlife
  WS  <- init["WS"] # Susceptible
  WEs <- init["WEs"] # Exposed (drug sensitive strain)
  WEr <- init["WEr"] # Exposed (drug resistant strain)
  WIs <- init["WIs"] # Infected (drug sensitive strain)
  WIr <- init["WIr"] # Infected (drug resistant strain)
  #WR  <- init["WR"] # Recovered
  
  # V - Vectors
  VSt <- init["VSt"] # Susceptible teneral
  VSf <- init["VSf"] # Susceptible fed
  VEs <- init["VEs"] # Exposed (drug sensitive strain) 
  VEr <- init["VEr"] # Exposed (drug resistant strain)
  VIs <- init["VIs"] # Infected (drug sensitive strain)
  VIr <- init["VIr"] # Infected (drug resistant strain) 
  
  ## ----- Cattle
  birth_c          <- parms["birth_c"]
  biterate         <- parms["biterate"]
  prob_infection_to_host <- parms["prob_infection_to_host"]
  gamma_c          <- parms["gamma_c"]
  death_c          <- parms["death_c"]
  sigma_c          <- parms["sigma_c"]
  treatment_q      <- parms["treatment_q"]
  treatment_p      <- parms["treatment_p"]
  sigma_st         <- parms["sigma_st"]
  emergence_p      <- parms["emergence_p"]  
  emergence_q      <- parms["emergence_q"]
  rec_adj          <- parms["rec_adj"]
  prop_prophylaxis_at_birth <- parms["prop_prophylaxis_at_birth"]
  proph_ongoing    <- parms["proph_ongoing"]
  fit_adj          <- parms["fit_adj"]
  waning           <- parms["waning"]
  waning_f2s       <- parms["waning_f2s"]
  partial_susceptibility <- parms["partial_susceptibility"]
  
  ## ----- Wildlife
  birth_w            <- parms["birth_w"]
  gamma_w            <- parms["gamma_w"]
  death_w            <- parms["death_w"]
  sigma_w            <- parms["sigma_w"]
  reversion          <- parms["reversion"]
  
  ## ----- Vectors
  K                  <- parms["K"]
  feeding.rate       <-  parms["feeding.rate"]
  prob_infection_to_vector   <-  parms["prob_infection_to_vector"]
  death_v            <- parms["death_v"]
  birth_v            <- parms["birth_v"]
  gamma_v            <- parms["gamma_v"]
  ten2fed            <- parms["ten2fed"]
  
  
  # Population total ----
  N <- CS + CEs + CEr + CIs + CIr + CTs + CTr +
    PF + PS + PEs + PEr + PIs + PIr + PTs + PTr +  PPs + PPr +
    WS + WEs + WEr + WIs + WIr 
  C <- CS + CEs + CEr + CIs + CIr + CTs + CTr 
  P <- PF + PS + PEs + PEr + PIs + PIr + PTs + PTr + PPs + PPr
  PC <- CS + CEs + CEr + CIs + CIr + CTs + CTr +
        PF + PS + PEs + PEr + PIs + PIr + PTs + PTr + PPs + PPr
  W <- WS + WEs + WEr + WIs + WIr 
  V <- VSt + VSf + VEs + VEr + VIs + VIr
  
  
  CS_C_frac <-  ifelse(C > 0, CS / C, 0)
  CIs_C_frac <- ifelse(C > 0, CIs / C, 0)
  CIr_C_frac <- ifelse(C > 0, CIr / C, 0)
  CTs_C_frac <- ifelse(C > 0, CTs / C, 0)
  CTr_C_frac <- ifelse(C > 0, CTr / C, 0)
  
  PF_P_frac <-  ifelse(P > 0, PF / P, 0)
  PS_P_frac <-  ifelse(P > 0, PS / P, 0)
  PIs_P_frac <- ifelse(P > 0, PIs / P, 0)
  PIr_P_frac <- ifelse(P > 0, PIr / P, 0)
  PTs_P_frac <- ifelse(P > 0, PTs / P, 0)
  PTr_P_frac <- ifelse(P > 0, PTr / P, 0)
  PPs_P_frac <- ifelse(P > 0, PPs / P, 0)
  PPr_P_frac <- ifelse(P > 0, PPr / P, 0)
  
  WS_W_frac <-  ifelse(W > 0, WS / W, 0)
  WIs_W_frac <- ifelse(W > 0, WIs / W, 0)
  WIr_W_frac <- ifelse(W > 0, WIr / W, 0)
  
  # Cattle ----
  # 
  # CS, CEs, CEr, CIs, CIr, CTs, CTr, CR
  
  dCS.dt <- 
    birth_c * (1 - prop_prophylaxis_at_birth) * PC +
    waning * PS -
  # biterate * prob_infection_to_host * CS * VIs / N -  
  # biterate * prob_infection_to_host * (CS / C) * VIs * (C / N) - 
    biterate * prob_infection_to_host * CS_C_frac * VIs * (C / N) - 
  # biterate * (prob_infection_to_host * fit_adj) * CS * VIr / N  + 
    biterate * (prob_infection_to_host * fit_adj) * CS_C_frac * VIr * (C / N)  + 
    sigma_c  * CIs + 
    sigma_c  * CIr + 
    sigma_st  * CTs +
    (sigma_c * rec_adj)  * CTr - 
    death_c * CS -
    proph_ongoing * CS
  
  dCEs.dt <- 
  # biterate * prob_infection_to_host * CS * VIs / N - 
    biterate * prob_infection_to_host * CS_C_frac * VIs * (C / N) - 
    gamma_c * CEs + 
    waning * PEs - 
    death_c * CEs +
    reversion * CEr -
    proph_ongoing * CEs
  
  dCEr.dt <- 
  # biterate * (prob_infection_to_host * fit_adj) * CS * VIr / N - 
    biterate * prob_infection_to_host * fit_adj * CS_C_frac * VIr * (C / N) - 
    gamma_c * CEr + 
    waning * PEr - 
    death_c * CEr -
    reversion * CEr -
    proph_ongoing * CEr
  
  dCIs.dt <- gamma_c * CEs - 
    treatment_q * CIs - 
    treatment_p * CIs - 
    sigma_c  * CIs + 
    waning * PIs + 
    waning * PPs - 
    death_c * CIs +
    reversion * CIr -
    proph_ongoing * CIs
  
  dCIr.dt <- gamma_c * CEr - 
    treatment_q * CIr - 
    treatment_p * CIr - 
    sigma_c  * CIr + 
    waning * PIr +
    waning * PPr -  
    death_c * CIr -
    reversion * CIr -
    proph_ongoing * CIr
  
  dCTs.dt <- treatment_q * CIs - 
    sigma_st  * CTs - 
    emergence_q * CTs + 
    waning * PTs - 
    death_c * CTs +
    reversion * CTr 
  
  dCTr.dt <- treatment_q * CIr - 
    (sigma_c * rec_adj) * CTr  + 
    emergence_q * CTs + 
    waning * PTr - 
    death_c * CTr -
    reversion * CTr 
  
  
  
  dPF.dt <- birth_c * (prop_prophylaxis_at_birth) * PC -                        # Adding new prophylactically treated cattle
  # biterate * (prob_infection_to_host * fit_adj) * PF * VIr / N +              # Infection of resistant strain
    biterate * prob_infection_to_host * fit_adj * PF_P_frac * VIr * (P / N) +
    sigma_st  * PPs +                                     # sigma from treated (prophylactic) sensitive strain infection
    (sigma_c * rec_adj)  * PPr -                            # sigma from treated (prophylactic) resistant strain infection
    waning_f2s * PF -                                        # Waning prophylaxis from fully protected to partially protected
    death_c * PF + 
    proph_ongoing * PS +
    proph_ongoing * PEs + 
    proph_ongoing * PEr +
    proph_ongoing * CS +
    proph_ongoing * CEs +
    proph_ongoing * CEr

  
  dPS.dt <-  waning_f2s * PF -                               # Waning of prophylactically treated cattle to semi protected
  # biterate * partial_susceptibility * prob_infection_to_host * PS * VIs / N -               # Infection of sensitive strain
    biterate * partial_susceptibility * prob_infection_to_host * PS_P_frac * VIs * (P / N) - 
  # biterate * prob_infection_to_host * fit_adj * PS * VIr / N +   # Infection of resistant strain
    biterate * prob_infection_to_host * fit_adj * PS_P_frac * VIr * (P / N) +
    sigma_c  * PIs +                                        # sigma_c from sensitive strain infection
    sigma_c  * PIr +                                        # sigma_c from resistant strain infection
    sigma_st  * PTs +                                     # sigma from treated (quick acting) sensitive strain infection
    (sigma_c * rec_adj)  * PTr   -                          # sigma from treated (quick acting) resistant strain infection
    waning * PS -                                            # Waning of infection to non-prophylactic class
    death_c * PS -                                              # Death of prophylactic susceptibles (partially protected)
    proph_ongoing * PS
    
  dPEs.dt <- 
  # biterate * partial_susceptibility * prob_infection_to_host * PS * VIs / N -      # Infection of sensitive strain
    biterate * partial_susceptibility * prob_infection_to_host * PS_P_frac * VIs * (P / N) -
    gamma_c * PEs -                                   # Movement from exposed to infectious
    emergence_p * PEs -
    waning *PEs -                                            # Waning of infection to non-prophylactic class
    death_c * PEs +                                             # Death of prophylactic exposed (sensitive strain)
    reversion * PEr -
    proph_ongoing * PEs
  
  dPEr.dt <- 
  # biterate * prob_infection_to_host * fit_adj * PS *VIr / N +    # Infection of resistant strain
    biterate * prob_infection_to_host * fit_adj * PS_P_frac * VIr * (P / N) +
  # biterate * (prob_infection_to_host * fit_adj * 1) * PF * VIr / N -   # Infection of resistant strain
    biterate * prob_infection_to_host * fit_adj * PF_P_frac * VIr * (P / N) -
    gamma_c * PEr +                                   # Movement from exposed to infectious
    emergence_p * PEs -
    waning * PEr -                                           # Waning of infection to non-prophylactic class
    death_c * PEr -                                             # Death of prophylactic exposed (resistant strain)
    reversion * PEr -
    proph_ongoing * PEr
    
  dPIs.dt <- gamma_c * PEs -                                   # Movement from exposed to infectious
    treatment_q * PIs -                                      # Treatment with quick acting drug
    treatment_p * PIs -                                      # Treatment with prophylactic acting drug 
    sigma_c  * PIs -                                        # sigma from sensitive strain infection
    emergence_p * PIs -                                        # Emergence of AMR
    waning * PIs -                                           # Waning of infection to non-prophylactic class
    death_c * PIs +                                             # Death of prophylactic infectious (sensitive strain)
    reversion * PIr -
    proph_ongoing * PIs
    
  dPIr.dt <- gamma_c * PEr -                                   # Movement from exposed to infectious 
    treatment_q * PIr -                                      # Treatment with quick acting drug 
    treatment_p * PIr -                                      # Treatment with prophylactic acting drug 
    sigma_c  * PIr +                                        # sigma from resistant strain infection
    emergence_p * PIs -                                        # Emergence of AMR 
    waning * PIr -                                           # Waning of infection to non-prophylactic class
    death_c * PIr -                                             # Death of prophylactic infectious (resistant strain)
    reversion * PIr -
    proph_ongoing * PIr
    
    
  dPTs.dt <- treatment_q * PIs -                                      # Treatment with quick acting drug 
    sigma_st  * PTs -                                     # sigma from sensitive strain infection (quick acting treatment)
    emergence_p * PTs -    
    emergence_q * PTs -                                        # Emergence of AMR  
    waning * PTs -                                           # Waning of infection to non-prophylactic class 
    death_c * PTs +                                              # Death of sensitive treated (quick acting)
    reversion * PTr 
    
  dPTr.dt <- treatment_q * PIr -                                      # Treatment with quick acting drug  
    (sigma_c * rec_adj)  * PTr +                            # Treatment with prophylactic acting drug  
    emergence_p * PTs +    
    emergence_q * PTs  -                                       # Emergence of AMR 
    waning * PTr -                                           # Waning of infection to non-prophylactic class  
    death_c * PTr -                                             # Death of sensitive treated (prophylactic)  
    reversion * PTr 
    
  dPPs.dt <- treatment_p * PIs +                                      # Treatment with prophylactic acting drug  
    treatment_p * CIs -                                      # Treatment with prophylactic acting drug  
    emergence_p * PPs -
    sigma_st  * PPs -                                     # sigma from sensitive strain infection (prophylactic treatment) 
    waning * PPs -                                           # Waning of infection to non-prophylactic class 
    death_c* PPs +                                              # Death of sensitive treated (prophylactic)  
    reversion * PPr + 
    proph_ongoing * PIs +
    proph_ongoing * CIs 
    
    
  dPPr.dt <- treatment_p * PIr +                                      # Treatment with prophylactic acting drug   
    treatment_p * CIr +                                      # Treatment with prophylactic acting drug   
    emergence_p * PPs -
    (sigma_c * rec_adj)  * PPr -                            # sigma from resistant strain infection (prophylactic treatment)  
    waning * PPr -                                           # Waning of infection to non-prophylactic class 
    death_c* PPr -                                         # Death of resistant treated (prophylactic) 
    reversion * PPr + 
    proph_ongoing * PIr +
    proph_ongoing * CIr 
  
  # Wildlife ----
  # 
  # WS, WEs, WEr, WIs, WIr, WTs, WTr
  
  dWS.dt <- birth_w * W - 
  # biterate * prob_infection_to_host * WS * VIs / N -
    biterate * prob_infection_to_host * WS_W_frac * VIs * (W / N) -
  # biterate * (prob_infection_to_host * fit_adj) * WS * VIr / N  - 
    biterate * prob_infection_to_host * fit_adj * WS_W_frac * VIr * (W / N)  - 
    death_w * WS + 
    sigma_w * WIs + 
    sigma_w * WIr 
  
  dWEs.dt <- 
  # biterate * prob_infection_to_host * WS * VIs / N - 
    biterate * prob_infection_to_host * WS_W_frac * VIs * (W / N) -
    gamma_w * WEs - 
    death_w * WEs 
  
  dWEr.dt <- 
  # biterate * (prob_infection_to_host * fit_adj) * WS * VIr / N - 
    biterate * prob_infection_to_host * fit_adj * WS_W_frac * VIr * (W / N) - 
    gamma_w * WEr - 
    death_w * WEr 
  
  dWIs.dt <- gamma_w * WEs - sigma_w * WIs - death_w * WIs + reversion * WIr
  
  dWIr.dt <- gamma_w * WEr - sigma_w * WIr - death_w * WIr - reversion * WIr
  
  # Tsetse ----
  # 
  # VS, VEs, VEr, VIs, VIr, 
  
  dVSt.dt <- birth_v * V  * (1 - V / K ) - 
    # prob_infection_to_vector * biterate * (CIs/N) * VSt -
    # prob_infection_to_vector * biterate * (CIr/N) * VSt -
    # prob_infection_to_vector * biterate * (CTs/N) * VSt -
    # prob_infection_to_vector * biterate * (CTr/N) * VSt -
    # prob_infection_to_vector * biterate * (PIs/N) * VSt -
    # prob_infection_to_vector * biterate * (PIr/N) * VSt -
    # prob_infection_to_vector * biterate * (PPs/N) * VSt - 
    # prob_infection_to_vector * biterate * (PPr/N) * VSt -  
    # prob_infection_to_vector * biterate * (PTs/N) * VSt -  
    # prob_infection_to_vector * biterate * (PTr/N) * VSt -
    # prob_infection_to_vector * biterate * (WIs/N) * VSt -
    # prob_infection_to_vector * biterate * (WIr/N) * VSt -
    prob_infection_to_vector * biterate * CIs_C_frac * (C / N) * VSt -
    prob_infection_to_vector * biterate * CIr_C_frac * (C / N) * VSt -
    prob_infection_to_vector * biterate * CTs_C_frac * (C / N) * VSt -
    prob_infection_to_vector * biterate * CTr_C_frac * (C / N) * VSt -
    prob_infection_to_vector * biterate * PIs_P_frac * (P / N) * VSt -
    prob_infection_to_vector * biterate * PIr_P_frac * (P / N) * VSt -
    prob_infection_to_vector * biterate * PTs_P_frac * (P / N) * VSt - 
    prob_infection_to_vector * biterate * PTr_P_frac * (P / N) * VSt -  
    prob_infection_to_vector * biterate * PPs_P_frac * (P / N) * VSt -  
    prob_infection_to_vector * biterate * PPr_P_frac * (P / N) * VSt -
    prob_infection_to_vector * biterate * WIs_W_frac * (W / N) * VSt -
    prob_infection_to_vector * biterate * WIr_W_frac * (W / N) * VSt -
    ten2fed * VSt -
    death_v * VSt 
  
  dVSf.dt <- ten2fed * VSt - 
    # prob_infection_to_vector * biterate * (CIs/N) * VSf - 
    # prob_infection_to_vector * biterate * (CIr/N) * VSf -
    # prob_infection_to_vector * biterate * (CTs/N) * VSf -
    # prob_infection_to_vector * biterate * (CTr/N) * VSf -
    # prob_infection_to_vector * biterate * (PIs/N) * VSf -
    # prob_infection_to_vector * biterate * (PIr/N) * VSf -
    # prob_infection_to_vector * biterate * (PPs/N) * VSf -  
    # prob_infection_to_vector * biterate * (PPr/N) * VSf -  
    # prob_infection_to_vector * biterate * (PTs/N) * VSf -  
    # prob_infection_to_vector * biterate * (PTr/N) * VSf -
    # prob_infection_to_vector * biterate * (WIs/N) * VSf -
    # prob_infection_to_vector * biterate * (WIr/N) * VSf -
    prob_infection_to_vector * biterate * CIs_C_frac * (C / N) * VSf -
    prob_infection_to_vector * biterate * CIr_C_frac * (C / N) * VSf -
    prob_infection_to_vector * biterate * CTs_C_frac * (C / N) * VSf -
    prob_infection_to_vector * biterate * CTr_C_frac * (C / N) * VSf -
    prob_infection_to_vector * biterate * PIs_P_frac * (P / N) * VSf -
    prob_infection_to_vector * biterate * PIr_P_frac * (P / N) * VSf -
    prob_infection_to_vector * biterate * PTs_P_frac * (P / N) * VSf - 
    prob_infection_to_vector * biterate * PTr_P_frac * (P / N) * VSf -  
    prob_infection_to_vector * biterate * PPs_P_frac * (P / N) * VSf -  
    prob_infection_to_vector * biterate * PPr_P_frac * (P / N) * VSf -
    prob_infection_to_vector * biterate * WIs_W_frac * (W / N) * VSf -
    prob_infection_to_vector * biterate * WIr_W_frac * (W / N) * VSf -
    death_v * VSf
  
  dVEs.dt <-  + 
    # prob_infection_to_vector * biterate * (CIs/N) * VSt +
    # prob_infection_to_vector * biterate * (CTs/N) * VSt +
    # prob_infection_to_vector * biterate * (PIs/N) * VSt +
    # prob_infection_to_vector * biterate * (PPs/N) * VSt +  
    # prob_infection_to_vector * biterate * (PTs/N) * VSt +
    # prob_infection_to_vector * biterate * (WIs/N) * VSt +
    # prob_infection_to_vector * biterate * (CIs/N) * VSf +
    # prob_infection_to_vector * biterate * (CTs/N) * VSf +
    # prob_infection_to_vector * biterate * (PIs/N) * VSf +
    # prob_infection_to_vector * biterate * (PPs/N) * VSf +  
    # prob_infection_to_vector * biterate * (PTs/N) * VSf +
    # prob_infection_to_vector * biterate * (WIs/N) * VSf -
    prob_infection_to_vector * biterate * CIs_C_frac * (C / N) * VSt +
    prob_infection_to_vector * biterate * CTs_C_frac * (C / N) * VSt +
    prob_infection_to_vector * biterate * PIs_P_frac * (P / N) * VSt +
    prob_infection_to_vector * biterate * PTs_P_frac * (P / N) * VSt + 
    prob_infection_to_vector * biterate * PPs_P_frac * (P / N) * VSt +  
    prob_infection_to_vector * biterate * WIs_W_frac * (W / N) * VSt +
    prob_infection_to_vector * biterate * CIs_C_frac * (C / N) * VSf +
    prob_infection_to_vector * biterate * CTs_C_frac * (C / N) * VSf +
    prob_infection_to_vector * biterate * PIs_P_frac * (P / N) * VSf +
    prob_infection_to_vector * biterate * PTs_P_frac * (P / N) * VSf + 
    prob_infection_to_vector * biterate * PPs_P_frac * (P / N) * VSf +  
    prob_infection_to_vector * biterate * WIs_W_frac * (W / N) * VSf -
    gamma_v * VEs - death_v * VEs
  
  dVEr.dt <-
    # prob_infection_to_vector * biterate * (CIr/N) * VSt +
    # prob_infection_to_vector * biterate * (CTr/N) * VSt +
    # prob_infection_to_vector * biterate * (PIr/N) * VSt +
    # prob_infection_to_vector * biterate * (PPr/N) * VSt +  
    # prob_infection_to_vector * biterate * (PTr/N) * VSt +
    # prob_infection_to_vector * biterate * (WIr/N) * VSt +
    # prob_infection_to_vector * biterate * (CIr/N) * VSf +
    # prob_infection_to_vector * biterate * (CTr/N) * VSf +
    # prob_infection_to_vector * biterate * (PIr/N) * VSf +
    # prob_infection_to_vector * biterate * (PPr/N) * VSf +  
    # prob_infection_to_vector * biterate * (PTr/N) * VSf +
    # prob_infection_to_vector * biterate * (WIr/N) * VSf -
    prob_infection_to_vector * biterate * CIr_C_frac * (C / N) * VSt +
    prob_infection_to_vector * biterate * CTr_C_frac * (C / N) * VSt +
    prob_infection_to_vector * biterate * PIr_P_frac * (P / N) * VSt +
    prob_infection_to_vector * biterate * PTr_P_frac * (P / N) * VSt + 
    prob_infection_to_vector * biterate * PPr_P_frac * (P / N) * VSt +  
    prob_infection_to_vector * biterate * WIr_W_frac * (W / N) * VSt +
    prob_infection_to_vector * biterate * CIr_C_frac * (C / N) * VSf +
    prob_infection_to_vector * biterate * CTr_C_frac * (C / N) * VSf +
    prob_infection_to_vector * biterate * PIr_P_frac * (P / N) * VSf +
    prob_infection_to_vector * biterate * PTr_P_frac * (P / N) * VSf + 
    prob_infection_to_vector * biterate * PPr_P_frac * (P / N) * VSf +  
    prob_infection_to_vector * biterate * WIr_W_frac * (W / N) * VSf -
    gamma_v * VEr - death_v * VEr
  
  dVIs.dt <- gamma_v * VEs - death_v * VIs
  
  dVIr.dt <- gamma_v * VEr - death_v * VIr
  
  # Model output ----
   dX <- c(dCS.dt, dCEs.dt, dCEr.dt, dCIs.dt, dCIr.dt, dCTs.dt, dCTr.dt,  
          dPF.dt, dPS.dt, dPEs.dt, dPEr.dt, dPIs.dt, dPIr.dt, dPTs.dt, dPTr.dt, dPPs.dt, dPPr.dt,
          dWS.dt, dWEs.dt, dWEr.dt, dWIs.dt, dWIr.dt, 
          dVSt.dt, dVSf.dt, dVEs.dt, dVEr.dt, dVIs.dt, dVIr.dt)
  #dX <- c(dCS.dt, dCEs.dt, 0.0, dCIs.dt, 0.0, dCTs.dt, 0.0,  
  #        dPF.dt, dPS.dt, dPEs.dt, 0.0, dPIs.dt, 0.0, dPTs.dt, 0.0, dPPs.dt, 0.0,
  #        dWS.dt, dWEs.dt, 0.0, dWIs.dt, 0.0, 
   #       dVSt.dt, dVSf.dt, dVEs.dt, 0.0, dVIs.dt, 0.0)
  list(dX)
  
  
}


findGlobals(fun = AAT_AMR_dens_dep, merge = FALSE)$variables
