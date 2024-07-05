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
  prob_infection   <- parms["prob_infection"]
  gamma_c          <- parms["gamma_c"]
  death_c          <- parms["death_c"]
  sigma_c          <- parms["sigma_c"]
  treatment_q      <- parms["treatment_q"]
  treatment_p      <- parms["treatment_p"]
  sigma_st         <- parms["sigma_st"]
  emergence_p      <- parms["emergence_p"]  
  emergence_f      <- parms["emergence_f"]
  rec_adj          <- parms["rec_adj"]
  prop_prophylaxis <- parms["prop_prophylaxis"]
  fit_adj          <- parms["fit_adj"]
  waning           <- parms["waning"]
  new_prop         <- parms["new_prop"]    
  waning_f2s       <- parms["waning_f2s"]
  
  ## ----- Wildlife
  birth_w            <- parms["birth_w"]
  prob_infection.s_w <- parms["prob_infection.s_w"]
  prob_infection.r_w <- parms["prob_infection.r_w"]
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
  
  # Cattle ----
  # 
  # CS, CEs, CEr, CIs, CIr, CTs, CTr, CR
  
  dCS.dt <- birth_c * (1 - prop_prophylaxis) * PC +
    waning * PS -
    biterate * prob_infection * CS * VIs / N -  
    biterate * (prob_infection * fit_adj) * CS * VIr / N  + 
    sigma_c  * CIs + 
    sigma_c  * CIr + 
    sigma_st  * CTs +
#    sigma_st  * PPs +  #test addition
    (sigma_c * rec_adj)  * CTr - 
#    new_prop * CS - 
    death_c * CS 
  
  dCEs.dt <- biterate * prob_infection * CS * VIs / N - 
    gamma_c * CEs + 
    waning * PEs - 
    death_c * CEs 
  
  dCEr.dt <- biterate * (prob_infection * fit_adj) * CS * VIr / N - 
    gamma_c * CEr + 
    waning * PEr - 
    death_c * CEr
  
  dCIs.dt <- gamma_c * CEs - 
    treatment_q * CIs - 
    treatment_p * CIs - 
    sigma_c  * CIs + 
    waning * PIs + 
    waning * PPs - #LM moved from CTs equation
    death_c * CIs 
  
  dCIr.dt <- gamma_c * CEr - 
    treatment_q * CIr - 
    treatment_p * CIr - 
    sigma_c  * CIr + 
    waning * PIr +
    waning * PPr -  #LM moved from CTr equation 29/9/22
    death_c * CIr 
  
  dCTs.dt <- treatment_q * CIs - 
    sigma_st  * CTs - 
    emergence_f * CTs + 
    waning * PTs - 
#    waning * PPs - #LM moved up to CIs equation
    death_c * CTs
  
  dCTr.dt <- treatment_q * CIr - 
    (sigma_c * rec_adj) * CTr  + 
    emergence_f * CTs + 
    waning * PTr - 
#    waning *PPr - #LM moved up to CIr equation 29/9/22
    death_c * CTr
  
  
  
  # dCTs.dt <- 0
  # 
  # dCTr.dt <- 0
  # 
  # dCR.dt <- treatment_q * CIs + treatment_q * CIr + sigma_c  * CIs + sigma_c  * CIr + sigma_c  * CTs + 
  #   sigma_c  * CTr - resusceptible * CR - death_c * CR
  
  
  # Cattle with prophylaxis ----
  # 
  # PS, PEs, PEr, PIs, PIr, PTs, PTr, PR
  
  dPF.dt <- birth_c * (prop_prophylaxis) * PC - #new_prop * CS # Adding new prophylactically treated cattle
    biterate * (prob_infection * fit_adj * 1) * PF * VIr / N +   # Infection of resistant strain
    sigma_st  * PPs +                                     # sigma from treated (prophylactic) sensitive strain infection
    (sigma_c * rec_adj)  * PPr -                            # sigma from treated (prophylactic) resistant strain infection
    waning_f2s * PF -                                        # Waning prophylaxis from fully protected to partially protected
    death_c * PF                                               # Death of prophylactic susceptibles (fully protected)
  
  dPS.dt <-  waning_f2s * PF -                               # Waning of prophylactically treated cattle to semi protected
    biterate * prob_infection * PS * VIs / N -               # Infection of sensitive strain
    biterate * (prob_infection * fit_adj) * PS * VIr / N +   # Infection of resistant strain
    sigma_c  * PIs +                                        # sigma_c from sensitive strain infection
    sigma_c  * PIr +                                        # sigma_c from resistant strain infection
    sigma_st  * PTs +                                     # sigma from treated (fast acting) sensitive strain infection
    (sigma_c * rec_adj)  * PTr   -                          # sigma from treated (fast acting) resistant strain infection
    waning * PS -                                            # Waning of infection to non-prophylactic class
    death_c * PS                                               # Death of prophylactic susceptibles (partially protected)
  
  dPEs.dt <- biterate * prob_infection * PS * VIs / N -      # Infection of sensitive strain
    gamma_c * PEs -                                   # Movement from exposed to infectious
    emergence_p * PEs -
    waning *PEs -                                            # Waning of infection to non-prophylactic class
    death_c * PEs                                              # Death of prophylactic exposed (sensitive strain)
  
  dPEr.dt <- biterate * (prob_infection * fit_adj) * PS *VIr / N +    # Infection of resistant strain
    biterate * (prob_infection * fit_adj * 1) * PF * VIr / N -   # Infection of resistant strain
    gamma_c * PEr +                                   # Movement from exposed to infectious
    emergence_p * PEs -
    waning * PEr -                                           # Waning of infection to non-prophylactic class
    death_c * PEr                                              # Death of prophylactic exposed (resistant strain)
  
  dPIs.dt <- gamma_c * PEs -                                   # Movement from exposed to infectious
    treatment_q * PIs -                                      # Treatment with fast acting drug
    treatment_p * PIs -                                      # Treatment with prophylactic acting drug 
    sigma_c  * PIs -                                        # sigma from sensitive strain infection
    emergence_p * PIs -                                        # Emergence of AMR
    waning * PIs -                                           # Waning of infection to non-prophylactic class
    death_c * PIs                                              # Death of prophylactic infectious (sensitive strain)
  
  dPIr.dt <- gamma_c * PEr -                                   # Movement from exposed to infectious 
    treatment_q * PIr -                                      # Treatment with fast acting drug 
    treatment_p * PIr -                                      # Treatment with prophylactic acting drug 
    sigma_c  * PIr +                                        # sigma from resistant strain infection
    emergence_p * PIs -                                        # Emergence of AMR 
    waning * PIr -                                           # Waning of infection to non-prophylactic class
    death_c * PIr                                              # Death of prophylactic infectious (resistant strain)
  
  dPTs.dt <- treatment_q * PIs -                                      # Treatment with fast acting drug 
    sigma_st  * PTs -                                     # sigma from sensitive strain infection (fast acting treatment)
    emergence_p * PTs -    
    emergence_f * PTs -                                        # Emergence of AMR  
    waning * PTs -                                           # Waning of infection to non-prophylactic class 
    death_c * PTs                                              # Death of sensitive treated (fast acting)
  
  dPTr.dt <- treatment_q * PIr -                                      # Treatment with fast acting drug  
    (sigma_c * rec_adj)  * PTr +                            # Treatment with prophylactic acting drug  
    emergence_p * PTs +    
    emergence_f * PTs  -                                       # Emergence of AMR 
    waning * PTr -                                           # Waning of infection to non-prophylactic class  
    death_c * PTr                                              # Death of sensitive treated (prophylactic)  
  
  dPPs.dt <- treatment_p * PIs +                                      # Treatment with prophylactic acting drug  
    treatment_p * CIs -                                      # Treatment with prophylactic acting drug  
    emergence_p * PPs -
    sigma_st  * PPs -                                     # sigma from sensitive strain infection (prophylactic treatment) 
    waning * PPs -                                           # Waning of infection to non-prophylactic class 
    death_c* PPs                                               # Death of sensitive treated (prophylactic)  
  
  dPPr.dt <- treatment_p * PIr +                                      # Treatment with prophylactic acting drug   
    treatment_p * CIr +                                      # Treatment with prophylactic acting drug   
    emergence_p * PPs -
    (sigma_c * rec_adj)  * PPr -                            # sigma from resistant strain infection (prophylactic treatment)  
    waning * PPr -                                           # Waning of infection to non-prophylactic class 
    death_c* PPr                                               # Death of resistant treated (prophylactic) 
  
  
  # Wildlife ----
  # 
  # WS, WEs, WEr, WIs, WIr, WTs, WTr
  
  dWS.dt <- birth_w * W - biterate * prob_infection * WS * VIs / N - 
    biterate * (prob_infection * fit_adj) * WS * VIr / N  - 
    death_w * WS + sigma_w * WIs + sigma_w * WIr 
  
  dWEs.dt <- biterate * prob_infection * WS * VIs / N - gamma_w * 
    WEs - death_w * WEs 
  
  dWEr.dt <- biterate * (prob_infection * fit_adj) * WS * VIr / N - gamma_w * 
    WEr - death_w * WEr 
  
  dWIs.dt <- gamma_w * WEs - sigma_w * WIs - death_w * WIs + reversion * WIr
  
  dWIr.dt <- gamma_w * WEr - sigma_w * WIr - death_w * WIr - reversion * WIr
  
  # if (W < 1.0-8){
  #   dWS.dt <- 0
  #   dWEs.dt <- 0
  #   dWEr.dt <- 0
  #   dWIs.dt <- 0
  #   dWIr.dt <- 0
  # }
  # 
  
  # Tsetse ----
  # 
  # VS, VEs, VEr, VIs, VIr, 
  
  dVSt.dt <- birth_v * V  * (1 - V / K ) - 
    prob_infection_to_vector * biterate * (CIs/N) * VSt -
    prob_infection_to_vector * biterate * (CIr/N) * VSt -
    prob_infection_to_vector * biterate * (CTs/N) * VSt -
    prob_infection_to_vector * biterate * (CTr/N) * VSt -
    prob_infection_to_vector * biterate * (PIs/N) * VSt -
    prob_infection_to_vector * biterate * (PIr/N) * VSt -
    prob_infection_to_vector * biterate * (PPs/N) * VSt -  #LM
    prob_infection_to_vector * biterate * (PPr/N) * VSt -  #LM
    prob_infection_to_vector * biterate * (PTs/N) * VSt -  #LM
    prob_infection_to_vector * biterate * (PTr/N) * VSt -
    prob_infection_to_vector * biterate * (WIs/N) * VSt -
    prob_infection_to_vector * biterate * (WIr/N) * VSt -
#    prob_infection_to_vector * biterate * (CIs + CIr + CTs +CTr + PIs + PIr + PPs + PPr + PTs + PTr + WIs + WIr) / N * VSt -
    ten2fed * VSt -
    death_v * VSt 
  
  dVSf.dt <- - 
    prob_infection_to_vector * biterate * (CIs/N) * VSf - #LM missing minus at start of line
    prob_infection_to_vector * biterate * (CIr/N) * VSf -
    prob_infection_to_vector * biterate * (CTs/N) * VSf -
    prob_infection_to_vector * biterate * (CTr/N) * VSf -
    prob_infection_to_vector * biterate * (PIs/N) * VSf -
    prob_infection_to_vector * biterate * (PIr/N) * VSf -
    prob_infection_to_vector * biterate * (PPs/N) * VSf -  #LM addition
    prob_infection_to_vector * biterate * (PPr/N) * VSf -  #LM addition
    prob_infection_to_vector * biterate * (PTs/N) * VSf -  #LM addition
    prob_infection_to_vector * biterate * (PTr/N) * VSf -
    prob_infection_to_vector * biterate * (WIs/N) * VSf -
    prob_infection_to_vector * biterate * (WIr/N) * VSf +
#  prob_infection_to_vector * biterate * (CIs + CIr + CTs +CTr + PIs + PIr + PPs + PPr + PTs + PTr + WIs + WIr) / N * VSf +
    ten2fed * VSt -
    death_v * VSf
  
  dVEs.dt <-  + 
    prob_infection_to_vector * biterate * (CIs/N) * VSt +
    prob_infection_to_vector * biterate * (CTs/N) * VSt +
    prob_infection_to_vector * biterate * (PIs/N) * VSt +
    prob_infection_to_vector * biterate * (PPs/N) * VSt +  #LM addition
    prob_infection_to_vector * biterate * (PTs/N) * VSt +
    prob_infection_to_vector * biterate * (WIs/N) * VSt +
    prob_infection_to_vector * biterate * (CIs/N) * VSf +
    prob_infection_to_vector * biterate * (CTs/N) * VSf +
    prob_infection_to_vector * biterate * (PIs/N) * VSf +
    prob_infection_to_vector * biterate * (PPs/N) * VSf +  #LM addition
    prob_infection_to_vector * biterate * (PTs/N) * VSf +
    prob_infection_to_vector * biterate * (WIs/N) * VSf -
    #prob_infection_to_vector * biterate * (CIs + 0*CIr + CTs + 0*CTr + PIs + 0*PIr + PPs + 0*PPr + PTs + 0*PTr + WIs + 0*WIr) / N * VSf +
    #prob_infection_to_vector * biterate * (CIs + 0*CIr + CTs + 0*CTr + PIs + 0*PIr + PPs + 0*PPr + PTs + 0*PTr + WIs + 0*WIr) / N * VSt -
    gamma_v * VEs - death_v *VEs
  
  dVEr.dt <-  + 
    prob_infection_to_vector * biterate * (CIr/N) * VSt +
    prob_infection_to_vector * biterate * (CTr/N) * VSt +
    prob_infection_to_vector * biterate * (PIr/N) * VSt +
    prob_infection_to_vector * biterate * (PPr/N) * VSt +  #LM addition
    prob_infection_to_vector * biterate * (PTr/N) * VSt +
    prob_infection_to_vector * biterate * (WIr/N) * VSt +
    prob_infection_to_vector * biterate * (CIr/N) * VSf +
    prob_infection_to_vector * biterate * (CTr/N) * VSf +
    prob_infection_to_vector * biterate * (PIr/N) * VSf +
    prob_infection_to_vector * biterate * (PPr/N) * VSf +  #LM addition, corrected VSt to VSf 29/9/2022
    prob_infection_to_vector * biterate * (PTr/N) * VSf +
    prob_infection_to_vector * biterate * (WIr/N) * VSf -
#    prob_infection_to_vector * biterate * (0*CIs + 1*CIr + 0*CTs + 1*CTr + 0*PIs + 1*PIr + 0*PPs + 1*PPr + 0*PTs + 1*PTr + 0*WIs + 1*WIr) / N * VSf +
#    prob_infection_to_vector * biterate * (0*CIs + 1*CIr + 0*CTs + 1*CTr + 0*PIs + 1*PIr + 0*PPs + 1*PPr + 0*PTs + 1*PTr + 0*WIs + 1*WIr) / N * VSt -
    gamma_v * VEr - death_v *VEr
  
  dVIs.dt <- gamma_v * VEs - death_v * VIs
  
  dVIr.dt <- gamma_v * VEr - death_v * VIr
  
  # Model output ----
  #dX <- c(dCS.dt, dCEs.dt, dCEr.dt, dCIs.dt, dCIr.dt, dCTs.dt, dCTr.dt,  
  #        dPF.dt, dPS.dt, dPEs.dt, dPEr.dt, dPIs.dt, dPIr.dt, dPTs.dt, dPTr.dt, dPPs.dt, dPPr.dt,
  #        dWS.dt, dWEs.dt, dWEr.dt, dWIs.dt, dWIr.dt, 
  #        dVSt.dt, dVSf.dt, dVEs.dt, dVEr.dt, dVIs.dt, dVIr.dt)
  dX <- c(dCS.dt, dCEs.dt, 0.0, dCIs.dt, 0.0, dCTs.dt, 0.0,  
          dPF.dt, dPS.dt, dPEs.dt, 0.0, dPIs.dt, 0.0, dPTs.dt, 0.0, dPPs.dt, 0.0,
          dWS.dt, dWEs.dt, 0.0, dWIs.dt, 0.0, 
          dVSt.dt, dVSf.dt, dVEs.dt, 0.0, dVIs.dt, 0.0)
  list(dX)
  
  
}


findGlobals(fun = AAT_AMR_dens_dep, merge = FALSE)$variables
