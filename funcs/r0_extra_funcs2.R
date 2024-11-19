calculate_loop_probabilities <- function(transition_probabilities, is_strain_sensitive) {
  loop_probabiities <- with(as.list(transition_probabilities, is_strain_sensitive), {
    
    pCItoPI <- (prob_CI_treat_p + prob_proph_from_CI) * ( 1 + prob_waning_from_partial_protection_from_PP)
    pPItoCI <- prob_waning_from_partial_protection_from_PI
    
    pPItoPP <- prob_PI_treat_p + prob_proph_from_PI + prob_waning_from_partial_protection_from_PI * (prob_CI_treat_p + prob_proph_from_CI)
    pPPtoPI <- prob_waning_from_partial_protection_from_PP
    
    # sum of probabilities of transitioning from PI to PP
    p1c <- prob_PI_treat_p + prob_proph_from_PI + prob_waning_from_partial_protection_from_PI * (prob_CI_treat_p + prob_proph_from_CI)
    # sum of probabilities of transitioning from PP to PI
    p2c <- prob_waning_from_partial_protection_from_PP
    list(p1c = p1c, p2c = p2c, pCItoPI = pCItoPI, pPItoCI = pPItoCI, pPItoPP = pPItoPP, pPPtoPI = pPPtoPI)
  })
  loop_probabiities
}


calculate_time2A <- function(time_in_state, transition_probabilities) {
  time2B <- calculate_time2B(time_in_state, transition_probabilities)
  time2A <- with(as.list(c(time_in_state, transition_probabilities, time2B)), {
    time2A <- (time_in_CI +
      prob_CI_treat_q * time_in_CT +
      prob_CI_treat_p * time_in_PP +
      prob_proph_from_CI * time_in_PP +
      pCItoPI * time2B) / (1 - pCItoPI * pPItoCI)
    time2A
  })
  time2A
}

calculate_time2B <- function(time_in_state, transition_probabilities) {
  
  time2B <- with(as.list( c(time_in_state, transition_probabilities)  ), {
    time2B <- (time_in_PI +
      prob_PI_treat_q * time_in_PT +
      prob_PI_treat_q * prob_waning_from_partial_protection_from_PT * time_in_CT +
      prob_PI_treat_p * time_in_PP +
      prob_proph_from_PI * time_in_PP) / (1 - pPItoPP * pPPtoPI)
    time2B
  })
  time2B
}

#################################################################################
calculate_RVC <- function(rate_vectors_infected, time_in_state, transition_probabilities) {
  time2A <- calculate_time2A(time_in_state, transition_probabilities)
  time2B <- calculate_time2B(time_in_state, transition_probabilities)
  #print(time2A)
  #print(time2B)
  
  RVC <- with(as.list(c(time_in_state, transition_probabilities, rate_vectors_infected, time2A, time2B)), {
    
    #prob_CI_from_CE <- prob_CI_treat_p + prob_proph_from_CI #?
    #print(prob_CI_from_CE)
    component1 <- prob_CI_from_CE * time2A
    
    prob_PP_from_CE <- prob_proph_from_CE * prob_disease_from_CEX
    component2 <- prob_PP_from_CE * time_in_PP + prob_PP_from_CE * prob_waning_from_partial_protection_from_PP * time2B +
      prob_PP_from_CE * prob_waning_from_partial_protection_from_PP * prob_waning_from_partial_protection_from_PI * time2A
    
    # transmission via C
    RVC <- (component1 + component2) * rate_vectors_infected
    RVC <- as.numeric(RVC)
    RVC
  })
  RVC
}

calculate_RVP <- function(rate_vectors_infected, time_in_state, transition_probabilities) {
  time2A <- calculate_time2A(time_in_state, transition_probabilities)
  time2B <- calculate_time2B(time_in_state, transition_probabilities)
  RVC <- calculate_RVC(rate_vectors_infected, time_in_state, transition_probabilities)

  RVP <- with(as.list(c(time_in_state, transition_probabilities, rate_vectors_infected, time2B, RVC)), {
  
  component1 <- prob_PI_from_PE * time2B + prob_PI_from_PE * prob_waning_from_partial_protection_from_PI * time2A
  component2 <- prob_waning_from_partial_protection_from_PE * RVC

  prob_PP_from_PE <- prob_proph_from_PE * prob_disease_from_PEX
  component3 <- prob_PP_from_PE * time_in_PP + prob_PP_from_PE * prob_waning_from_partial_protection_from_PP * time2B +
    prob_PP_from_PE * prob_waning_from_partial_protection_from_PP * prob_waning_from_partial_protection_from_PI * time2A

  RVP <- (component1 + component2 + component3) * rate_vectors_infected
  RVP <- as.numeric(RVP)
  RVP
  })
  RVP
}


findGlobals(fun = calculate_time2A, merge = FALSE)$variables
findGlobals(fun = calculate_time2B, merge = FALSE)$variables
findGlobals(fun = calculate_RVC, merge = FALSE)$variables
findGlobals(fun = calculate_RVP, merge = FALSE)$variables
