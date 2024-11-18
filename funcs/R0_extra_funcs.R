calculate_loop_probabilities <- function(params, is_strain_sensitive) {
  loop_probabiities <- with(as.list(params, is_strain_sensitive), {
    
    if (is_strain_sensitive == "yes") {
      sigma_treated <- sigma_st
    }
    if (is_strain_sensitive == "no") {
      sigma_treated <- sigma_c
    }
    
    # Probability of I -> Tp
    p1c <- (treatment_p + proph_ongoing) / (treatment_p + treatment_q + sigma_c + death_c + proph_ongoing)
    
    # Probability of Tp -> I
    # p2c <- waning_from_partial_protection / (treatment_p + treatment_q + sigma_st + death_c)
    p2c <- waning_from_partial_protection / (waning_from_partial_protection + sigma_treated + death_c) # LM corrected
    list(p1c = p1c, p2c = p2c)
  })
  loop_probabiities
}


calculate_loop_probabilities_alt <- function(transition_probabilities, is_strain_sensitive) {
  loop_probabiities <- with(as.list(transition_probabilities, is_strain_sensitive), {
    
    # Probability of I -> Tp
    #p1c <- (treatment_p + proph_ongoing) / (treatment_p + treatment_q + sigma_c + death_c + proph_ongoing)
    p1c <- prob_PI_treat_p + prob_proph_from_CI
    
    # Probability of Tp -> I
    # p2c <- waning_from_partial_protection / (treatment_p + treatment_q + sigma_st + death_c)
    #p2c <- waning_from_partial_protection / (waning_from_partial_protection + sigma_treated + death_c) # LM corrected
    p2c <- prob_waning_from_partial_protection_from_PP
    list(p1c = p1c, p2c = p2c)
  })
  loop_probabiities
}

#################################################################################
calculate_R1 <- function(rate_vectors_infected, time_in_state, transition_probabilities) {
  time_infectious_route1 <- with(as.list(c(time_in_state, transition_probabilities)), {
    
    result <- (time_in_CI +
                 prob_CI_treat_q * time_in_CT +
                 prob_CI_treat_p * time_in_PP +
                 prob_proph_from_CI * time_in_PP) / (1 - p1c * p2c)
    result
  })
  
  R1 <- rate_vectors_infected * time_infectious_route1
  R1
}

#################################################################################
calculate_RVC <- function(rate_vectors_infected, time_in_state, transition_probabilities) {
  R1 <- calculate_R1(rate_vectors_infected, time_in_state, transition_probabilities)
  
  RVC <- with(as.list(c(time_in_state, transition_probabilities, rate_vectors_infected, R1)), {
    prob_PP_from_CE <- prob_proph_from_CE * prob_disease_from_CEX
    
    # transmission via C
    RVC <- prob_CI_from_CE * R1 + prob_PP_from_CE * prob_waning_from_partial_protection_from_PP * R1 + prob_PP_from_CE * time_in_PP * rate_vectors_infected
    RVC <- as.numeric(RVC)
    RVC
  })
  
  return(RVC)
}

#################################################################################
calculate_RVP <- function(rate_vectors_infected, time_in_state, transition_probabilities) {
  R1 <- calculate_R1(rate_vectors_infected, time_in_state, transition_probabilities)
  RVC <- calculate_RVC(rate_vectors_infected, time_in_state, transition_probabilities)
  
  RVP <- with(as.list(c(time_in_state, transition_probabilities, rate_vectors_infected, R1)), {
    # transmission via P
    # prob_proph_from_PE2 <- proph_ongoing / (gamma_p + death_p + proph_ongoing + waning_from_partial_protection) *
    #  gamma_p / (gamma_p + death_p + sigma_treated)
    prob_PP_from_PE <- prob_proph_from_PE * prob_disease_from_PEX
    
    RVP1 <- rate_vectors_infected * time_in_PI + prob_waning_from_partial_protection_from_PI * R1 + # contribution from PIs
      rate_vectors_infected * prob_PI_treat_q * time_in_PT +
      rate_vectors_infected * prob_PI_treat_q * prob_waning_from_partial_protection_from_PT * time_in_CT +
      
      rate_vectors_infected * prob_PI_treat_p * time_in_PP + # contrib from PPs
      # contribution from waning_from_partial_protection back to CIS
      prob_PI_treat_p * prob_waning_from_partial_protection_from_PP * R1 +
      
      rate_vectors_infected * prob_proph_from_PI * time_in_PP +
      prob_proph_from_PI * prob_waning_from_partial_protection_from_PP * R1
    
    
    RVP2 <- prob_PP_from_PE * time_in_PP * rate_vectors_infected +
      prob_PP_from_PE * prob_waning_from_partial_protection_from_PP * R1
    
    
    RVP <- RVP1 * prob_PI_from_PE + RVP2 + prob_waning_from_partial_protection_from_PE * RVC
    RVP <- as.numeric(RVP)
    RVP
  })
  
  return(RVP)
}

findGlobals(fun = calculate_RVC, merge = FALSE)$variables
findGlobals(fun = calculate_RVP, merge = FALSE)$variables
findGlobals(fun = calculate_R1, merge = FALSE)$variables
