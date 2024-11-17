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

calculate_time2A <- function(prob_CI_treat_q, prob_CI_treat_p, prob_proph_from_CI, time_in_CI, time_in_CT, time_in_PP) {
  time2A <- (time_in_CI +
    prob_CI_treat_q * time_in_CT +
    prob_CI_treat_p * time_in_PP +
    prob_proph_from_CI * time_in_PP)
  time2A
}

calculate_time2B <- function(prob_PT_treat_p, prob_waning_from_partial_protection_from_PT,
                             time_in_PT, time_in_PI, prob_PI_treat_q, prob_PI_treat_p,
                             prob_proph_from_PI,
                             rate_vectors_infected, time_in_PP,
                             prob_waning_from_partial_protection_from_PP,
                             proph_ongoing, waning_from_partial_protection, time_in_CT) {
  time2A <- calculate_time2A(prob_CI_treat_q, prob_CI_treat_p, prob_proph_from_CI, time_in_CI, time_in_CT, time_in_PP)

  # sum of probabilities of transitioning from PI to PP, from PI to PT, and from PI to CI
  p1 <- prob_PI_treat_p + prob_proph_from_PI + prob_waning_from_partial_protection_from_PI * (prob_CI_treat_p + prob_proph_from_CI)
  # sum of probabilities of transitioning from PP to PI
  p2 <- prob_waning_from_partial_protection_from_PP

  time2B <- (time_in_PI +
    prob_waning_from_partial_protection_from_PI * time2A +
    prob_PI_treat_q * time_in_PT +
    prob_PI_treat_q * prob_waning_from_partial_protection_from_PT * time_in_CT +
    prob_PI_treat_p * time_in_PP +
    prob_proph_from_PI * time_in_PP) / (1 - p1 * p2)
  time2B
}

#################################################################################
calculate_RVC2 <- function(time_in_CI, prob_CI_treat_q, prob_CI_treat_p, prob_proph_from_CI, prob_proph_from_PE, prob_disease_from_CEX,
                           rate_vectors_infected, p1c, p2c, prob_waning_from_partial_protection_from_PP, time_in_PP, prob_proph_from_CE,
                           prob_CI_from_CE, time_in_CT) {
  time2A <- calculate_time2A(prob_CI_treat_q, prob_CI_treat_p, prob_proph_from_CI, time_in_CI, time_in_CT, time_in_PP)

  component1 <- prob_CI_from_CE * (time2A +
    (prob_CI_treat_p + prob_proph_from_CI) * prob_waning_from_partial_protection_from_PP * time_2B)

  prob_PP_from_CE <- prob_proph_from_CE * prob_disease_from_CEX

  component2 <- prob_PP_from_CE * time_in_PP + prob_PP_from_CE * prob_waning_from_partial_protection_from_PP * time_2B

  # transmission via C
  RVC <- (component1 + component2) * rate_vectors_infected
  RVC <- as.numeric(RVC)
  RVC
}

calculate_RPV2 <- function(prob_PI_from_PE, prob_waning_from_partial_protection_from_PE, prob_proph_from_PE,
                           prob_disease_from_PEX, prob_PP_from_PE,
                           prob_waning_from_partial_protection_from_PP,
                           prob_PT_treat_p, prob_waning_from_partial_protection_from_PT,
                           time_in_PT, time_in_PI, prob_PI_treat_q, prob_PI_treat_p,
                           prob_proph_from_PI,
                           rate_vectors_infected, time_in_PP,
                           proph_ongoing, waning_from_partial_protection, time_in_CT) {
  time_2B <- calculate_time2B(
    prob_PT_treat_p, prob_waning_from_partial_protection_from_PT,
    time_in_PT, time_in_PI, prob_PI_treat_q, prob_PI_treat_p,
    prob_proph_from_PI,
    rate_vectors_infected, time_in_PP,
    prob_waning_from_partial_protection_from_PP,
    proph_ongoing, waning_from_partial_protection, time_in_CT
  )

  component1 <- prob_PI_from_PE * time_2B
  component2 <- prob_waning_from_partial_protection_from_PE * RVC2

  prob_PP_from_PE <- prob_proph_from_PE * prob_disease_from_PEX
  component3 <- prob_PP_from_PE * time_in_PP + prob_PP_from_PE * prob_waning_from_partial_protection_from_PP * time_2B

  RPV <- (component1 + component2 + component3) * rate_vectors_infected
  RPV <- as.numeric(RPV)
  RPV
}

findGlobals(fun = calculate_time2A, merge = FALSE)$variables
findGlobals(fun = calculate_time2B, merge = FALSE)$variables
findGlobals(fun = calculate_RVC2, merge = FALSE)$variables
findGlobals(fun = calculate_RPV2, merge = FALSE)$variables
findGlobals(fun = calculate_RVC, merge = FALSE)$variables
findGlobals(fun = calculate_RVP, merge = FALSE)$variables
findGlobals(fun = calculate_R1, merge = FALSE)$variables
