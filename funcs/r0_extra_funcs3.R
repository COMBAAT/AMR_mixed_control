
  calculate_loop_probabilities <- function(transition_probabilities, is_strain_sensitive) {
    loop_probabiities <- with(as.list(transition_probabilities, is_strain_sensitive), {
      
      p1 <- prob_CI_treat_q
      p2 <- prob_CI_treat_p + prob_proph_from_CI
      p3 <- prob_waning_from_partial_protection_from_PP
      p4 <- prob_waning_from_partial_protection_from_PI
      p5 <- prob_PI_treat_p + prob_proph_from_PI
      pCIloopCI <- p2 * p3 * p4 / (1 - p5 * p3)
      
      list(p1 = p1, p2 = p2, p3 = p3, p4 = p4, p5 = p5, pCIloopCI = pCIloopCI)
    })
  loop_probabiities
}


calculate_time2A <- function(time_in_state, transition_probabilities) {
  time2B <- calculate_time2B(time_in_state, transition_probabilities)
  time2A <- with(as.list(c(time_in_state, transition_probabilities)), {
    
    time2A <- (time_in_CI +
      p1 * time_in_CT +
      p2 * time_in_PP +
      p2 * p3 * time2B) / (1 - pCIloopCI)
    time2A
  })
  time2A
}

calculate_time2B <- function(time_in_state, transition_probabilities) {
  
  time2B <- with(as.list( c(time_in_state, transition_probabilities)  ), {
    time2B <- (time_in_PI +
      prob_PI_treat_q * time_in_PT + prob_PI_treat_q * prob_waning_from_partial_protection_from_PT * time_in_CT +
      p5 * time_in_PP) / (1 - p5 * p3)
    time2B
  })
  time2B
}

#################################################################################
calculate_RVC <- function(rate_vectors_infected, time_in_state, transition_probabilities) {
  time2A <- calculate_time2A(time_in_state, transition_probabilities)
  time2B <- calculate_time2B(time_in_state, transition_probabilities)
  
  RVC <- with(as.list(c(time_in_state, transition_probabilities, rate_vectors_infected, time2A, time2B)), {
    
    component1 <- prob_CI_from_CE * time2A
    
    prob_PP_from_CE <- prob_proph_from_CE * prob_disease_from_CEX
    component2 <- prob_PP_from_CE * time_in_PP + 
           prob_PP_from_CE * prob_waning_from_partial_protection_from_PP * (time2B + time2A * p4 / (1 - p5 * p3))
    
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
  
  component1 <- prob_PI_from_PE * (time2B + time2A * p4 / (1 - p5 * p3))

  prob_PP_from_PE <- prob_proph_from_PE * prob_disease_from_PEX
  component2 <- prob_PP_from_PE * time_in_PP + 
    prob_PP_from_PE * prob_waning_from_partial_protection_from_PP * (time2B + time2A * p4 / (1 - p5 * p3))
  
  component3 <- prob_waning_from_partial_protection_from_PE * RVC

  RVP <- (component1 + component2) * rate_vectors_infected + component3
  RVP <- as.numeric(RVP)
  RVP
  })
  RVP
}


findGlobals(fun = calculate_time2A, merge = FALSE)$variables
findGlobals(fun = calculate_time2B, merge = FALSE)$variables
findGlobals(fun = calculate_RVC, merge = FALSE)$variables
findGlobals(fun = calculate_RVP, merge = FALSE)$variables
