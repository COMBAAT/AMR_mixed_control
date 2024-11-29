#-------------------------------------------------------------------------------
# Function Name: R_calc_sen_or_res
#
# Description:
#   This function calculates the basic reproduction number (R0) or the reproduction number (R)
#   for either sensitive or resistant strains of a disease within a host-vector population model.
#   It incorporates various epidemiological parameters to compute the transmission potential
#   via different routes (cattle, prophylactic cattle, and wildlife) and integrates the impact
#   of treatments and prophylaxis.
#
# Parameters:
#   params - A named vector or list containing the model parameters, including transmission rates,
#            treatment efficacy, and demographic factors.
#   Nc - Number of cattle susceptible to infection.
#   Npf - Number of prophylactic cattle fully protected from infection.
#   Nps - Number of prophylactic cattle partially protected from infection.
#   Nw - Number of wildlife susceptible to infection.
#   Nv - Number of vectors capable of transmitting the infection.
#   is_strain_sensitive - A string ("yes" or "no") indicating whether the strain is sensitive or resistant.
#   basic - A string ("yes" or "no") indicating whether to calculate the basic reproduction number (R0) or the reproduction number (R).
#
# Returns:
#   A numeric value representing the calculated reproduction number (R0 or R) for the specified strain
#   within the host-vector population.
#
# Example of use:
#   params <- c(NC = 1000, NW = 500, biterate = 0.5, prob_infection_to_host = 0.1, ...)
#   R0sen <- R_calc_sen_or_res(params, Nc = 1000, Npf = 500, Nps = 300, Nw = 200, Nv = 100, is_strain_sensitive = "yes", basic = "yes")
#
# Dependencies:
#   The function relies on the presence of the necessary epidemiological parameters within the `params`
#   vector or list. It assumes that all required parameters are provided and correctly named.
#
#-------------------------------------------------------------------------------


R_calc_sen_or_res <- function(params, Nc, Npf, Nps, Nw, Nv, is_strain_sensitive, basic) {
  NH <- params["NH"]
  
  biterate <- params["biterate"]
  prob_infection_to_host <- params["prob_infection_to_host"]
  partial_susceptibility_proph_cattle <- params["partial_susceptibility_proph_cattle"]
  prob_infection_to_vector <- params["prob_infection_to_vector"]
  fit_adj <- params["fit_adj"]
  sigma_c <- params["sigma_c"]
  sigma_st <- params["sigma_st"]
  gamma_w <- params["gamma_w"]
  death_w <- params["death_w"]
  sigma_w <- params["sigma_w"]
  gamma_v <- params["gamma_v"]
  death_v <- params["death_v"]
  
  if (is_strain_sensitive == "yes") {
    sigma_treated <- sigma_st
  }
  if (is_strain_sensitive == "no") {
    sigma_treated <- sigma_c
  }
  if (is_strain_sensitive == "no") {
    prob_infection_to_host <- prob_infection_to_host * fit_adj
  }
  
  rate_vectors_infected <- biterate * prob_infection_to_vector * Nv / NH * gamma_v / (gamma_v + death_v)
  
  #transition_probabilities <- create_named_vector_of_transition_probabilities(params, is_strain_sensitive)
  transition_probabilities <- create_named_vector_of_all_transition_probabilities(params, is_strain_sensitive)
  time_in_state <- create_named_vector_of_time_in_state(params, is_strain_sensitive)
  
  # transmission via C - cattle with no prophylaxis
  # from exposed host to infected vector
  RVC <- calculate_RVC(rate_vectors_infected, time_in_state, transition_probabilities)
  RVC <- as.numeric(RVC)
  
  # from infected vector to exposed host
  RCV <- biterate * prob_infection_to_host * (Nc / NH) * 1 / (death_v)
  RCV <- as.numeric(RCV)
  
  # transmission via P - cattle with prophylaxis
  # from exposed P to infected vector
  RVP <- calculate_RVP(rate_vectors_infected, time_in_state, transition_probabilities)
  RVP <- as.numeric(RVP)
  
  # from infected vector to exposed P
  if (is_strain_sensitive == "yes") {
    RPV <- biterate * partial_susceptibility_proph_cattle * prob_infection_to_host * Nps / NH * 1 / (death_v)
  }
  if (is_strain_sensitive == "no") {
    RPV <- biterate * prob_infection_to_host * ((Nps + Npf) / NH) * 1 / (death_v)
  }
  RPV <- as.numeric(RPV)
  
  # transmission via W
  # from infected wildlife to infected vector
  RVW <- biterate * prob_infection_to_vector * Nv / NH * 1 / (sigma_w + death_w) * gamma_v / (gamma_v + death_v)
  RVW <- as.numeric(RVW)
  
  # from infected vector to infected wildlife
  RWV <- biterate * prob_infection_to_host * Nw / NH * gamma_w / (gamma_w + death_w) * 1 / (death_v)
  RWV <- as.numeric(RWV)
  
  reproduction_number <- RCV * RVC + RPV * RVP + RWV * RVW
  reproduction_number
}




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
