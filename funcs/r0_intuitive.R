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
  NC <- params["NC"]
  NW <- params["NW"]
  
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
  
  Nc_frac <- calc_frac_with_zero(Nc, NC)
  Nps_frac <- calc_frac_with_zero(Nps, NC)
  Nps_Npf_frac <- calc_frac_with_zero(Nps + Npf, NC)
  Nw_frac <- calc_frac_with_zero(Nw, NW)
  Nv_div_NC <- calc_frac_with_zero(Nv, NC)
  Nv_div_NW <- calc_frac_with_zero(Nv, NW)
  
  if (is_strain_sensitive == "yes") {
    sigma_treated <- sigma_st
  }
  if (is_strain_sensitive == "no") {
    sigma_treated <- sigma_c
  }
  if (is_strain_sensitive == "no") {
    prob_infection_to_host <- prob_infection_to_host * fit_adj
  }
  
  #rate_vectors_infected <- biterate * prob_infection_to_vector * Nv / NH * gamma_v / (gamma_v + death_v)
  rate_vectors_infected_by_cattle <- biterate * prob_infection_to_vector * Nv_div_NC * gamma_v / (gamma_v + death_v) * bite_frac_cattle(NC, NH)
  #rate_vectors_infected_by_cattle <- biterate * prob_infection_to_vector * Nv / NH * gamma_v / (gamma_v + death_v) 
  
  #transition_probabilities <- create_named_vector_of_transition_probabilities(params, is_strain_sensitive)
  transition_probabilities <- create_named_vector_of_all_transition_probabilities(params, is_strain_sensitive)
  time_in_state <- create_named_vector_of_time_in_state(params, is_strain_sensitive)
  
  # transmission via C - cattle with no prophylaxis
  # from exposed host to infected vector
  RVC <- calculate_RVC(rate_vectors_infected_by_cattle, time_in_state, transition_probabilities)
  RVC <- as.numeric(RVC)
  
  # from infected vector to exposed host
  #RCV <- biterate * prob_infection_to_host * (Nc / NH) * 1 / (death_v)
  RCV <- biterate * prob_infection_to_host * Nc_frac * 1 / (death_v) * bite_frac_cattle(NC, NH)
  RCV <- as.numeric(RCV)
  
  # transmission via P - cattle with prophylaxis
  # from exposed P to infected vector
  RVP <- calculate_RVP(rate_vectors_infected_by_cattle, time_in_state, transition_probabilities)
  RVP <- as.numeric(RVP)
  
  # from infected vector to exposed P
  if (is_strain_sensitive == "yes") {
    #RPV <- biterate * partial_susceptibility_proph_cattle * prob_infection_to_host * Nps / NH * 1 / (death_v)
    RPV <- biterate * partial_susceptibility_proph_cattle * prob_infection_to_host * Nps_frac * 1 / (death_v) * bite_frac_cattle(NC, NH)
  }
  if (is_strain_sensitive == "no") {
    #RPV <- biterate * prob_infection_to_host * ((Nps + Npf) / NH) * 1 / (death_v)
    RPV <- biterate * prob_infection_to_host * Nps_Npf_frac * 1 / (death_v) * bite_frac_cattle(NC, NH)
  }
  RPV <- as.numeric(RPV)
  
  # transmission via W
  # from infected wildlife to infected vector
  RVW <- biterate * prob_infection_to_vector * Nv_div_NW * 1 / (sigma_w + death_w) * gamma_v / (gamma_v + death_v) * bite_frac_wildlife(NW, NH)
  RVW <- as.numeric(RVW)
  
  # from infected vector to infected wildlife
  RWV <- biterate * prob_infection_to_host * Nw_frac * gamma_w / (gamma_w + death_w) * 1 / (death_v) * bite_frac_wildlife(NW, NH)
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

#################################################################################
create_named_vector_of_all_transition_probabilities <- function(params, is_strain_sensitive) {
  transition_probs1 <- create_named_vector_of_transition_probabilities(params, is_strain_sensitive)
  #transition_probs2 <- calculate_loop_probabilities(params, is_strain_sensitive)
  transition_probs2 <- calculate_loop_probabilities(transition_probs1, is_strain_sensitive)
  transition_probs <- c(transition_probs1, transition_probs2)
  transition_probs
}

create_named_vector_of_transition_probabilities <- function(params, is_strain_sensitive) {
  transition_probabilities <- with(as.list(params, is_strain_sensitive), {
    if (is_strain_sensitive == "yes") {
      sigma_treated <- sigma_st
    }
    if (is_strain_sensitive == "no") {
      sigma_treated <- sigma_c
    }
    
    #loop_probabilities <- calculate_loop_probabilities(params, is_strain_sensitive)
    #p1c <- loop_probabilities$p1c
    #p2c <- loop_probabilities$p2c
    
    prob_CI_from_CE <- gamma_c / (gamma_c + death_c + proph_ongoing)
    prob_PI_from_PE <- gamma_c / (gamma_c + death_c + proph_ongoing + waning_from_PE)
    
    prob_CI_treat_q <- treatment_q / (treatment_p + treatment_q + sigma_c + death_c + proph_ongoing)
    prob_CI_treat_p <- treatment_p / (treatment_p + treatment_q + sigma_c + death_c + proph_ongoing)
    prob_proph_from_CI <- proph_ongoing / (treatment_p + treatment_q + sigma_c + death_c + proph_ongoing)
    
    prob_PI_treat_q <- treatment_q / (treatment_p + treatment_q + sigma_c + death_c + waning_from_PI + proph_ongoing)
    prob_PI_treat_p <- treatment_p / (treatment_p + treatment_q + sigma_c + death_c + waning_from_PI + proph_ongoing)
    prob_proph_from_PI <- proph_ongoing / (treatment_p + treatment_q + sigma_c + death_c + waning_from_PI + proph_ongoing)
    
    prob_waning_from_partial_protection_from_PE <- waning_from_PE / (gamma_c + death_c + waning_from_PE + proph_ongoing)
    prob_waning_from_partial_protection_from_PI <- waning_from_PI / (treatment_p + treatment_q + sigma_c + death_c + waning_from_PI + proph_ongoing)
    prob_waning_from_partial_protection_from_PT <- waning_from_PT / (sigma_treated + death_c + waning_from_PT)
    prob_waning_from_partial_protection_from_PP <- waning_from_PP / (sigma_treated + death_c + waning_from_PP)
    
    
    prob_proph_from_CE <- proph_ongoing / (gamma_c + death_c + proph_ongoing)
    prob_proph_from_PE <- proph_ongoing / (gamma_c + death_c + proph_ongoing + waning_from_PE)
    prob_disease_from_CEX <- gamma_c / (gamma_c + death_c + sigma_treated)
    prob_disease_from_PEX <- gamma_c / (gamma_c + death_c + sigma_treated)
    
    probs <- cbind(
      #p1c, p2c,
      prob_CI_from_CE, prob_PI_from_PE, prob_CI_treat_q, prob_CI_treat_p, prob_PI_treat_q, prob_PI_treat_p,
      prob_waning_from_partial_protection_from_PE, prob_waning_from_partial_protection_from_PI,
      prob_waning_from_partial_protection_from_PT, prob_waning_from_partial_protection_from_PP,
      prob_proph_from_CI, prob_proph_from_PI, prob_proph_from_CE, prob_proph_from_PE, prob_disease_from_CEX,
      prob_disease_from_PEX
    )
    probs <- convert_array_to_named_vector(probs)
    probs
  })
  
  transition_probabilities
}


create_named_vector_of_time_in_state <- function(params, is_strain_sensitive) {
  time_in_state <- with(as.list(params), {
    if (is_strain_sensitive == "yes") {
      sigma_treated <- sigma_st
    }
    if (is_strain_sensitive == "no") {
      sigma_treated <- sigma_c
    }
    
    time_in_CI <- 1 / (treatment_p + treatment_q + sigma_c + death_c + proph_ongoing)
    time_in_CT <- 1 / (sigma_treated + death_c)
    time_in_PI <- 1 / (treatment_p + treatment_q + sigma_c + death_c + waning_from_PI + proph_ongoing)
    time_in_PT <- 1 / (sigma_treated + death_c + waning_from_PT)
    time_in_PP <- 1 / (sigma_treated + death_c + waning_from_PP)
    times <- cbind(time_in_CI, time_in_CT, time_in_PI, time_in_PT, time_in_PP)
    times <- convert_array_to_named_vector(times)
    times
  })
  
  return(time_in_state)
}



findGlobals(fun = calculate_time2A, merge = FALSE)$variables
findGlobals(fun = calculate_time2B, merge = FALSE)$variables
findGlobals(fun = calculate_RVC, merge = FALSE)$variables
findGlobals(fun = calculate_RVP, merge = FALSE)$variables
