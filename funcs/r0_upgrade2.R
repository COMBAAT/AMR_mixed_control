
R_calc_sen_or_res2 <- function(params, Nc, Npf, Nps, Nw, Nv, is_strain_sensitive, basic) {

  R0 <- with(as.list(params), {

  if (is_strain_sensitive == "yes") {
    sigma_treated <- sigma_st
    Npsus <- Nps * partial_susceptibility_proph_cattle
  }
  if (is_strain_sensitive == "no") {
    sigma_treated <- sigma_c
  }
  if (is_strain_sensitive == "no") {
    prob_infection_to_host <- prob_infection_to_host * fit_adj
    Npsus <- Nps + Npf
  }


  # Define matrix of transitions
  # Order the elements of the infectious subsystem 1 CE, 2 CEX, 3 CI, 4 CT, 5 PE, 6 PEX, 7 PI, 8 PT, 9 PP, 10 WE, 11 WI, 12 VE, 13 VI
  size_of_infectious_subsystem <- 13
  Sigma <- matrix(0, size_of_infectious_subsystem, size_of_infectious_subsystem)

  # CE equation
  Sigma[1, 1] <- -(death_c + gamma_c + proph_ongoing2) # CE death, become infected, proph
  Sigma[1, 5] <- waning_from_PE # waning_from_partial_protection from PE

  # CEX equation
  Sigma[2, 1] <- proph_ongoing2 # from CE
  Sigma[2, 2] <- -(death_c + gamma_c + sigma_treated) # CEX death, become infected, recovery due to treatment

  # CI equation
  Sigma[3, 1] <- gamma_c # from CE
  Sigma[3, 3] <- -(death_c + sigma_c + treatment_p + treatment_q + proph_ongoing) # death, reovery or treatment of CI
  Sigma[3, 7] <- waning_from_PI # waning_from_partial_protection from PI
  #Sigma[3, 9] <- waning_from_partial_protection # waning_from_partial_protection from PP

  # CT equation
  Sigma[4, 3] <- treatment_q # from CI
  Sigma[4, 4] <- -(death_c + sigma_treated) # CT death, recovery due to treatment
  Sigma[4, 8] <- waning_from_partial_protection # waning_from_partial_protection from PT

  # PE equation
  Sigma[5, 5] <- -(death_c + gamma_c + proph_ongoing2 + waning_from_PE) # PE death, become infected, proph, waning_from_partial_protection

  # PEX equation
  Sigma[6, 5] <- proph_ongoing2 # from PE
  Sigma[6, 6] <- -(death_c + gamma_c + sigma_treated) # PEX death, become infected, recovery due to treatment

  # PI equation
  Sigma[7, 5] <- gamma_c # from PE
  Sigma[7, 7] <- -(death_c + sigma_c + treatment_p + treatment_q + proph_ongoing + waning_from_PI) # death, recovery or treatment of PI
  Sigma[7, 9] <- +waning_from_PP # waning_from_partial_protection from PP

  # PT equation
  Sigma[8, 7] <- treatment_q # from PI
  Sigma[8, 8] <- -(death_c + sigma_treated + waning_from_partial_protection) # PT death, recovery due to treatment

  # PP equation
  Sigma[9, 2] <- gamma_c # from CEX
  Sigma[9, 3] <- treatment_p + proph_ongoing # from CI
  Sigma[9, 7] <- treatment_p + proph_ongoing # from PI
  Sigma[9, 6] <- gamma_c # from PEX
  Sigma[9, 9] <- -(death_c + sigma_treated + waning_from_PP) # PP death, recovery due to treatment

  # WE equation
  Sigma[10, 10] <- -(death_w + gamma_w) # death, become infected

  # WI equation
  Sigma[11, 10] <- gamma_w # from WE
  Sigma[11, 11] <- -(death_w + sigma_w) # death, recovery

  # VE equation
  Sigma[12, 12] <- -(death_v + gamma_v) # death, become infected

  # VI equation
  Sigma[13, 12] <- gamma_v # from VE
  Sigma[13, 13] <- -death_v # death

  Inv_sigma <- solve(Sigma)

  # Define matrix of transmission
  # Order the elements of the infectiuos subsystem 1 CE, 2 CEX, 3 CI, 4 CT, 5 PE, 6 PEX, 7 PI, 8 PT, 9 PP, 10 WE, 11 WI, 12 VE, 13 VI
  Transmission <- matrix(0, size_of_infectious_subsystem, size_of_infectious_subsystem)

  # CE equation
  Transmission[1, 13] <- biterate * prob_infection_to_host * Nc / NH

  # PE equation
  Transmission[5, 13] <- biterate * prob_infection_to_host * Npsus / NH

  # WE equation
  Transmission[10, 13] <- biterate * prob_infection_to_host * Nw / NH

  # VE equation
  Transmission[12, 3] <- biterate * prob_infection_to_vector * Nv / NH
  Transmission[12, 4] <- biterate * prob_infection_to_vector * Nv / NH
  Transmission[12, 7] <- biterate * prob_infection_to_vector * Nv / NH
  Transmission[12, 8] <- biterate * prob_infection_to_vector * Nv / NH
  Transmission[12, 9] <- biterate * prob_infection_to_vector * Nv / NH
  Transmission[12, 11] <- biterate * prob_infection_to_vector * Nv / NH


  NGM <- -Transmission %*% Inv_sigma
  NGM

  lambda <- max(Re(eigen(NGM)$values))
  R0 <- lambda^2
  R0
  })
  R0
}
