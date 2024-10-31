add_R_trajectories2 <- function(params, df) {
  Rsen_vec <- c()
  Rres_vec <- c()
  for (i in 1:nrow(df)) {
    this_row <- df[i, ]
    Rsen_and_Rres <- calculate_R_from_row_of_df2(params, this_row)
    Rsen <- Rsen_and_Rres["Rsen"]
    Rres <- Rsen_and_Rres["Rres"]
    Rsen_vec <- c(Rsen_vec, Rsen)
    Rres_vec <- c(Rres_vec, Rres)
  }
  
  df$Rsen2 <- Rsen_vec
  df$Rres2 <- Rres_vec
  df
}



calculate_R02 <- function(params) {
  Npf <- params["PF"]
  Nps <- params["PS"]
  Nc <- params["CS"]
  Nw <- params["NW"]
  Nv <- params["NV"]
  R0sen <- R_calc_sen_or_res2(params, Nc, Npf, Nps, Nw, Nv, is_strain_sensitive = "yes", basic = "yes")
  R0res <- R_calc_sen_or_res2(params, Nc, Npf, Nps, Nw, Nv, is_strain_sensitive = "no", basic = "yes")
  c("R0sen" = R0sen, "R0res" = R0res)
}


calculate_R_from_row_of_df2 <- function(params, this_row) {
  Nc <- this_row$CS
  Nps <- this_row$PS
  Npf <- this_row$PF
  Nw <- this_row$WS
  Nv <- this_row$VSt + this_row$VSf
  Rsen <- R_calc_sen_or_res2(params, Nc, Npf, Nps, Nw, Nv, is_strain_sensitive = "yes", basic = "no")
  Rres <- R_calc_sen_or_res2(params, Nc, Npf, Nps, Nw, Nv, is_strain_sensitive = "no", basic = "no")
  c("Rsen" = Rsen, "Rres" = Rres)
}


R_calc_sen_or_res2 <- function(params, Nc, Npf, Nps, Nw, Nv, is_strain_sensitive, basic) {
  Nh <- params["NC"] + params["NW"]
  
  biterate <- params["biterate"]
  prob_infection_to_host <- params["prob_infection_to_host"]
  partial_susceptibility_proph_cattle <- params["partial_susceptibility_proph_cattle"]
  prob_infection_to_vector <- params["prob_infection_to_vector"]
  fit_adj <- params["fit_adj"]
  
  treatment_p <- params["treatment_p"]
  treatment_q <- params["treatment_q"]
  waning <- params["waning"]
  
  gamma_c <- params["gamma_c"]
  death_c <- params["death_c"]
  sigma_c <- params["sigma_c"]
  sigma_st <- params["sigma_st"]
  
  gamma_p <- params["gamma_c"]
  death_p <- params["death_c"]
  sigma_p <- params["sigma_c"]
  
  gamma_w <- params["gamma_w"]
  death_w <- params["death_w"]
  sigma_w <- params["sigma_w"]
  
  gamma_v <- params["gamma_v"]
  death_v <- params["death_v"]
  proph_ongoing <- params["proph_ongoing"]
  
  
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
  Sigma[1, 1] <- -(death_c + gamma_c + proph_ongoing) # CE death, become infected, proph
  Sigma[1, 5] <- waning                               # waning from PE
  
  # CEX equation
  Sigma[2, 1] <- proph_ongoing                        # from CE
  Sigma[2, 2] <- -(death_c + gamma_c + sigma_treated) # CEX death, become infected, recovery due to treatment
  
  # CI equation
  Sigma[3, 1] <- gamma_c                              # from CE
  Sigma[3, 3] <- -(death_c + sigma_c + treatment_p + treatment_q + proph_ongoing) # death, reovery or treatment of CI
  Sigma[3, 7] <- waning                               # waning from PI
  Sigma[3, 9] <- waning                               # waning from PP
  
  # CT equation
  Sigma[4, 3] <- treatment_q                          # from CI
  Sigma[4, 4] <- -(death_c + sigma_treated)           # CT death, recovery due to treatment
  Sigma[4, 8] <- waning                               # waning from PT
  
  # PE equation
  Sigma[5, 5] <- -(death_p + gamma_p + proph_ongoing + waning) # PE death, become infected, proph, waning
  
  # PEX equation
  Sigma[6, 5] <- proph_ongoing                        # from PE
  Sigma[6, 6] <- -(death_p + gamma_p + sigma_treated) # PEX death, become infected, recovery due to treatment
  
  # PI equation
  Sigma[7, 5] <- gamma_p                              # from PE
  Sigma[7, 7] <- -(death_p + sigma_p + treatment_p + treatment_q + proph_ongoing + waning) # death, recovery or treatment of PI
  
  # PT equation
  Sigma[8, 7] <- treatment_q                          # from PI
  Sigma[8, 8] <- -(death_p + sigma_treated + waning)  # PT death, recovery due to treatment
  
  # PP equation
  Sigma[9, 2] <- gamma_c                              # from CEX
  Sigma[9, 3] <- treatment_p + proph_ongoing          # from CI
  Sigma[9, 7] <- treatment_p + proph_ongoing         # from PI
  Sigma[9, 6] <- gamma_p                              # from PEX
  Sigma[9, 9] <- -(death_p + sigma_treated + waning)  # PP death, recovery due to treatment
  
  # WE equation
  Sigma[10, 10] <- -(death_w + gamma_w)               # death, become infected
  
  # WI equation
  Sigma[11, 10] <- gamma_w                            # from WE
  Sigma[11, 11] <- -(death_w + sigma_w)               # death, recovery
  
  # VE equation
  Sigma[12, 12] <- -(death_v + gamma_v)               # death, become infected
  
  # VI equation
  Sigma[13, 12] <- gamma_v                            # from VE
  Sigma[13, 13] <- -death_v                           # death
  
  Inv_sigma <- solve(Sigma)
  
  # Define matrix of transmission
  # Order the elements of the infectiuos subsystem 1 CE, 2 CEX, 3 CI, 4 CT, 5 PE, 6 PEX, 7 PI, 8 PT, 9 PP, 10 WE, 11 WI, 12 VE, 13 VI
  Transmission <- matrix(0, size_of_infectious_subsystem, size_of_infectious_subsystem)
  
  # CE equation
  Transmission[1, 13] <- biterate * prob_infection_to_host * Nc / Nh 
  
  # PE equation
  Transmission[5, 13] <- biterate * prob_infection_to_host * Npsus / Nh
  
  # WE equation
  Transmission[10, 13] <- biterate * prob_infection_to_host * Nw / Nh
  
  # VE equation
  Transmission[12, 3] <- biterate * prob_infection_to_vector * Nv / Nh
  Transmission[12, 4] <- biterate * prob_infection_to_vector * Nv / Nh
  Transmission[12, 7] <- biterate * prob_infection_to_vector * Nv / Nh
  Transmission[12, 8] <- biterate * prob_infection_to_vector * Nv / Nh
  Transmission[12, 9] <- biterate * prob_infection_to_vector * Nv / Nh
  Transmission[12, 11] <- biterate * prob_infection_to_vector * Nv / Nh
  
  
  NGM <- -Transmission %*% Inv_sigma
  NGM
  
  lambda <- max(Re(eigen(NGM)$values))
  R0 <- lambda^2
  R0
}
