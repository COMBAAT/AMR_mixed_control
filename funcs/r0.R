library(codetools)

## --------------------- R0

R_calc_sen_or_res <- function(params, Nc, Npf, Nps, Nw, Nv, is_strain_sensitive, basic) {
  Nh <- params["NC"] + params["NW"]

  biterate <- params["biterate"]
  prob_infection_to_host <- params["prob_infection_to_host"]
  partial_susceptibility <- params["partial_susceptibility"]
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
  }
  if (is_strain_sensitive == "no") {
    sigma_treated <- sigma_c
  }
  if (is_strain_sensitive == "no") {
    prob_infection_to_host <- prob_infection_to_host * fit_adj
  }

  RCV <- biterate * prob_infection_to_host * Nc / Nh * gamma_c / (gamma_c + death_c + proph_ongoing) * 1 / (death_v)
  RCV <- as.numeric(RCV)

  if (is_strain_sensitive == "yes") {
  RPV <- biterate * partial_susceptibility * prob_infection_to_host * Nps / Nh * gamma_p / (gamma_p + death_p + proph_ongoing) * 1 / (death_v)
  } 
  if (is_strain_sensitive == "no") {
    RPV <- biterate * prob_infection_to_host * (Nps + Npf) / Nh * gamma_p / (gamma_p + death_p + proph_ongoing) * 1 / (death_v)
  }
  RPV <- as.numeric(RPV)

  RWV <- biterate * prob_infection_to_host * Nw / Nh * gamma_w / (gamma_w + death_w) * 1 / (death_v)
  RWV <- as.numeric(RWV)


  # Probability of I -> Tp
  p1c <- (treatment_p + proph_ongoing) / (treatment_p + treatment_q + sigma_c + death_c + proph_ongoing)

  # Probability of Tp -> I
  # p2c <- waning / (treatment_p + treatment_q + sigma_st + death_c)
  p2c <- waning / (waning + sigma_treated + death_c) # LM corrected


  rate_vectors_infected <- biterate * prob_infection_to_vector * Nv / Nh * gamma_v / (gamma_v + death_v)
  time_in_CI <- 1 / (treatment_p + treatment_q + sigma_c + death_c + proph_ongoing)
  time_in_CT <- 1 / (sigma_treated + death_c)
  prob_CI_treat_q <- treatment_q / (treatment_p + treatment_q + sigma_c + death_c + proph_ongoing)
  prob_CI_treat_p <- treatment_p / (treatment_p + treatment_q + sigma_c + death_c + proph_ongoing)
  
  time_in_PI <- 1 / (treatment_p + treatment_q + sigma_p + death_p + waning + proph_ongoing)
  time_in_PT <- 1 / (sigma_treated + death_p + waning)
  time_in_PP <- 1 / (sigma_treated + death_p + waning)
  
  prob_PI_treat_q <- treatment_q / (treatment_p + treatment_q + sigma_p + death_p + waning + proph_ongoing)
  prob_PI_treat_p <- treatment_p / (treatment_p + treatment_q + sigma_p + death_p + waning + proph_ongoing)
  prob_waning_from_PI <- waning / (treatment_p + treatment_q + sigma_p + death_p + waning + proph_ongoing)
  prob_waning_from_PT <- waning / (sigma_treated + death_p + waning)
  prob_waning_from_PP <- waning / (sigma_treated + death_p + waning)
  prob_proph_from_CI <- proph_ongoing / (treatment_p + treatment_q + sigma_c + death_c + proph_ongoing)
  prob_proph_from_PI <- proph_ongoing / (treatment_p + treatment_q + sigma_p + death_p + waning + proph_ongoing)
  
  
  RVC <- rate_vectors_infected *
    ( time_in_CI * 1 / (1 - p1c * p2c) +
        prob_CI_treat_q * time_in_CT * 1 / (1 - p1c * p2c) +
        prob_CI_treat_p * time_in_PP * 1 / (1 - p1c * p2c) +
        prob_proph_from_CI * time_in_PP * 1 / (1 - p1c * p2c) 
      )
  RVC <- as.numeric(RVC)
    
  # RVC <- biterate * prob_infection_to_vector * Nv / Nh * gamma_v / (gamma_v + death_v) *
  #   (1 / (treatment_p + treatment_q + sigma_c + death_c) * 1 / (1 - p1c * p2c) +
  #      treatment_q / (treatment_p + treatment_q + sigma_c + death_c) * 1 / (death_c + sigma_treated) * 1 / (1 - p1c * p2c) +
  #      treatment_p / (treatment_p + treatment_q + sigma_c + death_c) * 1 / (death_c + sigma_treated + waning) *
  #      1 / (1 - p1c * p2c))
  # RVC <- as.numeric(RVC)
  #print(paste0(is_strain_sensitive, " ", basic, " ", RVCnew, " ", RVC))
  
  
  RVP <- rate_vectors_infected * time_in_PI +  prob_waning_from_PI * RVC +                            # contribution from PIs
         rate_vectors_infected * prob_PI_treat_q * time_in_PT +
         rate_vectors_infected * prob_PI_treat_q * prob_waning_from_PT * time_in_CT +

         rate_vectors_infected * prob_PI_treat_p * time_in_PP +          # contrib from PPs
                                            # contribution from waning back to CIS
         prob_PI_treat_p * prob_waning_from_PP * RVC +

         rate_vectors_infected * prob_proph_from_PI * time_in_PP +
         prob_proph_from_PI * prob_waning_from_PP * RVC
  RVP <- as.numeric(RVP)
  
  
  
  # RVP <- biterate * prob_infection_to_vector * Nv / Nh * gamma_v / (gamma_v + death_v) *
  #       (1 / (treatment_p + treatment_q + sigma_p + death_p + waning)) + # contribution from PIs
  # 
  #       (waning / (treatment_p + treatment_q + sigma_p + death_p + waning)) * RVC + # contribution from waning back to CIS
  # 
  #       biterate * prob_infection_to_vector * Nv / Nh * gamma_v / (gamma_v + death_v) * (
  #         treatment_q / (treatment_p + treatment_q + sigma_p + death_p + waning) * (1 / (death_p + sigma_treated + waning) + # contribution from PTs
  #           waning / (death_p + sigma_treated + waning) * 1 / (sigma_treated + death_c))) + # waning from PTs back to CTs
  # 
  #       biterate * prob_infection_to_vector * Nv / Nh * gamma_v / (gamma_v + death_v) * (
  #         treatment_p / (treatment_p + treatment_q + sigma_p + death_p + waning)) * 1 / (sigma_treated + death_p + waning) + # contrib from PPs
  # 
  #       treatment_p / (treatment_p + treatment_q + sigma_p + death_p + waning) *
  #         waning / (sigma_treated + death_p + waning) * RVC # contribution from PPs waning back to CIs
  # 
  # RVP <- as.numeric(RVP)
  # print(paste0(is_strain_sensitive, " ", basic, " ", RVPnew, " ", RVP))


  RVW <- biterate * prob_infection_to_vector * Nv / Nh * 1 / (sigma_w + death_w) * gamma_v / (gamma_v + death_v)
  RVW <- as.numeric(RVW)

  reproduction_number <- RCV * RVC + RPV * RVP + RWV * RVW
  reproduction_number
}

calculate_R0 <- function(params) {
  Npf <- params["PF"]
  Nps <- params["PS"]
  Nc <- params["CS"]
  Nw <- params["NW"]
  Nv <- params["NV"]
  R0sen <- R_calc_sen_or_res(params, Nc, Npf, Nps, Nw, Nv, is_strain_sensitive = "yes", basic = "yes")
  R0res <- R_calc_sen_or_res(params, Nc, Npf, Nps, Nw, Nv, is_strain_sensitive = "no", basic = "yes")
  c("R0sen" = R0sen, "R0res" = R0res)
}


calculate_R_from_row_of_df <- function(params, this_row) {
  Nc <- this_row$CS
  Nps <- this_row$PS
  Npf <- this_row$PF
  Nw <- this_row$WS
  Nv <- this_row$VSt + this_row$VSf
  Rsen <- R_calc_sen_or_res(params, Nc, Npf, Nps, Nw, Nv, is_strain_sensitive = "yes", basic = "no")
  Rres <- R_calc_sen_or_res(params, Nc, Npf, Nps, Nw, Nv, is_strain_sensitive = "no", basic = "no")
  c("Rsen" = Rsen, "Rres" = Rres)
}


add_R_trajectories <- function(params, df) {
  Rsen_vec <- c()
  Rres_vec <- c()
  for (i in 1:nrow(df)) {
    this_row <- df[i, ]
    Rsen_and_Rres <- calculate_R_from_row_of_df(params, this_row)
    Rsen <- Rsen_and_Rres["Rsen"]
    Rres <- Rsen_and_Rres["Rres"]
    Rsen_vec <- c(Rsen_vec, Rsen)
    Rres_vec <- c(Rres_vec, Rres)
  }

  df$Rsen <- Rsen_vec
  df$Rres <- Rres_vec
  df
}

add_R0 <- function(params, df) {
  R0sen_and_R0res <- calculate_R0(params)
  R0sen <- R0sen_and_R0res["R0sen"]
  R0res <- R0sen_and_R0res["R0res"]
  df$R0sen <- R0sen
  df$R0res <- R0res
  df
}


findGlobals(fun = R_calc_sen_or_res, merge = FALSE)$variables
findGlobals(fun = calculate_R0, merge = FALSE)$variables
findGlobals(fun = add_R0, merge = FALSE)$variables
# findGlobals(fun = add_R0, merge = FALSE)$variables
findGlobals(fun = calculate_R_from_row_of_df, merge = FALSE)$variables
findGlobals(fun = add_R_trajectories, merge = FALSE)$variables
