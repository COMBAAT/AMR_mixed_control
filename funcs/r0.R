library(codetools)

## --------------------- R0

R_calc_sen_or_res <- function(params, Nc, Np, Nw, Nv, is_strain_sensitive, basic) {
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

  RCV <- biterate * prob_infection_to_host * Nc / Nh * gamma_c / (gamma_c + death_c) * 1 / (death_v)
  RCV <- as.numeric(RCV)

  RPV <- biterate * partial_susceptibility * prob_infection_to_host * Np / Nh * gamma_p / (gamma_p + death_p) * 1 / (death_v)
  RPV <- as.numeric(RPV)

  RWV <- biterate * prob_infection_to_host * Nw / Nh * gamma_w / (gamma_w + death_w) * 1 / (death_v)
  RWV <- as.numeric(RWV)


  # Probability of I -> Tp
  p1c <- treatment_p / (treatment_p + treatment_q + sigma_c + death_c)

  # Probability of Tp -> I
  # p2c <- waning / (treatment_p + treatment_q + sigma_st + death_c)
  p2c <- waning / (waning + sigma_treated + death_c) # LM corrected


  RVC <- biterate * prob_infection_to_vector * Nv / Nh * gamma_v / (gamma_v + death_v) *
    (1 / (treatment_p + treatment_q + sigma_c + death_c) * 1 / (1 - p1c * p2c) +
      treatment_q / (treatment_p + treatment_q + sigma_c + death_c) * 1 / (death_c + sigma_treated) * 1 / (1 - p1c * p2c) +
      treatment_p / (treatment_p + treatment_q + sigma_c + death_c) * 1 / (death_c + sigma_treated + waning) *
        1 / (1 - p1c * p2c))
  RVC <- as.numeric(RVC)


  RVP <- biterate * prob_infection_to_vector * Nv / Nh * gamma_v / (gamma_v + death_v) *
    (1 / (treatment_p + treatment_q + sigma_p + death_p + waning)) + # contribution from PIs

    (waning / (treatment_p + treatment_q + sigma_p + death_p + waning)) * RVC + # contribution from waning back to CIS

    biterate * prob_infection_to_vector * Nv / Nh * gamma_v / (gamma_v + death_v) * (
      treatment_q / (treatment_p + treatment_q + sigma_p + death_p + waning) * (1 / (death_p + sigma_treated + waning) + # contribution from PTs
        waning / (death_p + sigma_treated + waning) * 1 / (sigma_treated + death_c))) + # waning from PTs back to CTs

    biterate * prob_infection_to_vector * Nv / Nh * gamma_v / (gamma_v + death_v) * (
      treatment_p / (treatment_p + treatment_q + sigma_p + death_p + waning)) * 1 / (sigma_treated + death_p + waning) + # contrib from PPs

    treatment_p / (treatment_p + treatment_q + sigma_p + death_p + waning) *
      waning / (sigma_treated + death_p + waning) * RVC # contribution from PPs waning back to CIs

  RVP <- as.numeric(RVP)


  RVW <- biterate * prob_infection_to_vector * Nv / Nh * 1 / (sigma_w + death_w) * gamma_v / (gamma_v + death_v)
  RVW <- as.numeric(RVW)

  reproduction_number <- RCV * RVC + RPV * RVP + RWV * RVW
  reproduction_number
}

calculate_R0 <- function(params) {
  Np <- params["PS"]
  Nc <- params["CS"]
  Nw <- params["NW"]
  Nv <- params["NV"]
  R0sen <- R_calc_sen_or_res(params, Nc, Np, Nw, Nv, is_strain_sensitive = "yes", basic = "yes")
  R0res <- R_calc_sen_or_res(params, Nc, Np, Nw, Nv, is_strain_sensitive = "no", basic = "yes")
  c("R0sen" = R0sen, "R0res" = R0res)
}


calculate_R_from_row_of_df <- function(params, this_row) {
  Nc <- this_row$CS
  Np <- this_row$PS
  Nw <- this_row$WS
  Nv <- this_row$VSt + this_row$VSf
  Rsen <- R_calc_sen_or_res(params, Nc, Np, Nw, Nv, is_strain_sensitive = "yes", basic = "no")
  Rres <- R_calc_sen_or_res(params, Nc, Np, Nw, Nv, is_strain_sensitive = "no", basic = "no")
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
