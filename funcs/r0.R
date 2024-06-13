## --------------------- R0
##
##
##
##

r0_calc_sen_or_res <- function(params, Nc, Np, Nw, Nv, sen, basic){
  
  Nh <- params["NC"] + params["NW"]  
  
  biterate <- params["biterate"]
  prob.infection <- params["prob.infection"]
  prob.infection.v <- params["prob.infection.v"]
  fit.adj <- params["fit.adj"]
  
  treatment.p <- params["treatment.p"]
  treatment.q <- params["treatment.q"]
  waning <- params["waning"]
  
  gamma.c <- params["gamma.c"]
  death.c <- params["death.c"]
  sigma.c <- params["sigma.c"]
  sigma.st <- params["sigma.st"]
  
  gamma.p <- params["gamma.c"]
  death.p <- params["death.c"]
  sigma.p <- params["sigma.c"]
  
  gamma.w <- params["gamma.w"]
  death.w <- params["death.w"]
  sigma.w <- params["sigma.w"]
  
  gamma.v <- params["gamma.v"]
  death.v <- params["death.v"]
  
  
  if (sen == "yes"){sigma.treated <- sigma.st}
  if (sen == "no"){sigma.treated <- sigma.c}
  if (sen == "no"){prob.infection <- prob.infection* fit.adj}
  
  RCV <- biterate * prob.infection * Nc / Nh * gamma.c / (gamma.c + death.c) * 1 / (death.v)
  RCV <- as.numeric(RCV)
  
  RPV <- biterate * prob.infection * Np / Nh * gamma.p / (gamma.p + death.p) * 1 / (death.v)
  RPV <- as.numeric(RPV)
  
  RWV <- biterate * prob.infection * Nw / Nh * gamma.w / (gamma.w + death.w) * 1 / (death.v)
  RWV <- as.numeric(RWV)

  
  # Probability of I -> Tp
  p1c <- treatment.p/ (treatment.p + treatment.q + sigma.c + death.c)  
  
  # Probability of Tp -> I
  #p2c <- waning/ (treatment.p + treatment.q + sigma.st + death.c) 
  p2c <- waning/ (waning + sigma.treated + death.c) #LM corrected
  
  
  RVC <- biterate * prob.infection.v * Nv / Nh * gamma.v / (gamma.v + death.v) * 
    ( 1/(treatment.p + treatment.q + sigma.c + death.c) * 1/(1-p1c *p2c)  +
        treatment.q/(treatment.p + treatment.q + sigma.c + death.c) * 1/(death.c + sigma.treated) * 1/(1-p1c *p2c) +
        treatment.p/(treatment.p + treatment.q + sigma.c + death.c) * 1/ (death.c + sigma.treated + waning) *
        1/(1-p1c *p2c))
  RVC <- as.numeric(RVC)
  
  
  RVP <- biterate * prob.infection.v * Nv / Nh * gamma.v / (gamma.v + death.v) * 
    (1/(treatment.p + treatment.q + sigma.p + death.p + waning)) +              #contribution from PIs
    
    (waning /(treatment.p + treatment.q + sigma.p + death.p + waning)) * RVC  + #contribution from waning back to CIS
    
    biterate * prob.infection.v * Nv / Nh * gamma.v / (gamma.v + death.v) * (
      treatment.q/(treatment.p + treatment.q + sigma.p + death.p + waning) * ( 1/(death.p + sigma.treated + waning) + #contribution from PTs
                                                                                 waning/(death.p + sigma.treated + waning) * 1/(sigma.treated + death.c)))  + # waning from PTs back to CTs 
    
    biterate * prob.infection.v * Nv / Nh * gamma.v / (gamma.v + death.v) * (
      treatment.p /(treatment.p + treatment.q + sigma.p + death.p + waning)) * 1/(sigma.treated + death.p + waning) + #contrib from PPs
    
    treatment.p /(treatment.p + treatment.q + sigma.p + death.p + waning) *
    waning /(sigma.treated + death.p + waning) * RVC #contribution from PPs waning back to CIs
  
  
  RVP <- as.numeric(RVP)
  
  
  RVW <- biterate * prob.infection.v * Nv / Nh * 1 / (sigma.w + death.w) * gamma.v / (gamma.v + death.v)
  RVW <- as.numeric(RVW)
  
  
  answer <- RCV * RVC + RPV * RVP + RWV * RVW
  if (basic == TRUE){answer_name = "R0"} else {answer_name = "R"}
  
  names <- c(answer_name, "RCV", "RVC", "RPV", "RVP", "RWV", "RVW")
  output <- c(answer, RCV, RVC, RPV, RVP, RWV, RVW)
  names(output) <- names
  output
  
  return(output)
  
}


add_R_trajectories <- function(params, df) {
  
  Rsen_vec <- c()
  Rres_vec <- c()
  for (i in 1:nrow(df)) {
    Nc <- df$CS[i]
    Np <- df$PS[i]
    Nw <- df$WS[i]
    Nv <- df$VSt[i] + df$VSf[i]
    Rsen <- r0_calc_sen_or_res(params, Nc, Np, Nw, Nv, sen = "yes", basic = "no")[1]
    Rres <- r0_calc_sen_or_res(params, Nc, Np, Nw, Nv, sen = "no", basic = "no")[1]
    Rsen_vec <- c(Rsen_vec, Rsen)
    Rres_vec <- c(Rres_vec, Rres)
  }

  df$Rsen <- Rsen_vec
  df$Rres <- Rres_vec
  df
}

calculate_R0_from_inits <- function(inits) {
  Nc <- as.numeric(inits["CS"] + inits["CIs"])
  Np <- as.numeric(inits["PS"])
  Nw <- as.numeric(inits["WS"])
  Nv <- as.numeric(inits["VSt"])
  sen <- "yes"
  basic <- "yes"
  R0sen <- r0_calc_sen_or_res(params, Nc, Np, Nw, Nv, sen, basic)[1]
  sen <- "no"
  R0res <- r0_calc_sen_or_res(params, Nc, Np, Nw, Nv, sen, basic)[1]
  list(R0sen, R0res)
}

add_R0 <- function(inits, df){
  R0sen_and_R0res <- calculate_R0_from_inits(inits)
  df$R0sen <- R0sen_and_R0res[[1]]
  df$R0res <- R0sen_and_R0res[[2]]
  return(df)  
}
