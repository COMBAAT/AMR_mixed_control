my_rootfun <- function(t, y, params) {
  return(c(y['CS'] - 20.0, y['CIs'] - 10.0))
}

my_rootfun2 <- function (t, y, params) {
  dstate <- unlist(AAT_AMR_dens_dep(t, y, params)) # rate of change vector
  condition1 <- (y['CIs'] - 1e-5)
  condition2 <- sum(abs(dstate)) - 1e-5
  return(c(condition1, condition2))
}

my_rootfun3 <- function (t, y, params) {
  dstate <- unlist(AAT_AMR_dens_dep(t, y, params)) # rate of change vector
  Nc <- y['CS']
  Np <- y['PS']
  Nw <- y['WS']
  Nv <- y['VSt'] + y['VSf']
  Rsen <- r0_calc_sen_or_res(params, Nc, Np, Nw, Nv, is_strain_sensitive = "yes", basic = "no")
  
  condition1 <- (Rsen - 1.01)
  condition2 <- (y['CIs'] - 1e-5)
  
  return(c(condition1, condition2))
}

convert_named_vector_to_df <- function(named_vec){
  df = data.frame(as.list(vector1)) 
  df
}


calculate_epi_outputs <- function(treatment_type, params, last) {
  if (treatment_type == "F") {
    No_trt_cat <- as.numeric(params["treatment.q"]) * last$CIs * 365.25
    Incidence <- as.numeric(params["gamma.c"]) * last$CEs * 365.25
    Prob_onward_tran <- 1 - dpois(0, last$Rres[1])
    RiskA <- (last$CTs + last$PTs)
    RiskE <- (1 - dpois(0, last$Rres[1])) * (last$CTs + last$PTs)
  }

  if (treatment_type == "P") {
    No_trt_cat <- as.numeric(params["treatment.q"]) * (last$PIs + last$CIs) * 365.25
    Incidence <- as.numeric(params["gamma.c"]) * (last$PEs + last$CEs) * 365.25
    Prob_onward_tran <- 1 - dpois(0, last$Rres[1])
    RiskA <- (last$PEs + last$PIs + last$PPs)
    RiskE <- (1 - dpois(0, last$Rres[1])) * (last$PEs + last$PIs + last$PPs)
  }

  if (treatment_type == "B") {
    No_trt_cat <- (as.numeric(params["treatment.q"]) + as.numeric(params["treatment.p"])) * (last$PIs + last$CIs) * 365.25
    Inc <- as.numeric(params["gamma.c"]) * (last$PEs + last$CEs) * 365.25
    Prob_onward_tran <- 1 - dpois(0, last$Rres[1])
    RiskA <- (last$PEs + last$PIs + last$PPs + last$CTs + last$PTs)
    RiskE <- (1 - dpois(0, last$Rres[1])) * (last$PEs + last$PIs + last$PPs + last$CTs + last$PTs)
  }
  prevalence <- (last$PIs + last$CIs) / last$All.cows
  
  epi_outputs <- as.data.frame(cbind(No_trt_cat, Incidence, prevalence, Prob_onward_tran, RiskA, RiskE))
  return(epi_outputs)
}

findGlobals(fun = calculate_epi_outputs, merge = FALSE)$variables
