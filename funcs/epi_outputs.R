

append_epi_outputs_to_df <- function(df) {
  days_per_year <- 365.25

  df <- df %>% mutate(
    No_trt_cat = (treatment.q * CIs_final + treatment.p * PIs_final) * days_per_year,
    Incidence = gamma.c * (PEs_final + CEs_final) * days_per_year,
    Prob_onward_tran = 1 - dpois(0, Rres_final),
    RiskA = PEs_final + PIs_final + PPs_final + CTs_final + PTs_final,
    RiskE = Prob_onward_tran * RiskA,
    prevalence = (PIs_final + CIs_final) / All.cows_final
  )
  df
}


add_population_totals <- function(df){
  
  df_new <- df %>% mutate(Cattle_total = rowSums(select(., starts_with("C"))),
                          Prophylactic_total = rowSums(select(., starts_with("P"))),
                          Vector_total = rowSums(select(., starts_with("V"))),
                          Wildlife_total = rowSums(select(., starts_with("W"))),
                          All.cows = Cattle_total + Prophylactic_total)
  
  return(df_new)
  
}


calculate_epi_outputs <- function(treatment_type, params, final_state) {
  if (treatment_type == "F") {
    No_trt_cat <- as.numeric(params["treatment.q"]) * final_state$CIs * 365.25
    Incidence <- as.numeric(params["gamma.c"]) * final_state$CEs * 365.25
    Prob_onward_tran <- 1 - dpois(0, final_state$Rres[1])
    RiskA <- (final_state$CTs + final_state$PTs)
    RiskE <- (1 - dpois(0, final_state$Rres[1])) * (final_state$CTs + final_state$PTs)
  }

  if (treatment_type == "P") {
    No_trt_cat <- as.numeric(params["treatment.q"]) * (final_state$PIs + final_state$CIs) * 365.25
    Incidence <- as.numeric(params["gamma.c"]) * (final_state$PEs + final_state$CEs) * 365.25
    Prob_onward_tran <- 1 - dpois(0, final_state$Rres[1])
    RiskA <- (final_state$PEs + final_state$PIs + final_state$PPs)
    RiskE <- (1 - dpois(0, final_state$Rres[1])) * (final_state$PEs + final_state$PIs + final_state$PPs)
  }

  if (treatment_type == "B") {
    No_trt_cat <- (as.numeric(params["treatment.q"]) + as.numeric(params["treatment.p"])) * (final_state$PIs + final_state$CIs) * 365.25
    Inc <- as.numeric(params["gamma.c"]) * (final_state$PEs + final_state$CEs) * 365.25
    Prob_onward_tran <- 1 - dpois(0, final_state$Rres[1])
    RiskA <- (final_state$PEs + final_state$PIs + final_state$PPs + final_state$CTs + final_state$PTs)
    RiskE <- (1 - dpois(0, final_state$Rres[1])) * (final_state$PEs + final_state$PIs + final_state$PPs + final_state$CTs + final_state$PTs)
  }
  prevalence <- (final_state$PIs + final_state$CIs) / final_state$All.cows
  
  epi_outputs <- as.data.frame(cbind(No_trt_cat, Incidence, prevalence, Prob_onward_tran, RiskA, RiskE))
  return(epi_outputs)
}



findGlobals(fun = add_population_totals, merge = FALSE)$variables
findGlobals(fun = append_epi_outputs_to_df, merge = FALSE)$variables
