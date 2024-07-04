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
  df = data.frame(as.list(named_vec)) 
  df
}

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


include_parameters <- function(params, df) {
  params_df <- convert_named_vector_to_df(params)
  df_with_params <- cbind(params_df, df)
  df_with_params
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

merge_dfs_without_duplicate_columns <- function(df1, df2){
  duplicated_names <- names(df2)[(names(df2) %in% names(df1))]
  df2_reduced <- df2 %>% select(-all_of(duplicated_names))
  merged_df <- merge(df1, df2_reduced)
  merged_df
}

move_populations_first <- function(df){
  df <- df %>% select(NC, CS, PF, PS, NW, NV, everything())
  df
}

append_suffix_to_column_names <- function(df, suffix) {
  df <- df %>% rename_with(~ paste0(., suffix))
}


findGlobals(fun = calculate_epi_outputs, merge = FALSE)$variables
