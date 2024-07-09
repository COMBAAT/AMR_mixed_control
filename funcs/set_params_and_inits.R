## --------------------- Params & Inits

library(codetools)

## Set parameters & Initial Conditions ----




set_baseline_parameters <- function() {
  # Using day as unit of time
  cattle_lifespan <- 5 * 365
  cattle_incubation_period <- 15
  cattle_infection_period <- 100
  cattle_proph_full_protection_period <- 60
  cattle_proph_partial_protection_period <- 30
  cattle_treatment_period <- 3

  wildlife_lifespan <- 365
  wildlife_infection_period <- cattle_infection_period
  wildlife_incubation_period <- 20

  bite_rate <- 0.8 / 4
  prob_infection_to_host <- 0.46
  prob_infection_to_vector <- 0.025
  vector_teneral_period <- 4
  days_between_feeds <- 4
  prob_vector_surviving_feed <- 0.96
  prob_vector_surviving_nonfeeding_day <- 0.98
  
  baseline_params <- cbind(cattle_lifespan, cattle_incubation_period, cattle_infection_period,
                           cattle_proph_full_protection_period, cattle_proph_partial_protection_period,
                           cattle_treatment_period, wildlife_lifespan, wildlife_infection_period,
                           wildlife_incubation_period, bite_rate, prob_infection_to_host, prob_infection_to_vector,
                           vector_teneral_period, days_between_feeds, prob_vector_surviving_feed, prob_vector_surviving_nonfeeding_day
    
  )
  baseline_params <- convert_array_to_named_vector(baseline_params)
  baseline_params
}


calculate_vector_death_rate <- function(d, qf, qn, pi) {
  # From Hargrove et al 2012, using exp( - daily_mortality * d) = qf * qn ^ d
  # pi is proportion of animals that are insecticide treated
  # qf is probability of a vector surviving a feed
  # qn is probability of vector surviving a non-feeding day
  # d is the length of the feeding cycle
  daily_mortality <- -log( (1 - pi) * qf * qn ^ d) / d
  daily_mortality
}


set_parameters_NEW <- function(this_scenario) {
  birth_adj <- this_scenario$birth_adj
  fit_adj <- this_scenario$fit_adj
  K <- this_scenario$K
  treat_prop <- this_scenario$treat_prop
  prop_insecticide <- this_scenario$prop_insecticide
  NW <- this_scenario$NW
  prop_prophylaxis <- this_scenario$prop_prophylaxis
  treatment_type <- this_scenario$treatment_type
  dose_adj <- this_scenario$dose_adj
  emergence_adj <- this_scenario$emergence_adj
  
  baseline_params <- set_baseline_parameters()
  
  ## Cattle -----
  birth_c <- 1 / baseline_params["cattle_lifespan"]
  gamma_c <- 1 / baseline_params["cattle_incubation_period"]
  death_c <- birth_c
  death_p <- death_c
  sigma_c <- 1 / baseline_params["cattle_infection_period"]
  treatment <- 1 * treat_prop * (sigma_c + death_c) / (1 - treat_prop)
  emergence <- 0
  
  
  if (treatment_type == "quick") {
    treatment_q <- treatment
    treatment_p <- 0
    emergence_p <- 0
    emergence_f <- emergence * emergence_adj
  } else {
    if (treatment_type == "P") {
      treatment_q <- 0
      treatment_p <- treatment
      emergence_p <- emergence * emergence_adj
      emergence_f <- 0
    } else {
      treatment_q <- treatment
      treatment_p <- treatment
      emergence_p <- emergence * emergence_adj
      emergence_f <- emergence * emergence_adj
    }
  }
  
  sigma_st <- (1 / 3) * dose_adj + sigma_c * (1 - dose_adj) #* 250 #LM: adjusted so that R0 drops below 1 when 99% treated to reflect Hargrove
  rec_adj <- 1
  waning <- (1 / baseline_params["cattle_proph_partial_protection_period"]) / dose_adj
  waning_f2s <- (1 / baseline_params["cattle_proph_full_protection_period"]) / dose_adj
  new_prop <- 0
  
  
  NC <- 50 # Total cattle
  # figure out equilibrium in absence of infection
  # birth_c * prop_prophylaxis *NC - death_c * PF - waning_f2s*PF
  PF <- birth_c * prop_prophylaxis * NC / (death_c + waning_f2s)
  # waning_f2s * PF - death_p * PS - waning * PS
  PS <- waning_f2s * PF / (death_p + waning)
  # birth_c * (1-prop_prophylaxis) * NC - death_c * CS + waning * PS
  CS <- (birth_c * (1 - prop_prophylaxis) * NC + waning * PS) / death_c
  
  ## ----- Wildlife
  birth_w <- 1 / baseline_params["wildlife_lifespan"]
  prob_infection.s_w <- baseline_params["prob_infection_to_host"]
  prob_infection.r_w <- baseline_params["prob_infection_to_host"]
  gamma_w <- 1 / baseline_params["wildlife_incubation_period"]
  death_w <- birth_w
  sigma_w <- sigma_c
  reversion <- 0
  
  ## -----  Vectors
  
  ten2fed <- 1 / baseline_params["vector_teneral_period"]

  prop_insecticide.actual <- prop_insecticide * NC / (NC + NW) # Proportion of insecticide adjusted for wildlife
  
  death_v <- calculate_vector_death_rate(d = baseline_params["days_between_feeds"],
                                         qf = baseline_params["prob_vector_surviving_feed"],
                                         qn = baseline_params["prob_vector_surviving_nonfeeding_day"],
                                         pi = prop_insecticide.actual)
  death_v_no_insecticide <- calculate_vector_death_rate(d = baseline_params["days_between_feeds"],
                                                        qf = baseline_params["prob_vector_surviving_feed"],
                                                        qn = baseline_params["prob_vector_surviving_nonfeeding_day"],
                                                        pi = 0.0)
  birth_v <- birth_adj * death_v_no_insecticide # Vector birth rate
  equil_vector_pop <- max(0, K * (1 - death_v / birth_v)) # Vector equilibrium population
  
  incubation <- 20
  gamma_v <- death_v * exp(-death_v * incubation) / (1 - exp(-death_v * incubation)) # Rate from E to I
  
  NV <- equil_vector_pop # equil_vector_pop
  
  # take directly from baseline
  biterate <- 0.8 / 4
  prob_infection_to_host <- 0.46
  prob_infection_to_vector <- 0.025
  qf <- 0.96 # Probability of surviving on a feeding day
  qn <- 0.98 # Probability of surviving on a non-feeding day
  feed.cyc <- 4 # Days between feeding
  
  ## ----- Parameters & initial conditions output
  
  derived_params <- cbind(
    NC, NV, NW, PF, PS, CS,
    birth_c, biterate, prob_infection_to_host, fit_adj, rec_adj, sigma_st,
    gamma_c, death_c, treatment_p, treatment_q, sigma_c, birth_v,
    death_v, feed.frequency, prob_infection_to_vector, gamma_v, emergence_p, emergence_f,
    reversion, K, birth_w, gamma_w, death_w, sigma_w, equil_vector_pop,
    waning, waning_f2s, new_prop, ten2fed, prop_prophylaxis
  )
  derived_params <- convert_array_to_named_vector(derived_params)
  
  qual_check_no0(derived_params) # ensure there are no negative values
  
  return(params = derived_params)
}


















# set_parameters <- function(this_scenario) {
#   birth_adj <- this_scenario$birth_adj
#   fit_adj <- this_scenario$fit_adj
#   K <- this_scenario$K
#   treat_prop <- this_scenario$treat_prop
#   prop_insecticide <- this_scenario$prop_insecticide
#   NW <- this_scenario$NW
#   prop_prophylaxis <- this_scenario$prop_prophylaxis
#   treatment_type <- this_scenario$treatment_type
#   dose_adj <- this_scenario$dose_adj
#   emergence_adj <- this_scenario$emergence_adj
# 
#   ## Cattle -----
#   birth_c <- 1 / (5 * 365)
#   biterate <- 0.8 / 4
#   prob_infection_to_host <- 0.46
#   gamma_c <- 1 / 15
#   resusceptible <- 10
#   death_c <- birth_c
#   death_p <- death_c
#   sigma_c <- 1 / 100
#   treatment <- 1 * treat_prop * (sigma_c + death_c) / (1 - treat_prop)
#   emergence <- 0
# 
# 
#   if (treatment_type == "quick") {
#     treatment_q <- treatment
#     treatment_p <- 0
#     emergence_p <- 0
#     emergence_f <- emergence * emergence_adj
#   } else {
#     if (treatment_type == "P") {
#       treatment_q <- 0
#       treatment_p <- treatment
#       emergence_p <- emergence * emergence_adj
#       emergence_f <- 0
#     } else {
#       treatment_q <- treatment
#       treatment_p <- treatment
#       emergence_p <- emergence * emergence_adj
#       emergence_f <- emergence * emergence_adj
#     }
#   }
# 
#   sigma_st <- (1 / 3) * dose_adj + sigma_c * (1 - dose_adj) #* 250 #LM: adjusted so that R0 drops below 1 when 99% treated to reflect Hargrove
#   rec_adj <- 1
#   waning <- (1 / 30) / dose_adj
#   waning_f2s <- (1 / 60) / dose_adj
#   new_prop <- 0
# 
# 
#   NC <- 50 # Total cattle
#   # figure out equilibrium in absence of infection
#   # birth_c * prop_prophylaxis *NC - death_c * PF - waning_f2s*PF
#   PF <- birth_c * prop_prophylaxis * NC / (death_c + waning_f2s)
#   # waning_f2s * PF - death_p * PS - waning * PS
#   PS <- waning_f2s * PF / (death_p + waning)
#   # birth_c * (1-prop_prophylaxis) * NC - death_c * CS + waning * PS
#   CS <- (birth_c * (1 - prop_prophylaxis) * NC + waning * PS) / death_c
# 
#   ## ----- Wildlife
#   birth_w <- 1 / 365
#   prob_infection.s_w <- 0.46
#   prob_infection.r_w <- 0.46
#   gamma_w <- 1 / 20
#   resusceptible_w <- 1 / 100
#   death_w <- birth_w
#   sigma_w <- sigma_c
#   reversion <- 0
# 
#   ## -----  Vectors
# 
#   ten2fed <- 1 / 4
#   qf <- 0.96 # Probability of surviving on a feeding day
#   qn <- 0.98 # Probability of surviving on a non-feeding day
#   feed.cyc <- 4 # Days between feeding
#   feed.frequency <- 0
#   prob_infection_to_vector <- 0.025
#   incubation <- 20
#   prop_insecticide.actual <- prop_insecticide * NC / (NC + NW) # Proportion of insecticide adjusted for wildlife
#   death_v <- -1 * log((1 - prop_insecticide.actual) * qf * qn^feed.cyc) / feed.cyc # Vector death rate
#   birth_v <- birth_adj * (-1) * log((1 - 0) * qf * qn^feed.cyc) / feed.cyc # Vector birth rate
#   equil_vector_pop <- max(0, K * (1 - death_v / birth_v)) # Vector equilibrium population
#   gamma_v <- death_v * exp(-death_v * incubation) / (1 - exp(-death_v * incubation)) # Rate from E to I
# 
#   NV <- equil_vector_pop # equil_vector_pop
# 
#   ## ----- Parameters & initial conditions output
# 
#   params <- cbind(
#     NC, NV, NW, PF, PS, CS,
#     birth_c, biterate, prob_infection_to_host, fit_adj, rec_adj, sigma_st,
#     gamma_c, death_c, treatment_p, treatment_q, sigma_c, birth_v,
#     death_v, feed.frequency, prob_infection_to_vector, gamma_v, emergence_p, emergence_f,
#     reversion, K, birth_w, gamma_w, death_w, sigma_w, equil_vector_pop,
#     waning, waning_f2s, new_prop, ten2fed, prop_prophylaxis
#   )
#   names <- colnames(params)
#   params <- as.vector(params)
#   names(params) <- names
# 
#   qual_check_no0(params) # ensure there are no negative values
# 
#   return(params = params)
# }

create_params_for_output <- function(params) {
  
  fixed_params_for_output <- c()
  fixed_params_for_output["cattle life span"] <- 1/params["death_c"]
  fixed_params_for_output["cattle inc period"] <- 1/params["gamma_c"]
  fixed_params_for_output["cattle inf period"] <- 1/params["sigma_c"]
  fixed_params_for_output["wildlife life span"] <- 1/params["death_w"]
  fixed_params_for_output["wildlife inc period"] <- 1/params["gamma_w"]
  fixed_params_for_output["wildlife inf period"] <- 1/params["sigma_w"]
  fixed_params_for_output["vector lifespan"] <- 1/params["death_v"]
  fixed_params_for_output["vector feed cyc"] <- 1/params["feed_cyc"]
  fixed_params_for_output["vector teneral period"] <- 1/params["ten2fed"]
  fixed_params_for_output["prob inf vector to host"] <- params["prob_infection_to_host"]
  fixed_params_for_output["prob inf host to vector"] <- params["prob_infection_to_vector"]
  fixed_params_for_output["cattle treatment period"] <- 1/params["sigma_st"]
  fixed_params_for_output["cattle proph full protection"] <- 1/params["waning_f2s"]
  fixed_params_for_output["cattle proph partial protection"] <- 1/params["waning"]
  
  fixed_params_for_output 
}


output_baseline_params_as_plot_and_csv <- function(report_time){
  
  params_for_output <- set_baseline_parameters()
  plot_this <- convert_named_vector_to_long_df(params_for_output)
  
  p1 <- plot_this %>% ggplot() +
    geom_bar(aes(y = value, x = name), fill = "skyblue", stat = "identity") + ylab(" ") + xlab(" ") + ylim(c(0, 2000)) +
    geom_text(aes(label= signif(value, 4), y = value, x = name), vjust = -0.1, hjust = -0.2) +
    coord_flip() +
    theme(text = element_text(size = 12), axis.text.x = element_text(angle=0, hjust=1, size = 8))
  
  print(p1)
  ggsave(p1, filename = paste0("output/baseline_params_plot_", report_time, ".pdf"))
  write.csv(df1, paste0("output/baseline_params_", report_time, ".csv"))
}



set_inital_conditions <- function(params, disease_present) {
  NC <- params["NC"]
  PF <- params["PF"]
  PS <- params["PS"]
  CS <- params["CS"]
  NW <- params["NW"]
  NV <- params["equil_vector_pop"]

  if (disease_present == TRUE) {
    CIr <- 0 # Infected (drug resistant strain)
    CIs <- 1 # Infected (drug sensitive strain)
  } else {
    CIr <- 0 # Infected (drug resistant strain)
    CIs <- 0 # Infected (drug sensitive strain)
  }

  CS <- CS - CIs - CIr # Susceptible
  CEs <- 0 # Exposed (drug sensitive strain)
  CEr <- 0 # Exposed (drug resistant strain)
  CTs <- 0 # Treated (drug sensitive strain)
  CTr <- 0 # Treated (drug resistant strain)

  PEs <- 0 # Exposed (drug sensitive strain)
  PEr <- 0 # Exposed (drug resistant strain)
  PIs <- 0 # Infected (drug sensitive strain)
  PIr <- 0 # Infected (drug resistant strain)
  PTs <- 0 # Treated (drug sensitive strain)
  PTr <- 0 # Treated (drug resistant strain)
  PR <- 0 # Recovered
  PPs <- 0
  PPr <- 0


  WIs <- 0 # Infected (drug sensitive strain)
  WS <- NW - WIs # Susceptible
  WEs <- 0 # Exposed (drug sensitive strain)
  WEr <- 0 # Exposed (drug resistant strain)
  WIr <- 0 # Infected (drug resistant strain)

  ## -----  Vectors
  VSt <- NV # Susceptible
  VSf <- 0
  VEs <- 0 # Exposed (drug sensitive strain)
  VEr <- 0 # Exposed (drug resistant strain)
  VIs <- 0 # Infected (drug sensitive strain)
  VIr <- 0 # Infected (drug resistant strain)

  ## ----- Initial conditions output

  inits <- cbind(
    CS, CEs, CEr, CIs, CIr, CTs, CTr, PF, PS, PEs, PEr, PIs,
    PIr, PTs, PTr, PPs, PPr, WS, WEs, WEr, WIs, WIr, VSt, VSf, VEs,
    VEr, VIs, VIr
  )
  names <- colnames(inits)
  inits <- as.vector(inits)
  names(inits) <- names

  qual_check_no0(inits) # ensure there are no negative values

  return(inits = inits)
}

get_variables <- function() {
  cattle_no_prophylaxis <- c("CS", "CEs", "CEr", "CIs", "CIr", "CTs", "CTr")
  cattle_with_prophylaxis <- c("PF", "PS", "PEs", "PEr", "PIs", "PIr", "PTs", "PTr", "PPs", "PPr")
  wildlife <- c("WS", "WEs", "WEr", "WIs", "WIr")
  vectors <- c("VSt", "VSf", "VEs", "VEr", "VIs", "VIr")
  
  variable_names <- list("cattle_no_prophylaxis" = cattle_no_prophylaxis,
                         "cattle_with_prophylaxis" = cattle_with_prophylaxis,
                         "wildlife" = wildlife, "vectors" = vectors)
  variable_names
}


initialise_variables_with_zeros <- function(variable_names) {
  vector_of_zeros <- rep(0.0, length(variable_names))
  names(vector_of_zeros) <- variable_names
  vector_of_zeros
}


set_inital_conditions2 <- function(params, number_initially_infected) {
  variables_names <- get_variables()
  
  cattle_no_prophylaxis <- initialise_variables_with_zeros(variables_names$cattle_no_prophylaxis)
  cattle_with_prophylaxis <- initialise_variables_with_zeros(variables_names$cattle_with_prophylaxis)
  wildlife <- initialise_variables_with_zeros(variables_names$wildlife)
  vectors <- initialise_variables_with_zeros(variables_names$vectors)
  
  cattle_no_prophylaxis["CS"] <- params["CS"]
  cattle_with_prophylaxis["PF"] <- params["PF"]
  wildlife["WS"] <- params["NW"]
  vectors["VSt"] <- params["equil_vector_pop"]
  
  cattle_no_prophylaxis["CIs"] <- number_initially_infected # Infected (drug resistant strain)
  cattle_no_prophylaxis["CS"] <- cattle_no_prophylaxis["CS"] - number_initially_infected
  
  ## ----- Initial conditions output
  
  inits <- c(cattle_no_prophylaxis, cattle_with_prophylaxis, wildlife, vectors)
  qual_check_no0(inits) # ensure there are no negative values
  
  return(inits)
}


findGlobals(fun = set_inital_conditions, merge = FALSE)$variables
findGlobals(fun = set_parameters_NEW, merge = FALSE)$variables



