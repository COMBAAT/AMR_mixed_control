# For an event that happens at rate lambda (eg infection) we want to know how long we have to wait
# If the time period is infinite, the answer is just 1 / lambda
# We have to adjust this if we are interested in the mean time for the events that happen within t_max
waiting_time_over_finite_interval <- function(lambda, t_max) {
  waiting_time <- ifelse(lambda <= 0,
                         t_max,  # if no event occurs, mean waiting time equals full interval
                         1 / lambda - t_max * exp(-lambda * t_max) / (1 - exp(-lambda * t_max)))
  return(waiting_time)
}


get_fertility_estimate <- function(df) {
  
  annual_incidence <- df[["Incidence"]]
  days_per_year <- set_days_per_year()
  herd_size <- df[["NC"]]
  prop_adult_female <- df[["prop_adult_female"]]
  treat_prop <- df[["treat_prop"]]
  prob_late_pregnancy_infection_cause_abortion <- df[["P_abort_preg"]]
  lactation_period <- df[["lactation_period"]]
  days_fallow <- df[["days_fallow"]]
  gestation_period <- df[["gestation_period"]]
  end_second_trimester <- df[["end_second_trimester"]]
  
  
  # Infection incidence estimation
  daily_incidence <- annual_incidence / days_per_year
  daily_incidence_per_head <- daily_incidence / herd_size
  daily_incidence_per_adult_female <- daily_incidence_per_head * prop_adult_female
  
  # Days in third trimester
  days_end_preg <- gestation_period - end_second_trimester
  
  # Mean days of stay in the third trimester before abortion occurs
  mean_wait <- waiting_time_over_finite_interval(daily_incidence_per_adult_female, days_end_preg)
  dloss <- days_end_preg - mean_wait # days lost due to abortion in third trimester
  
  prob_no_infection <- (1 - daily_incidence_per_adult_female)^days_end_preg
  prob_infection <- 1 - prob_no_infection
  prob_infected_animal_proceeds_to_lactation <- (treat_prop + (1 - treat_prop) * (1 - prob_late_pregnancy_infection_cause_abortion))
  
  # p1 is prob of no abortion occuring by end of pregnancy
  p1 <- prob_no_infection + prob_infection * prob_infected_animal_proceeds_to_lactation
  
  # p2 is the prob that an abortion does occur 
  p2 <- 1 - p1
  
  # Calving interval and pregnancies per year
  pregnancy_interval <- gestation_period + p1 * lactation_period + p2 * (days_fallow - dloss)
  pregnancies_per_year <- days_per_year / pregnancy_interval
  
  df[["pregnancy_interval"]] <- pregnancy_interval
  df[["pregnancies_per_year"]] <- pregnancies_per_year
  
  # proportion of pregnancies end in abortion is 1 - p1
  df[["calves_per_year_per_female"]] <- pregnancies_per_year * p1
  
  #Probability of pregnant
  df[["prob_pregnant"]] <- (gestation_period - p2 * dloss) / pregnancy_interval
  
  #Probability of lactating
  df[["prob_lactating"]] <- p1 * lactation_period / pregnancy_interval
  
  #Probability of fallow
  df[["prob_fallow"]] <-  p2 * days_fallow / pregnancy_interval
  
  #Probability of lactating infected animals treated
  df[["prob_lactating_infected_animals_treated"]] <- treat_prop / prob_infected_animal_proceeds_to_lactation
  
  #Probability of lactating infected animals untreated
  df[["prob_lactating_infected_animals_untreated"]] <- (1 - treat_prop) * (1 - prob_late_pregnancy_infection_cause_abortion) / prob_infected_animal_proceeds_to_lactation
  
  df
}



#-------------------------------------------------------
# Now conduct mathematical calculation
#-------------------------------------------------------
get_fertility_estimate_2 <- function(df) {
  annual_incidence <- df[["Incidence"]]
  treat_prop <- df[["treat_prop"]]
  prob_abort_given_infection <- df[["P_abort_preg"]]
  days_pregnant <- df[["gestation_period"]]
  end_first_trimester <- df[["end_first_trimester"]]
  days_lactating <- df[["lactation_period"]]
  days_fallow <- df[["days_fallow"]]
  days_per_year <- set_days_per_year()
  herd_size <- df[["NC"]]
  proportion_adult_female <- df[["prop_adult_female"]]
  
  
  # Daily hazard
  daily_incidence <- annual_incidence / days_per_year
  daily_incidence_per_head <- daily_incidence / herd_size
  daily_incidence_per_adult_female <- daily_incidence_per_head #* proportion_adult_female adjusted by LM
  
  # Length of pregnancy window at risk of infection
  days_end_preg <- days_pregnant - end_first_trimester
  
  # Probability of infection happening in pregnancy window
  p_no_inf_window <- (1 - daily_incidence_per_adult_female)^days_end_preg
  p_inf_window <- 1 - p_no_inf_window
  
  # Probability infection does NOT cause abortion
  p_infected_proceed_lactation <-
    (1 - prob_abort_given_infection) * (1 - treat_prop) + treat_prop
  
  # Probability of no abortion
  p_no_abort <- p_no_inf_window + p_inf_window * p_infected_proceed_lactation
  p_abort <- 1 - p_no_abort
  
  # Expected time to abortion given that abortion occurs
  d_abort <- waiting_time_over_finite_interval(
    daily_incidence_per_adult_female,
    days_end_preg
  )
  
  # Pregnancy days until abortion
  L_abort_preg <- end_first_trimester + d_abort
  
  # Successful cycle length:
  # Pregnancy + Lactation minus first trimester overlap
  L_success <- days_pregnant + days_lactating - end_first_trimester
  
  # Expected calving interval:
  # CI = p_no_abort * L_success + p_abort * (L_abort_preg + days_fallow + CI)
  # The recursion applies because of the possibility of another abortion after the fallow period
  # Rearranging gives:
  CI_expected <-
    (p_no_abort * L_success + p_abort * (L_abort_preg + days_fallow)) / (1 - p_abort)
  
  #Calving interval
  df[["CI_expected"]] <- CI_expected
  
  #Calves per year
  df[["calves_per_year_per_female"]] <- days_per_year / CI_expected
  
  #Probability of lactating infected animals treated
  df[["prob_lactating_infected_animals_treated"]] <- treat_prop / p_infected_proceed_lactation
  
  #Probability of lactating infected animals untreated
  df[["prob_lactating_infected_animals_untreated"]] <- (1 - treat_prop) * (1 - prob_abort_given_infection) / p_infected_proceed_lactation
  
  return(df)
}

