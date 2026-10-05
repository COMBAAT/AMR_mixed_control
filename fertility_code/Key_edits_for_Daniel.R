get_production_loss_estimate <- function(df) {
  days_per_year <- set_days_per_year()
  df <- get_fertility_estimate_2(df) # Get fertility estimates
  df <- df %>%
    mutate(
      #================== Production losses due of disease in the scenario=========================================
      # Milk production loss
      # First account for milk production loss from animals infected when pregnant
      milk_production_loss_per_untreated_cow = milk_output_reduct * average_milk_production * cattle_infection_period * cost_per_litre,
      milk_production_loss_per_treated_cow = milk_output_reduct * average_milk_production * (cattle_treatment_period + period_infection_bf_treatment ) * cost_per_litre,
      milk_production_loss = (prob_lactating_infected_animals_untreated * milk_production_loss_per_untreated_cow) + 
        (prob_lactating_infected_animals_treated * milk_production_loss_per_treated_cow), # per average infected cow
      ##number_infected_milk_prod =  prop_adult_female * prob_lactating * Incidence,
      number_infected_milk_prod =  prop_adult_female * prob_pregnant_and_at_risk * Incidence,
      cost_milk_prod_loss_A = milk_production_loss * number_infected_milk_prod, 
      
      #Next account for milk production loss from animals infected while lactating
      #For animals infected near the start of lactation, infection / treatment will run its course during lactation
      #For animals infected near the end of lactation, infection / treatment could continue in to the next pregnancy so 
      #impact on milk production will be reduced
      #The relative reductions in impact will differ between the treated and untreated groups
      #Approach is to calculate the losses as though no reduction for infections at the end of lactation and then rescale
      rescale_untreated <- (L_sucess - cattle_infection_period) / L_success + 0.5 * cattle_infection_period / L_success,
      rescale_treated <- (L_success - cattle_treatment_period - period_infection_bf_treatment) / L_success +
        0.5 * (cattle_treatment_period + period_infection_bf_treatment) / L_success,
      
      number_infected_milk_prod_B =  prop_adult_female * prob_lactating * Incidence,
      milk_production_loss_untreated_B = (1-treat_prop) * milk_production_loss_per_untreated_cow * rescale_untreated,
      milk_production_loss_treated_B = (treat_prop) * milk_production_loss_per_treated_cow * rescale_treated,
      
      milk_production_loss_B = milk_production_loss_untreated_B + milk_production_loss_treated_B,
      cost_milk_prod_loss_B = milk_production_loss_B * number_infected_milk_prod_B, 
      
      cost_milk_prod_loss <- cost_milk_prod_loss_A + cost_milk_prod_loss_B,
      
      # Draught power loss
      draught_power_loss_untreated_oxen = (cattle_infection_period /days_per_year) * 
        nu_days_draught_per_year *  cost_hire_oxen, # draught power loss untreated oxen are unable to work
      draught_power_loss_treated_oxen = ((cattle_treatment_period + period_infection_bf_treatment) / days_per_year) * 
        nu_days_draught_per_year * cost_hire_oxen, # draught power loss treated oxen are unable to work
      draught_power_loss = ((1 - treat_prop) * draught_power_loss_untreated_oxen) + 
        (treat_prop * draught_power_loss_treated_oxen), # Total draught power loss
      N_infected_draught_oxen = prop_adult_male * prop_draughting * Incidence, # number of infected draught oxen
      cost_draught_power_loss = N_infected_draught_oxen * draught_power_loss, # Total cost for draught power loss
      
      # Disease related deaths
      proportion_deaths_calves = (relative_risk_calf * prop_calves) /
        ((relative_risk_calf * prop_calves) + (relative_risk_adult * (1 - prop_calves))), # Proportion of deaths that are calves
      cost_mortality_loss_calf = proportion_deaths_calves * Deaths_due_disease * calf_sale, # Cost of calf replacement
      cost_mortality_loss_adult = (1 - proportion_deaths_calves) * Deaths_due_disease * adult_sale, # Cost of adult cattle replacement
      cost_mortality_loss = cost_mortality_loss_calf + cost_mortality_loss_adult, # Total cost of disease related deaths
      
      # Fertility 
      cost_calves_per_year_per = calves_per_year_per_female * prop_adult_female * NC * calf_sale,
      
    )
  df
}

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
  daily_incidence_per_adult_female <- daily_incidence_per_head * proportion_adult_female
  
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
  
  #############################################################################
  # Additional variables to return
  # this probability should replace prob_lactating in line 12 of this script
  df[["prob_pregnant_and_at_risk"]] <- (days_pregnant - end_first_trimester) / CI_expected
  
  #############################################################################
  
  return(df)
}