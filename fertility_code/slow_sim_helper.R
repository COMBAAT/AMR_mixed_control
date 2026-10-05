# Fertility and infection model that avoids dataframe computation
library(tictoc)
library(dplyr)

create_infection_and_fertility_simulation_v1 <- function(n_days, days_per_year,
                                                      herd_size, 
                                                      annual_incidence, 
                                                      treat_prop, 
                                                      P_abort_preg, 
                                                      end_first_trimester, 
                                                      cattle_infection_period,
                                                      cattle_treatment_period,
                                                      period_infection_bf_treatment,
                                                      gestation_period,
                                                      lactation_period,
                                                      fallow_period,
                                                      milk_output_reduct) {
                                                      #average_milk_production,
                                                      #sale_per_litre,
                                                      #labour_per_litre,
                                                      #calf_sale,
                                                      #prop_adult_female)
  gestation_period <- gestation_period - end_first_trimester
  end_first_trimester <- end_first_trimester - end_first_trimester
  
  daily_incidence <- annual_incidence / days_per_year
  daily_incidence_per_head <- daily_incidence / herd_size
  daily_incidence_per_adult_female <- daily_incidence_per_head #* proportion_adult_female
  
  infection_event <- rbinom(n_days, 1, daily_incidence_per_adult_female) #NEW
  treatment_event <- rbinom(n_days, 1, treat_prop) #NEW
  infection_status <- rep("none", n_days)
  days_in_infected_state <- rep(0, n_days)
  
  ############################################################################
  # Define vectors with durations
  duration_max_infection <- c(
    none              = 0L,
    infected_Untreated = cattle_infection_period,
    infected_Treated   = cattle_treatment_period + period_infection_bf_treatment
  )
  
  duration_max_fertility <- c(
    pregnant  = gestation_period,
    lactating = lactation_period,
    fallow    = fallow_period
  )
  
  next_fertility <- c(
    pregnant  = "lactating",
    lactating = "pregnant",
    fallow    = "pregnant"
  )
  ############################################################################
  
  # Create sequence of infections and treatments
  for (i in 2:n_days) {
    if (infection_status[i-1] == "none"){
      if (infection_event[i-1] == 0) {
        infection_status[i] <- "none"
      } else if (treatment_event[i-1] == 1) {
        infection_status[i] <- "infected_Treated"
        days_in_infected_state[i] <- 1
      } else {
        infection_status[i] <- "infected_Untreated"
        days_in_infected_state[i] <- 1
      }
    }
    
    if (infection_status[i-1] != "none") {
      max_duration_in_infected_state <- duration_max_infection[infection_status[i-1]]
      
      if (days_in_infected_state[i-1] < max_duration_in_infected_state) {
        days_in_infected_state[i] <- days_in_infected_state[i-1] + 1
        infection_status[i] <- infection_status[i-1]
      } else {
        days_in_infected_state[i] <- 0
        infection_status[i] <- "none"
      }
    }
  }
  #stopifnot(all(infection_status %in% c("none","infected_Untreated","infected_Treated")))
  
  # Now layer on the fertility (cached version)
  abortion_event <- rbinom(n_days, 1, P_abort_preg)
  calves <- integer(n_days)
  fertility_status <- rep(NA_character_, n_days)
  days_in_fertility_state <- integer(n_days)
  
  fertility_status[1] <- "pregnant"
  days_in_fertility_state[1] <- 1
  
  for (i in 2:n_days) {
    calves[i] <- calves[i-1]
    
    # Cache "previous" values used multiple times
    prev_fertility_status <- fertility_status[i-1]
    prev_days_in_fertility_state <- days_in_fertility_state[i-1]
    
    # Abortion trigger: new untreated infection in pregnancy after 1st trimester
    if (infection_status[i-1] == "none" &&
        infection_status[i]   == "infected_Untreated" &&
        prev_fertility_status      == "pregnant" &&
        prev_days_in_fertility_state        > end_first_trimester &&
        abortion_event[i-1]   == 1) {
      
      fertility_status[i] <- "fallow"
      days_in_fertility_state[i] <- 1
      
    } else {
      # Cache duration lookup (was previously recomputed)
      max_duration_in_fertility_state <- duration_max_fertility[prev_fertility_status]
      
      if (prev_days_in_fertility_state < max_duration_in_fertility_state) {
        fertility_status[i] <- prev_fertility_status
        days_in_fertility_state[i] <- prev_days_in_fertility_state + 1
      } else {
        next_status <- next_fertility[prev_fertility_status]
        fertility_status[i] <- next_status
        days_in_fertility_state[i] <- 1
        
        if (next_status == "lactating") {
          calves[i] <- calves[i] + 1
        }
      }
    }
  }
  
  milk_days <- numeric(n_days)
  milk_days[fertility_status == "lactating" & infection_status == "none"] <- 1
  milk_days[fertility_status == "lactating" & infection_status != "none"] <- 1 - milk_output_reduct
  
  calves_per_year_per_adult_female <- calves[n_days] / n_days * days_per_year
  milk_days_per_year_per_adult_female <- sum(milk_days) / n_days * days_per_year
  
  
  # df <- data.frame(day = 1:n_days, infection_event, treatment_event, infection_status, days_in_infected_state, 
  #                  abortion_event, fertility_status, days_in_fertility_state, calves, 
  #                  milk_days, milk_revenue)
  # df
  list(sim = "v1", calves_per_year_per_adult_female = calves_per_year_per_adult_female, 
       milk_days_per_year_per_adult_female = milk_days_per_year_per_adult_female)
}


