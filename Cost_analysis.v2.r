source("cost_helper_functions.R")

#' This function contains the cost parameters of the current costs of drug, insecticide
#' used per head for treating cattle in the study area.
#' 
#' 
library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)
library(grid)
library(codetools) # by LM


# Production systems in Tanzania
#'Dairy and agro-pastoral production systems
# Function to return milk and draught parameters based on system
get_system_parameters <- function(system) {
  if (system == "Dairy") {
    return(list(milk = 4.1, 
                draught_days = 0))
  } else if (system == "Agro-pastoral") {
    return(list(milk = 1.17, 
                draught_days = 58.75)) #(Okello et al., 2021)
  } else {
    stop("Unknown production system")
  }
}

# Baseline parameters for cost estimation
get_economics_baseline_params <- function(system = "Agro-pastoral") {
  # Get system-specific parameters
  sys_params <- get_system_parameters(system)
  
  days_proph_protection <- 60
  days_insecticide_protection <- 15
  period_infection_bf_treatment <- 14
  average_milk_production <- sys_params$milk
  milk_output_reduct <- 0.15
  nu_days_draught_per_year <- sys_params$draught_days
  prop_adult_male <- 0.06
  prop_adult_female <- 0.44
  prop_calves <- 0.22
  prop_draughting <- 0.5
  relative_risk_adult <- 1
  relative_risk_calf <- 2
  P_abort_preg <- 0.178 #0.036 #Rowlands et al. 1994 & Rowlands et al. 1995
  gestation_period <- 283
  lactation_period <- 305
  end_first_trimester <- 93
  days_fallow <- 90

  params <- cbind(
    days_proph_protection, days_insecticide_protection, period_infection_bf_treatment, 
    average_milk_production, milk_output_reduct, nu_days_draught_per_year, prop_adult_male, 
    prop_adult_female, prop_calves, prop_draughting, relative_risk_adult, relative_risk_calf,
    P_abort_preg, gestation_period, lactation_period, end_first_trimester, days_fallow
  )
  
  params <- convert_array_to_named_vector(params)
  return(params)
}


# Cost parameters for treatment scenario
get_cost_params <- function(){
  cost_quick_curative <- 0.54 # DONE Cost of quick curative treatment per animal (Harriet)
  cost_proph <- 0.88 # DONE Cost of prophylactic treatment per animal (Harriet)
  cost_insecticide <- 0.10 # DONE Cost of insecticide per animal 
  cost_labour <- 0.07 #labour cost per animal treated - Opportunity cost of time
  cost_consumables_drug <- 0.17 # consumables cost per animal treated with drug (Muhanguzi et al. 2015)
  cost_consumables_insecticide <- 0.07 # DONE consumables cost per animal treated with insecticide 
  sale_per_litre = 0.348 # Estimated
  labour_per_litre <- cost_labour
  cost_per_litre <- sale_per_litre - labour_per_litre # profit from a litre  = sale - (labour - consumables). Consumable = 10% of sale
  cost_hire_oxen <- 3.0 #Meyer et al. 2018
  adult_sale <- 199.84 # Done Estimated
  calf_sale <-  92.4 # Done Estimated
  cost_diagnostic <-0
  
  cost_param <- cbind(cost_quick_curative, cost_proph, cost_insecticide, cost_labour, cost_consumables_drug,
                      cost_consumables_insecticide, cost_per_litre, cost_hire_oxen, calf_sale, adult_sale,
                      sale_per_litre, labour_per_litre, cost_diagnostic)
  
  cost_param <- convert_array_to_named_vector(cost_param)
  cost_param
}


# Treatment expenditure of treatment scenario
get_treat_expenditure <- function(df) {
  days_per_year <- set_days_per_year()
  
  df <- df %>%
    mutate(
      total_infected = CIs_final + CIr_final + PIs_final + PIr_final, # Total infected cases 
      total_susceptible = CS_final + CEs_final + CEsX_final + CEr_final + CErX_final + PF_final + PS_final + PEs_final + PEsX_final+ PEr_final + PErX_final, # Total susceptible cases
      
      #================== Scenario characteristics 
      # Total treatment cost combines
      treatment_cost = ((cost_quick_curative + cost_consumables_drug + cost_labour) * treatment_q * days_per_year * total_infected) +   #Quick curative treatment cost for infected animals,
        ((cost_proph + cost_consumables_drug + cost_labour)* treatment_p * days_per_year * total_infected) +        #Responsive prophylactic treatment cost for infected animals,
        ((cost_proph + cost_consumables_drug + cost_labour) * proph_ongoing * days_per_year * total_susceptible),    #Prophylactic treatment cost for susceptible animals,
      
      # Insecticide cost 
      insecticide_cost = (cost_insecticide + cost_consumables_insecticide + cost_labour) * prop_cattle_with_insecticide * (days_per_year / days_insecticide_protection) * NC, 
      
      #=================Total expenditure on treatment and insecticide=========================================
      treat_insecticide_cost = treatment_cost + insecticide_cost
      #=========================================================================================================
    )
  
  df
}


# Production losses estimation of treatment scenario
get_production_loss_estimate <- function(df) {
  days_per_year <- set_days_per_year()
  df <- get_fertility_estimate_2(df) # Get fertility estimates
  n_days = 100000
  print(n_days)
  df <- calculate_fertility_outputs(df, n_days = n_days, version = "v2") # Added by LM
  df <- df %>%
    mutate(
      #================== Production losses due of disease in the scenario=========================================
      # Milk production loss
      milk_production_loss_per_untreated_cow = milk_output_reduct * average_milk_production * cattle_infection_period * cost_per_litre,
      milk_production_loss_per_treated_cow = milk_output_reduct * average_milk_production * (cattle_treatment_period + period_infection_bf_treatment ) * cost_per_litre,
      milk_production_loss = (prob_lactating_infected_animals_untreated * milk_production_loss_per_untreated_cow) + 
                              (prob_lactating_infected_animals_treated * milk_production_loss_per_treated_cow), # per average infected cow
      #number_infected_milk_prod =  prop_adult_female * prob_lactating * Incidence,
      prob_lactating = lactation_period / (gestation_period - end_first_trimester + lactation_period),
      number_infected_milk_prod =  prop_adult_female * prob_lactating * Incidence, # Added by lM to make the code run
      cost_milk_prod_loss = milk_production_loss * number_infected_milk_prod, # Total cost due to milk production loss
      
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

# Get baseline scenarios and merge with treatment scenarios
get_baseline_scenarios <- function(test, scenarios_df, option) {
  if (option %in% c(1, 2)) {
    my_cols <- scenarios_df %>% 
      select(-treat_prop, -prop_cattle_with_insecticide) %>% 
      names()
    baseline_df <- test %>% 
      filter(treat_prop == 0.0, prop_cattle_with_insecticide == 0.0) %>%
      select(all_of(my_cols),
             prevalence,
             Incidence,
             Deaths_due_disease,
             cost_milk_prod_loss,
             cost_draught_power_loss,
             cost_mortality_loss_calf,
             cost_mortality_loss_adult,
             cost_calves_per_year_per,
             Xmilk_revenue_herd,
             Xcalf_revenue_herd) %>%
      rename(
        prevalence_baseline = prevalence,
        Incidence_baseline = Incidence,
        Deaths_due_disease_baseline = Deaths_due_disease,
        cost_milk_prod_loss_baseline = cost_milk_prod_loss,
        cost_draught_power_loss_baseline = cost_draught_power_loss,
        cost_mortality_loss_calf_baseline = cost_mortality_loss_calf,
        cost_mortality_loss_adult_baseline = cost_mortality_loss_adult,
        cost_calves_per_year_per_baseline = cost_calves_per_year_per,
        Xmilk_revenue_herd_baseline = Xmilk_revenue_herd,
        Xcalf_revenue_herd_baseline = Xcalf_revenue_herd
      )
    print("hello1")
    test %>% count(across(all_of(my_cols))) %>% filter(n > 1) %>% arrange(desc(n)) %>% print()
    print("hello2")
    baseline_df %>% count(across(all_of(my_cols))) %>% filter(n > 1) %>% arrange(desc(n)) %>% print()
    print("hello3")
    test_augmented <- left_join(test, baseline_df, by = my_cols, relationship = "many-to-many")
  } else if (option == 3) { # Fixed by LM
    
    my_cols <- c("NW", "K")
    
    baseline_df <- test %>%
      filter(proph_ongoing == 0, prop_cattle_with_insecticide == 0) %>%
      select(all_of(my_cols),
             prevalence,
             Incidence,
             Deaths_due_disease,
             cost_milk_prod_loss,
             cost_draught_power_loss,
             cost_mortality_loss_calf,
             cost_mortality_loss_adult,
             cost_calves_per_year_per,
             Xmilk_revenue_herd,
             Xcalf_revenue_herd) %>%
      rename(
        prevalence_baseline = prevalence,
        Incidence_baseline = Incidence,
        Deaths_due_disease_baseline = Deaths_due_disease,
        cost_milk_prod_loss_baseline = cost_milk_prod_loss,
        cost_draught_power_loss_baseline = cost_draught_power_loss,
        cost_mortality_loss_calf_baseline = cost_mortality_loss_calf,
        cost_mortality_loss_adult_baseline = cost_mortality_loss_adult,
        cost_calves_per_year_per_baseline = cost_calves_per_year_per,
        Xmilk_revenue_herd_baseline = Xmilk_revenue_herd,
        Xcalf_revenue_herd_baseline = Xcalf_revenue_herd
      )
    
    test_augmented <- left_join(test, baseline_df, by = my_cols, relationship = "many-to-many")
  }
}


# Parameter merge for cost estimation
economic_analysis <- function(df, system, option) {
  df <- df
  days_per_year <- set_days_per_year()
  cost_baseline <- get_economics_baseline_params(system) # Get the cost parameters for the baseline scenario
  cost_params <- get_cost_params() # Get the cost parameters for the treatment scenario
  
  cost_baseline <- as.data.frame(t(cost_baseline))
  cost_params <- as.data.frame(t(cost_params))
  df <- merge_params_into_this_scenario(df, cost_baseline) #merge cost parameters into model output
  df <- merge_params_into_this_scenario(df, cost_params) #merge cost parameters into model output
  
  df <- get_treat_expenditure(df) # Treatment expenditure estimation
  df <- get_production_loss_estimate(df) # Production losses estimation
  
  # user_inputs <- get_user_inputs()
  # if (user_inputs$multiple_scenarios == TRUE) {
  #   scenarios_df <- create_multiple_scenarios()
  #   } else {
  #     scenarios_df <- create_single_scenario()
  #   }
   scenarios_df <- df %>% select(NC, NW, K, K_host_ratio, use_carrying_capacity, 
                                 maintain_vector_pop, Baseline_vector_population,
                                 treat_prop, prop_cattle_with_insecticide, proph_ongoing) # Added by LM
  
  df <- get_baseline_scenarios(df, scenarios_df, option) # Get baseline scenarios and merge with treatment scenarios
  #glimpse(df[, tail(names(df), 15)])
  
  if (option == 1) {
    print ("Quick curative treatment scenario")
    } else if (option == 2) {           # LM else if fomratting fixed
    print ("Responsive prophylactic treatment scenario")
    } else if (option == 3) {
    print ("Prophylactic ongoing treatment scenario")
    }
  
  df <- df %>% 
    mutate(
      
      calves_per_year_per_baseline = max(cost_calves_per_year_per) - cost_calves_per_year_per_baseline, # To estimate the contribution of each loss component
      
      baseline_cost = cost_milk_prod_loss_baseline + cost_draught_power_loss_baseline + 
        cost_mortality_loss_calf_baseline + cost_mortality_loss_adult_baseline + 
        calves_per_year_per_baseline,
      #================== Production losses averted =========================================
      milk_losses_averted = cost_milk_prod_loss_baseline - cost_milk_prod_loss,
      milk_losses_averted2 = Xmilk_revenue_herd - Xmilk_revenue_herd_baseline,
      draught_losses_averted = cost_draught_power_loss_baseline - cost_draught_power_loss,
      calf_mortality_losses_averted = cost_mortality_loss_calf_baseline - cost_mortality_loss_calf,
      adult_mortality_losses_averted = cost_mortality_loss_adult_baseline - cost_mortality_loss_adult,
      mortality_losses_averted = calf_mortality_losses_averted + adult_mortality_losses_averted,
      
      #reversed for fertility losses
      averted_cost_calves_per_year_per_herd =  cost_calves_per_year_per - cost_calves_per_year_per_baseline,
      averted_cost_calves_per_year_per_herd2 = Xcalf_revenue_herd - Xcalf_revenue_herd_baseline,
      
      #Total production losses averted
      sum_averted_production_losses = milk_losses_averted + draught_losses_averted + 
        mortality_losses_averted + averted_cost_calves_per_year_per_herd,
      
      sum_averted_production_losses2 = milk_losses_averted2 + draught_losses_averted + 
        mortality_losses_averted + averted_cost_calves_per_year_per_herd2,
      
      #=================Net benefit estimation==========================================
      net_benefit = sum_averted_production_losses - treat_insecticide_cost,
      
      #=================Benefit-Cost Ratio estimation===================================
      BCR_scenario = ifelse(treat_insecticide_cost > 0, 
                            sum_averted_production_losses / treat_insecticide_cost, NA_real_),
      
      BCR_scenario2 = ifelse(treat_insecticide_cost > 0, 
                            sum_averted_production_losses2 / treat_insecticide_cost, NA_real_)
      
      #=================================================================================
      
    )
  df
}



#===============================================================================
# Example usage:
# params_vec <- cost_baseline_params()
# export_params_table_pdf(params_vec, pdf_file = "my_params.pdf")
export_params_table_pdf <- function(params_vec,
                                    pdf_file = "parameters_table.pdf",
                                    width = 8.5, height = 11) {
  
  # Convert the vector into a data frame
  params_df <- data.frame(
    Parameter = names(params_vec),
    Value     = round(as.numeric(params_vec), 3),
    row.names = NULL,
    stringsAsFactors = FALSE)
  
  # Open PDF device
  pdf(pdf_file, width = width, height = height)
  
  # Render the table
  grid.newpage()
  grid.table(params_df, rows = NULL)
  
  # Close device (writes file)
  dev.off()
  message("PDF saved to ", normalizePath(pdf_file))
}

#export_params_table_pdf(get_economics_baseline_params(), "cost_baseline_params.pdf") # by LM
#export_params_table_pdf(get_cost_params(), "cost_params.pdf") # by LM
findGlobals(fun = economic_analysis, merge = FALSE)$variables
findGlobals(fun = get_baseline_scenarios, merge = FALSE)$variables
findGlobals(fun = get_system_parameters, merge = FALSE)$variables
findGlobals(fun = get_baseline_parameters, merge = FALSE)$variables
findGlobals(fun = get_cost_params, merge = FALSE)$variables
findGlobals(fun = get_treat_expenditure, merge = FALSE)$variables
findGlobals(fun = get_production_loss_estimate, merge = FALSE)$variables
findGlobals(fun = export_params_table_pdf, merge = FALSE)$variables