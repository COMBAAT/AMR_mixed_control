
calculate_fertility_outputs <- function(input_df, n_days, version){
  
  print(paste0("input_df has this many rows ", nrow(input_df)))
  print(version)
  create_infection_and_fertility_simulation <- if(version == "v1"){
    create_infection_and_fertility_simulation_v1 
  } else {
    create_infection_and_fertility_simulation_v2
  }
  
  
  days_per_year <- 365.25
  number_scenarios <- nrow(input_df)
  
  calves_per_year <- rep(0, number_scenarios)
  milk_days_per_year <- rep(0, number_scenarios)
  milk_revenue <- rep(0, number_scenarios)
  
  herd_size_vec <- input_df$NC
  annual_incidence_vec <- input_df$Incidence
  treat_prop_vec <- input_df$treat_prop
  
  P_abort_preg_vec <- input_df$P_abort_preg
  end_first_trimester_vec <- input_df$end_first_trimester
  cattle_infection_period_vec = input_df$cattle_infection_period
  cattle_treatment_period_vec = input_df$cattle_treatment_period
  period_infection_bf_treatment_vec = input_df$period_infection_bf_treatment
  gestation_period_vec = input_df$gestation_period
  lactation_period_vec = input_df$lactation_period
  fallow_period_vec = input_df$days_fallow
  milk_output_reduct_vec = input_df$milk_output_reduct
  average_milk_production_vec = input_df$average_milk_production
  sale_per_litre_vec = input_df$sale_per_litre
  labour_per_litre_vec = input_df$labour_per_litre
  calf_sale_vec = input_df$calf_sale
  prop_adult_female_vec = input_df$prop_adult_female
  
  calves_per_year_per_adult_female <- numeric(number_scenarios)
  milk_days_per_year_per_adult_female <- numeric(number_scenarios)
  
  for (i in 1:number_scenarios){
    if (i %% 10 == 0) {
      print(i)
    }
    herd_size <- herd_size_vec[i]
    annual_incidence <- annual_incidence_vec[i]
    treat_prop <- treat_prop_vec[i]
    P_abort_preg <- P_abort_preg_vec[i]
    end_first_trimester <- end_first_trimester_vec[i]
    cattle_infection_period = cattle_infection_period_vec[i]
    cattle_treatment_period = cattle_treatment_period_vec[i]
    period_infection_bf_treatment = period_infection_bf_treatment_vec[i]
    gestation_period = gestation_period_vec[i]
    lactation_period = lactation_period_vec[i]
    fallow_period = fallow_period_vec[i]
    milk_output_reduct = milk_output_reduct_vec[i]
    average_milk_production = average_milk_production_vec[i]
    sale_per_litre = sale_per_litre_vec[i]
    labour_per_litre = labour_per_litre_vec[i]
    calf_sale = calf_sale_vec[i]
    prop_adult_female = prop_adult_female_vec[i]
    
    output_list <- create_infection_and_fertility_simulation(n_days, days_per_year,
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
                                                             milk_output_reduct)
    #average_milk_production,
    #sale_per_litre,
    #labour_per_litre,
    #calf_sale,
    #prop_adult_female)
    
    calves_per_year_per_adult_female[i] <- output_list[["calves_per_year_per_adult_female"]]
    milk_days_per_year_per_adult_female[i] <- output_list[["milk_days_per_year_per_adult_female"]]
    
  }
  
  input_df$Xcalves_per_adult_female <- calves_per_year_per_adult_female
  input_df$Xmilk_days_per_year_per_adult_female <- milk_days_per_year_per_adult_female
  
  input_df <- input_df %>% mutate(Xcalf_revenue = Xcalves_per_adult_female * calf_sale,
                                  Xcalf_revenue_herd = Xcalf_revenue * herd_size * prop_adult_female, 
                                  Xmilk_production_per_year_per_adult_female = Xmilk_days_per_year_per_adult_female * average_milk_production,
                                  Xmilk_revenue_per_year_per_adult_female    = Xmilk_production_per_year_per_adult_female * (sale_per_litre - labour_per_litre),
                                  Xmilk_revenue_herd = Xmilk_revenue_per_year_per_adult_female * herd_size * prop_adult_female)
  
  input_df
}



