run_one_scenario <- function(row, scenarios_df, user_inputs) {
  
  this_scenario <- scenarios_df[row, ]
  params <- set_parameters(this_scenario)
  full_scenario <- merge_params_into_this_scenario(this_scenario, params)
  full_scenario <- append_descriptor(full_scenario, descriptor = user_inputs$current_descriptor)
  full_scenario <- move_populations_first(full_scenario)
  
  # Add R0 to full_scenario
  # R0sen2 and R0res2 are calculated using the next generation matrix method as a check
  R0sen_and_R0res <- calculate_R0(params)
  R0sen <- R0sen_and_R0res["R0sen"]
  R0res <- R0sen_and_R0res["R0res"]
  R0sen2 <- R0sen_and_R0res["R0sen2"]
  R0res2 <- R0sen_and_R0res["R0res2"]
  full_scenario$R0sen <- R0sen
  full_scenario$R0sen2 <- R0sen2
  full_scenario$R0res <- R0res
  full_scenario$R0res2 <- R0res2
  
  ## Make the simulation time dependent on R0 value
  ## Only run full simulation if R0 >= 1.0
  if (R0sen < 1.0) {
    # if R0 < 1, set inits to disease free equilibrium and exit simulation after 0.1 day
    inits <- set_inital_conditions2(params, initial_sensitive_infections = 0, initial_resistant_infections = 0)
    inits2 <- adjust_inits(inits)
    times <- seq(0, 0.1, 0.1)
  } else {
    # if R0 >= 1.0 run full simulation
    inits <- set_inital_conditions2(params, initial_sensitive_infections = 1, initial_resistant_infections = 0)
    inits2 <- adjust_inits(inits)
    times <- seq(0, this_scenario$max_time, 1)
  }
  
  ## RUN MODEL ----
  # use rootfunc option to exit simulation when Rsen < 1.01 or Number infected cattle < 1e-5
  if (user_inputs$use_root_functions == TRUE) {
    time_trajectory <- ode(
      y = inits2, parms = params, func = AAT_AMR_dens_dep, times = times,
      rootfunc = my_rootfun, events = list(root = TRUE, terminalroot = c(1, 2))
    )
  } else {
    time_trajectory <- ode(y = inits2, parms = params, func = AAT_AMR_dens_dep, times = times, method = "daspk")
  }
  time_trajectory <- as.data.frame(time_trajectory)
  time_trajectory <- time_trajectory %>% 
    mutate(PF = PF1 + PF2 + PF3 + PF4 + PF5 + PF6) %>% 
    select(-PF1, -PF2, -PF3, -PF4, -PF5, -PF6)
  
  expanded_output <- add_population_totals(time_trajectory)
  expanded_output <- add_R_trajectories(params, expanded_output)
  expanded_output <- add_R0(params, expanded_output)
  
  final_state <- tail(expanded_output, 1)
  final_state <- append_suffix_to_column_names(final_state, "_final")
  final_state_with_full_scenario <- include_full_scenario(full_scenario, final_state)
  final_state_with_full_scenario <- append_epi_outputs_to_df(final_state_with_full_scenario)
  
  #all_simulations_summary <- rbind(all_simulations_summary, final_state_with_full_scenario)
  
  print(paste0("final time = ", round(final_state$time, 1), " days"))
  print(paste0("R0 = ", final_state_with_full_scenario$R0sen))
  print(paste0("Rsen_final = ", final_state_with_full_scenario$Rsen_final))
  # print(paste0("Rsen2_final = ", final_state_with_full_scenario$Rsen2_final))
  # print(paste0("Rres_final = ", final_state_with_full_scenario$Rres_final))
  # print(paste0("Rres2_final = ", final_state_with_full_scenario$Rres2_final))
  #print(quick_plot(expanded_output))
  
  final_state_with_full_scenario
}