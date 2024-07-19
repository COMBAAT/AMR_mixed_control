## ------------------------------------------------------ LOAD LIBRARIES

library(crayon)
library(codetools)
library(dplyr)
library(deSolve)
library(ggplot2)
library(lubridate)
library(patchwork)
library(stringr)
library(tictoc)
library(tidyr)

## ------------------------------------------------------ LOAD FUNCTIONS

source("0_set_non_epi_inputs.R")
source("1_input_scenarios.R")
source("funcs/set_params.R")
source("funcs/set_inits.R")
source("funcs/qual_check.R")
source("funcs/r0.R")
source("funcs/AAT_AMR_dens_dep.R")
source("funcs/helper_functions.R")
source("funcs/epi_outputs.R")
source("funcs/quick_plot.R")
source("funcs/output_baseline_params_and_scenarios.R")



## ----
# View inputs
user_inputs <- get_user_inputs()
scenarios_df <- get_scenarios()
baseline_parameters <- get_baseline_parameters()
plot_baseline_parameters(baseline_parameters)
plot_scenarios(scenarios_df)

# Create empty dataframe to store outputs
all_scenarios_summary <- data.frame()

## ---- Start run time estimates
tic()

## ---- Execute model
for (row in 1:nrow(scenarios_df)) {
  print(paste0("runnng scenario ", row, ", ", "total scenarios = ", nrow(scenarios_df)))

  this_scenario <- scenarios_df[row, ]
  params <- set_parameters_NEW(this_scenario)
  full_scenario <- merge_params_into_this_scenario(this_scenario, params)
  full_scenario <- append_descriptor(full_scenario, descriptor = user_inputs$current_descriptor)
  full_scenario <- move_populations_first(full_scenario)

  # Add R0 to full_scenario
  R0sen_and_R0res <- calculate_R0(params)
  R0sen <- R0sen_and_R0res["R0sen"]
  R0res <- R0sen_and_R0res["R0res"]
  full_scenario$R0sen <- R0sen
  full_scenario$R0res <- R0res

  ## Set simulation time dependent on R0 value
  ## Only run full simulation if R0 >= 1.0
  if (R0sen[1] < 1.0) {
    # if R0 < 1, set inits to disease free equilibrium and exit simulation after 0.1 day
    #inits <- set_inital_conditions(params, FALSE)
    inits <- set_inital_conditions2(params, 0)
    times <- seq(0, 0.1, 0.1)
  } else {
    # if R0 >= 1.0 run full simulation
    #inits <- set_inital_conditions(params, TRUE)
    inits <- set_inital_conditions2(params, 1)
    times <- seq(0, this_scenario$max_time, 1)
  }

  ## RUN MODEL ----
  # use rootfunc option to exit simulation when Rsen < 1.01 or Number infected cattle < 1e-5
  if (user_inputs$use_root_functions == TRUE) {
    time_trajectory <- ode(
      y = inits, parms = params, func = AAT_AMR_dens_dep, times = times,
      rootfunc = my_rootfun, events = list(root = TRUE, terminalroot = c(1, 2))
    )
  } else {
    time_trajectory <- ode(y = inits, parms = params, func = AAT_AMR_dens_dep, times = times, method = "daspk")
  }
  time_trajectory <- as.data.frame(time_trajectory)

  expanded_output <- add_population_totals(time_trajectory)
  expanded_output <- add_R_trajectories(params, expanded_output)

  final_state <- tail(expanded_output, 1)
  final_state <- append_suffix_to_column_names(final_state, "_final")
  final_state_with_full_scenario <- include_full_scenario(full_scenario, final_state)
  final_state_with_full_scenario <- append_epi_outputs_to_df(final_state_with_full_scenario)

  all_scenarios_summary <- rbind(all_scenarios_summary, final_state_with_full_scenario)

  print(paste0("final time = ", round(final_state$time, 1), " days"))
  print(paste0("R0 = ", final_state_with_full_scenario$R0sen))
  print(paste0("R = ", final_state_with_full_scenario$Rsen_final))
  
}


# save outputs as dataframe called test
test <- all_scenarios_summary
filename <- get_filename()
save(test, baseline_parameters, scenarios_df, file = filename) 

# some exploratory plots showing final simulation in scenario set
quick_plot(expanded_output)
quick_plot2(expanded_output)
quick_plot3(expanded_output)
R0_plot(expanded_output)

# display Rsen to check close to 1
all_scenarios_summary %>%
  filter(R0sen < 5) %>%
  ggplot() +
  geom_point(aes(y = Rsen_final, x = R0sen))

glimpse(all_scenarios_summary)

toc()
