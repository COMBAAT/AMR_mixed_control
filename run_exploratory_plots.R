## ------------------------------------------------------ LOAD LIBRARIES

library(deSolve)
library(tictoc)
library(lubridate)
library(ggplot2)
library(dplyr)
library(gghighlight)
library(cowplot)
library(crayon)
library(codetools)

## ------------------------------------------------------ LOAD FUNCTIONS

source("input_scenarios.R")
source("funcs/set_params.R")
source("funcs/set_inits.R")
source("funcs/qual_check.R")
source("funcs/r0.R")
source("funcs/AAT_AMR_dens_dep.R")
source("funcs/helper_functions.R")
source("funcs/epi_outputs.R")
source("funcs/plot_helper.R")
source("funcs/quick_plot.R")



## ----
# User choices
user_inputs <- set_user_inputs()

# Create dataframe of parameter combinations for each scenario
if (user_inputs$multiple_scenarios == TRUE) {
  scenarios_df <- create_multiple_scenarios(label = user_inputs$label)
} else {
  scenarios_df <- create_single_scenario(label = user_inputs$label)
}

# Create empty dataframe to store outputs
all_scenarios_summary <- data.frame()

# Create filename for .Rda file
if (user_inputs$append_current_time_to_output_file == TRUE) {
  file_label <- get_label_with_current_datetime(user_inputs$label)
} else {
  file_label <- user_inputs$label
}
file_name <- paste0("output/", this_scenario$treatment_type, "_treatment_", file_label)

## ---- Start run time estimates
tic()

## ---- Execute model
for (row in 1:nrow(scenarios_df)) {
  print(paste0("runnng scenario ", row, ", ", "total scenarios = ", nrow(scenarios_df)))

  this_scenario <- scenarios_df[row, ]
  params <- set_parameters_NEW(this_scenario)
  full_scenario <- merge_params_into_this_scenario(this_scenario, params)
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
}

# save outputs as dataframe called test
test <- all_scenarios_summary
save(test, file = paste0(file_name, ".Rda")) 

# some exploratory plots
quick_plot(expanded_output)
quick_plot2(expanded_output)
quick_plot3(expanded_output)
R0_plot(expanded_output)

all_scenarios_summary %>%
  filter(R0sen < 5) %>%
  ggplot() +
  geom_point(aes(y = Rsen_final, x = R0sen))

glimpse(all_scenarios_summary)

toc()



test %>% filter(prop_cattle_with_insecticide < 1.0) %>% 
  ggplot() + 
  geom_point(aes(x = death_v_option1, y = death_v, shape = as.factor(NW), colour = as.factor(prop_cattle_with_insecticide))) + 
  geom_abline(aes(slope = 1, intercept = 0), colour = "red")
