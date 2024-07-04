
## ------------------------------------------------------ LOAD LIBRARIES

library(deSolve)
library(tictoc)
library(progress)
library(ggplot2)
library(dplyr)
library(gghighlight)
library(cowplot)
library(crayon)

## ------------------------------------------------------ LOAD FUNCTIONS

source("funcs/AAT_AMR_dens_dep.R")
source("funcs/r0.R")
source("funcs/set1_params_inits.R")
source("funcs/qual_check.R")
source("funcs/quick_plot.R")
source("funcs/helper_functions.R")
source("input_scenarios.R")

## ---- 
# Specify multiple or single scenario
multiple_scenarios <- TRUE
# Create dataframe of parameter combinations for each scenario
if (multiple_scenarios == TRUE) {
  scenarios_df <- create_multiple_scenarios()
} else {
  scenarios_df <- create_single_scenario()
}

# Create empty dataframes to store outputs
df <- data.frame()
df2 <- data.frame()

## ---- Run time estimates
tic()
start.time <- Sys.time()

## ---- Execute model
for (row in 1:nrow(scenarios_df)) {
  print(row)
  
                this_scenario <- scenarios_df[row, ]
                params <- set_parameters(this_scenario)
                params_df <- convert_named_vector_to_df(params)
                full_scenario <- merge_dfs_without_duplicate_columns(this_scenario, params_df)
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
                  inits <- set_inital_conditions(params, disease_present = FALSE)
                  times <- seq(0, 0.1, 0.1)
                } else {
                  # if R0 >= 1.0 run full simulation
                  inits <- set_inital_conditions(params, disease_present = TRUE)
                  times <- seq(0, this_scenario$max_time, 1)
                }
                
                ## RUN MODEL ----
                # use rootfunc option to exit simulation when Rsen < 1.01 or Number infected cattle < 1e-5
                use_root_functions <- TRUE
                if (use_root_functions == TRUE) {
                  time_trajectory <- ode(y = inits, parms = params, func = AAT_AMR_dens_dep, times = times,
                            rootfunc = my_rootfun3, events = list(root = TRUE, terminalroot = c(1,2)))
                } else {
                  time_trajectory <- ode(y = inits, parms = params, func = AAT_AMR_dens_dep, times = times, method = "daspk")
                }
                time_trajectory <- as.data.frame(time_trajectory)
               
                expanded_output <- add_population_totals(time_trajectory)
                expanded_output <- add_R_trajectories(params, expanded_output)
                
                final_state <- tail(expanded_output, 1)
                final_state <- append_suffix_to_column_names(final_state, "_final")
                final_state_with_full_scenario <- include_parameters(full_scenario, final_state)
                final_state_with_full_scenario <- append_epi_outputs_to_df(final_state_with_full_scenario)
                
                #epi_outputs <- calculate_epi_outputs(this_scenario$treatment_type, params, final_state)

                selected_outputs <- final_state_with_full_scenario
                df2 = rbind(df2, selected_outputs)
                
                #wide <- cbind(selected_outputs) #, final_state)
                #df2 = rbind(df2, wide)
                
                print(final_state$time)
                
}  

toc()

test <- df2
time <- format(Sys.time(), "%a %b %d %X %Y")
save(test, file = paste0("output/test_", this_scenario$treatment_type, "_play2", ".Rda"))
#save(test,file ="output/test.Rda")

quick_plot(expanded_output)
quick_plot2(expanded_output)
quick_plot3(expanded_output)
R0_plot(expanded_output)

#df2 %>% filter(R0sen < 5) %>% ggplot() + geom_point(aes(y = Rsen, x = R0sen))
glimpse(df2)
