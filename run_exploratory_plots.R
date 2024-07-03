
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
multiple_scenarios <- FALSE
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
 
                # Add R0 to this_scenario_df
                R0sen_and_R0res <- calculate_R0(params)
                R0sen <- R0sen_and_R0res["R0sen"]
                R0res <- R0sen_and_R0res["R0res"]
                this_scenario$R0sen <- R0sen
                this_scenario$R0res <- R0res
                
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
                use_root_functions <- TRUE
                if (use_root_functions == TRUE) {
                  # use rootfunc option to exist simulation when Rsen < 1.01 or Number infected cattle < 1e-5
                  out <- ode(y = inits, parms = params, func = AAT_AMR_dens_dep, times = times,
                            rootfunc = my_rootfun3, events = list(root = TRUE, terminalroot = c(1,2)))
                } else {
                  out <- ode(y = inits, parms = params, func = AAT_AMR_dens_dep, times = times, method = "daspk")
                }
                out <- as.data.frame(out)
               
                expanded_output <- add_totals(out)
                expanded_output <- add_R_trajectories(params, expanded_output)
                last <- tail(expanded_output, 1)
                
                epi_outputs <- calculate_epi_outputs(this_scenario$treatment_type, params, last)
                params_df <- as.data.frame(as.list(params))
                
                selected_outputs <- cbind( data.frame(scenario_id = row, 
                                               this_scenario,
                                               epi_outputs
                                               ))
                df = rbind(df, selected_outputs)
                
                wide <- cbind(selected_outputs, last)
                df2 = rbind(df2, wide)
                
                print(last$time)
                
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

