
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
source("funcs/ode_stopping_functions.R")
source("input_scenarios.R")

## ---- 
# Specify multiple or single scenario
multiple_scenarios <- FALSE
# Create dataframe of parameter combinations for each scenario
scenarios_df <- input_scenarios(multiple_scenarios)

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
                
                # Get params and intis for this scenario
                params_and_inits <- set1(this_scenario)
                params <- params_and_inits[["params"]]
                inits <- params_and_inits[["inits"]]
                
                # Remove unused parameters and variables
                myvars <- names(params) %in% c("resusceptible", "resusceptible.w") 
                params <- params[!myvars]
                myvars2 <- names(inits) %in% c("CR", "PR", "WR") 
                inits <- inits[!myvars2]
                
                ## R0 calculations - only run full simulation if R0 >= 1.0
                R0sen_and_R0res <- calculate_R0_from_inits(inits, params)
                R0sen <- R0sen_and_R0res$R0sen
                
                ## Set simulation times dependent on R0 value
                if (R0sen[1] < 1.0){
                  # if R0 < 1, set inits to disease free equilibrium and exit simulation after 0.1 day
                  inits['CS'] <- inits['CS'] + inits['CIs']
                  inits['CIs'] <- 0
                  times <- seq(0, 0.1, 0.1)
                } else {
                  # if R0 >= 1.0 run full simulation
                  times <- seq(0, this_scenario$max_time, 1)
                }
                
                ## RUN MODEL ----
                #out <-ode(y = inits, parms = params, func = AAT_AMR_dens_dep, times = times, method = "daspk")
                # use rootfunc option to exist simulation when Rsen < 1.01 or Number infected cattle < 1e-5
                out <-ode(y = inits, parms = params, func = AAT_AMR_dens_dep, times = times,
                          rootfunc = my_rootfun3, events = list(root = TRUE, terminalroot = c(1,2)))
                out <- as.data.frame(out)
                names(out)[names(out) == 'time'] <- "times"
                
                expanded_output <- add_totals(out)
                expanded_output <- add_R0(inits, params, expanded_output)
                expanded_output <- add_R_trajectories(params, expanded_output)
                last <- tail(expanded_output, 1)
                
                # #Check get 1 at equilibrium when run with only sensitive strains
                # #fraction of cattle available for infection by sensitive strain
                # fC <- last$CS / Nc
                # #fraction of vectors available for infection by sensitive strain
                # if (Nv > 0){fV <- (last$VSt + last$VSf) / Nv} else {fV <- 0}
                # 
                # #fraction of wildlife available for infection by sensitive strain
                # if (Nw > 0) {fW = last$WS / Nw} else {fW <- 0}
                
                ## R calculations
                Rsen_and_Rres <- calculate_R_from_row_of_df(params, last)
                Rres <- Rsen_and_Rres[[2]]
                # Nc = last$CS
                # Np = last$PS 
                # Nw = last$WS
                # Nv = last$VSt + last$VSf
                # sen <- "yes"
                # basic <- "no"
                # Rsen <- r0_calc_sen_or_res(params, Nc, Np, Nw, Nv, sen, basic)
                # sen <- "no"
                # Np = last$PS + last$PF
                # Rres <- r0_calc_sen_or_res(params, Nc, Np, Nw, Nv, sen, basic)
                
                
                #Rsen <- fC * R0sen[1] * fV * R0sen[3] + fW * R0sen[2] * fV * R0sen[4]  #Hurrah
                #Rres <- fC * R0res[1] * fV * R0res[3] + fW * R0res[2] * fV * R0res[4]
                
                #pull calculations from df below to here
                #be mindful of prophylactic treatment that they can be re-treated
                #2 stage run - pull calcs out, check for quick trt, expand to prophylactic next
                
                if(this_scenario$treatment_type == "F"){
                  No_trt_cat <-  params["treatment.q"] * last$CIs * 365.25
                  Inc <- params["gamma.c"] * last$CEs * 365.25
                  Prob_onward_transmission <- 1 - dpois(0, Rres[1])
                  RiskA <- ( last$CTs + last$PTs)
                  RiskE <- (1 - dpois(0, Rres[1])) * ( last$CTs + last$PTs)                  
                }
                
                if(this_scenario$treatment_type == "P"){
                  No_trt_cat <-  params["treatment.p"] * (last$PIs + last$CIs) * 365.25
                  Inc <- params["gamma.c"] * (last$PEs + last$CEs) * 365.25
                  Prob_onward_transmission <- 1 - dpois(0, Rres[1])
                  RiskA <- (last$PEs + last$PIs + last$PPs)
                  RiskE <- (1 - dpois(0, Rres[1])) * (last$PEs + last$PIs + last$PPs)
                }
                
                if(this_scenario$treatment_type == "B"){
                  No_trt_cat <-  (params["treatment.p"]+ params["treatment.q"]) * (last$PIs + last$CIs) * 365.25
                  Inc <- params["gamma.c"] * (last$PEs + last$CEs) * 365.25 
                  Prob_onward_transmission <- 1 - dpois(0, Rres[1])
                  RiskA <-  (last$PEs + last$PIs + last$PPs + last$CTs + last$PTs) 
                  RiskE <- (1 - dpois(0, Rres[1])) * (last$PEs + last$PIs + last$PPs + last$CTs + last$PTs)
                }
                
                prev <- (last$PIs + last$CIs) / last$All.cows
                
                selected_outputs <- cbind( data.frame(scenario_id = row, this_scenario, W_st = out[1, "WS"], 
                                               No_trt_cat = No_trt_cat, 
                                               Incidence = Inc, 
                                               Vector_no = as.numeric(inits["VSt"]), 
                                               Prob_onward_tran = Prob_onward_transmission, 
                                               RiskA = RiskA , RiskE = RiskE,
                                               prevalence = prev, vector_birth = params["birth.v"], 
                                               vector_mortality = params["death.v"], 
                                               eq_pop = params["equil_vector_pop"]))
                df = rbind(df, selected_outputs)
                
                wide <- cbind(selected_outputs, last)
                df2 = rbind(df2, wide)
                
                print(last$times)
                
}  

toc()

test <- df2
time <- format(Sys.time(), "%a %b %d %X %Y")
save(test, file = paste0("output/test_", this_scenario$treatment_type, "_play", ".Rda"))
#save(test,file ="output/test.Rda")

#quick_plot(expanded_output)
#quick_plot2(expanded_output)
quick_plot3(expanded_output)
#R0_plot(expanded_output)

df2 %>% filter(R0sen < 5) %>% ggplot() + geom_point(aes(y = Rsen, x = R0sen))

