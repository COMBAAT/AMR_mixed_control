
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
source("input_scenarios.R")

loops <- FALSE
scenarios_df <- input_scenarios(loops)

df <- data.frame()
df2 <- data.frame()

## ---- Run time estimates
tic()
start.time <- Sys.time()

## ---- Execute model

for (i in 1:nrow(scenarios_df)) {
  print(i)
  
                this_scenario <- scenarios_df[i, ]
                
                params_and_inits <- set1(this_scenario)
                params <- params_and_inits[["params"]]
                inits <- params_and_inits[["inits"]]
                
                qual_check_no0(params) # ensure there are no negative values
                qual_check_no0(inits) # ensure there are no negative values
                
                myvars <- names(params) %in% c("resusceptible", "resusceptible.w") 
                params <- params[!myvars]
                
                myvars2 <- names(inits) %in% c("CR", "PR", "WR") 
                inits <- inits[!myvars2]
                
                
                ## R0 calculations
                
                Nc = as.numeric(inits["CS"] + inits["CIs"])
                Np = as.numeric(inits["PS"])
                Nw = as.numeric(inits["WS"])
                Nv = as.numeric(inits["VSt"])
                sen <- "yes"
                basic <- "yes"
                R0s <- r0_calc_sen_or_res(params, Nc, Np, Nw, Nv, sen, basic)
                sen <- "no"
                R0r <- r0_calc_sen_or_res(params, Nc, Np, Nw, Nv, sen, basic)
                
                
                ## Times ----
                times <- seq(0, this_scenario$max_time, 1)
                
                ## RUN MODEL ----
                #out <-ode(y = inits, parms = params, func = AAT_AMR_dens_dep, times = times, method = "daspk",
                #          rootfunc = my_rootfun2,events = list(root = TRUE, terminalroot = c(1,2)))
                out <-ode(y = inits, parms = params, func = AAT_AMR_dens_dep, times = times, method = "daspk")
                out <- as.data.frame(out)
                names(out)[names(out) == 'time'] <- "times"
                
                expanded_output <- add_totals(out)
                last <- tail(expanded_output, 1)
                
                
                #Check get 1 at equilibrium when run with only sensitive strains
                #fraction of cattle available for infection by sensitive strain
                fC <- last$CS / Nc
                #fraction of vectors available for infection by sensitive strain
                if (Nv > 0){fV <- (last$VSt + last$VSf) / Nv} else {fV <- 0}
                
                #fraction of wildlife available for infection by sensitive strain
                if (Nw > 0) {fW = last$WS / Nw} else {fW <- 0}
                
                
                ## R calculations
                
                Nc = last$CS
                Np = last$PS 
                Nw = last$WS
                Nv = last$VSt + last$VSf
                sen <- "yes"
                basic <- "no"
                Rsen <- r0_calc_sen_or_res(params, Nc, Np, Nw, Nv, sen, basic)
                sen <- "no"
                Np = last$PS + last$PF
                Rres <- r0_calc_sen_or_res(params, Nc, Np, Nw, Nv, sen, basic)
                
                
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
                
                
                selected_outputs <- cbind( data.frame(W_st = out[1, "WS"], R_eq_sen = Rsen[1], R0_sen = R0s[1], 
                                               R_eq_res = Rres[1], R0_res = R0r[1], 
                                               No_trt_cat = No_trt_cat, 
                                               Incidence = Inc, 
                                               Vector_no = as.numeric(inits["VSt"]), 
                                               Prob_onward_tran = Prob_onward_transmission, 
                                               RiskA = RiskA , RiskE = RiskE,
                                               prevalence = prev, vector_birth = params["birth.v"], 
                                               vector_mortality = params["death.v"], 
                                               eq_pop = params["equil.vector_pop"],
                                               this_scenario))
                df = rbind(df, selected_outputs)
                
                wide <- cbind(selected_outputs, last)
                df2 = rbind(df2, wide)
                
                
                print(last$times)
 
}  

toc()

Rsen
Rres



test <- df2
time <- format(Sys.time(), "%a %b %d %X %Y")
#save(test, file = paste0("output/test_", this_scenario$treatment_type, "_play", ".Rda"))
#save(test,file ="output/test.Rda")

quick_plot(expanded_output)
quick_plot2(expanded_output)
quick_plot3(expanded_output)



