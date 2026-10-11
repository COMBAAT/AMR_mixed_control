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

library(future)
library(furrr)
library(progressr)
handlers(global = TRUE)


## ------------------------------------------------------ LOAD FUNCTIONS

#source("1_set_user_inputs.R")
source("funcs/set_params.R")
source("funcs/set_inits.R")
source("funcs/qual_check.R")
source("funcs/helper_functions.R")
source("funcs/epi_outputs.R")
source("funcs/quick_plot.R")
source("funcs/output_baseline_params_and_scenarios.R")
source("funcs/AAT_AMR_dens_dep.R")
source("funcs/r0_intuitive.R")
source("funcs/r0_NGM.R")
source("funcs/r0_helper.R")
source("1_set_user_inputs.R")

source("run_helper.R")




## ----
# View inputs
user_inputs <- get_user_inputs()
if (user_inputs$multiple_scenarios == TRUE) {
  scenarios_df <- create_multiple_scenarios_new()
} else {
  scenarios_df <- create_single_scenario_new()
}
baseline_parameters <- get_baseline_parameters()
#plot_baseline_parameters(baseline_parameters)
#plot_scenarios(scenarios_df)

# Create empty dataframe to store outputs
#all_simulations_summary <- data.frame()

## ---- Start run time estimates
tic()

## ---- Execute model
number_of_scenarios <- nrow(scenarios_df)
number_of_scenarios



## ---- Parallel setup
plan(multisession, workers = max(1, parallelly::availableCores() - 4))
options(future.globals.maxSize = 50 * 1024^2)   # error if anything large gets exported

## ---- Run in chunks, saving each one
tic("everything")

chunk_size <- 5000
chunks <- split(1:number_of_scenarios, ceiling(seq_len(number_of_scenarios) / chunk_size))
chunk_dir <- paste0(get_full_path(), "_chunks")   # one folder per run
dir.create(chunk_dir, showWarnings = FALSE, recursive = TRUE)

tic("total")
for (i in seq_along(chunks)) {
  chunk_file <- file.path(chunk_dir, sprintf("chunk_%03d.rds", i))
  if (file.exists(chunk_file)) next
  
  tic(sprintf("chunk %d of %d", i, length(chunks)))
  chunk_df <- scenarios_df[chunks[[i]], ]
  res <- future_map(
    seq_len(nrow(chunk_df)),
    function(j) run_one_scenario(j, chunk_df, user_inputs),
    .options = furrr_options(scheduling = 20, stdout = FALSE)
  )
  saveRDS(data.table::rbindlist(res), chunk_file)
  toc()            # e.g. "chunk 51 of 270: 78.4 sec elapsed"
}
toc()              # "total: ... sec elapsed"

results_df <- as.data.frame(data.table::rbindlist(
  lapply(sort(list.files(chunk_dir, pattern = "\\.rds$", full.names = TRUE)), readRDS)
  #lapply(sort(list.files(chunk_dir, full.names = TRUE)), readRDS)
))
toc()
plan(sequential)   # shut the workers down



# results <- vector("list", number_of_scenarios)
# for (row in 1:number_of_scenarios) {
#   message("running scenario ", row, " of ", number_of_scenarios)
#   results[[row]] <- run_one_scenario(row, scenarios_df, user_inputs)
# }
# results_df <- as.data.frame(data.table::rbindlist(results))
# toc()



# plan(sequential)
# plan(multisession, workers = max(1, parallelly::availableCores() - 1))
# 
# tic()
# with_progress({
#   p <- progressor(steps = number_of_scenarios)
#   
#   results2 <- future_map(
#     1:number_of_scenarios,
#     function(row) {
#       p()
#       run_one_scenario(row, scenarios_df, user_inputs, return_trajectory = FALSE)
#     },
#     .options = furrr_options(scheduling = 10)
#   )
# })
# results_df <- as.data.frame(data.table::rbindlist(results2))
# toc()

# plan(sequential)
# plan(multisession, workers = 4)
# tic()
# with_progress({
#   p <- progressor(steps = number_of_scenarios)
#   
#   results2 <- future_map(
#     1:number_of_scenarios,
#     function(row) {
#       p(sprintf("scenario %d of %d", row, number_of_scenarios))
#       run_one_scenario(row, scenarios_df, user_inputs, return_trajectory = FALSE)
#     }
#   )
# })
# results_df <- as.data.frame(data.table::rbindlist(results2))
# toc()

all_simulations_summary <- results_df

# add columns indicating outcome of cometition with or invasion by resistant strains
all_simulations_summary <- add_competition_and_invasion_columns(all_simulations_summary)

#quick_plot(expanded_output)

df <- simplify_outputs(all_simulations_summary)
#glimpse(df)

# Outputs to an Rda file
#saved_simulations <- all_simulations_summary
saved_simulations <- all_simulations_summary
filename <- get_filename()
save(saved_simulations, baseline_parameters, scenarios_df, file = filename)

saved_simulations %>% filter(!(treat_prop == 0 & proph_ongoing == 0) ) %>%
  select(NW, treatment_type, treat_prop, proph_ongoing, prevalence, 
         Incidence, CS_final, PF_final, PS_final, 
         RiskA, CIs_final, PEs_final, PIs_final, PPs_final, PEsX_final, CEXs_final) %>% glimpse()

