library(ggplot2)
library(dplyr)
library(patchwork)
library(tictoc)

source("funcs/helper_functions.R")
source("funcs/epi_outputs.R")
source("funcs/plot_helper.R")
source("funcs/plot_settings.R")
source("15_plots_for_SALT_TZ_helper.R")
source("funcs/plot_settings.R")

# Cost analysis scripts
source("funcs/set_params.R")
source("Cost_analysis.v2.r")
source("fast_sim_helper.R")
source("fertility_update.r")
source("run_fertility_model_core_function.R")


# Useful function --------------------------------------------------------------
get_filename2 <- function(this_path, Rda_file){
  filename_temp <- gsub(".Rda", "", Rda_file)
  filename <- gsub(this_path, "", filename_temp)
  filename
}

get_latest_Rda_file2 <- function(this_path = "output/") {
  output_folder <- gsub("/", "", this_path)
  datafiles <- list.files(output_folder, pattern = ".Rda", full.names = TRUE)
  info <- file.info(datafiles)
  most_recent_creation_time <- max(info$ctime)
  latest_file <- rownames(info[info$ctime == most_recent_creation_time, ])
  latest_file
}

# Load data files --------------------------------------------------------------
this_path <- "output/"
load_latest_file <- TRUE
if (load_latest_file == TRUE) {
  Rda_file <- get_latest_Rda_file2(this_path)
  filename <- get_filename2(this_path, Rda_file)
} else {
  filename <- Jan_mortality_fix_and_protection_80_combined_1
  Rda_file <- paste0(this_path, filename, ".Rda")
}
print(filename)
file_for_analysis <- Rda_file
print(file_for_analysis)

load(file_for_analysis)
folder_name <- gsub(".Rda", "/", file_for_analysis)
dir.create(folder_name)

# Inspect contents --------------------------------------------------------------
nrow(saved_simulations)
table(saved_simulations$prop_cattle_with_insecticide)
table(saved_simulations$K_host_ratio)
table(saved_simulations$maintain_vector_pop)

# temporary fix ----------------------------------------------------------------
# temp fix until code is rerun with fixed append_epi_outputs_to_df()
corrected_data <- append_epi_outputs_to_df(saved_simulations)
corrected_data <- corrected_data %>% 
  mutate(prevalence = prevalence_new, Incidence = Incidence_new) %>% 
  rename(CEsX_final = CEXs_final, CErX_final = CEXr_final)

data_for_analysis <- corrected_data
table(data_for_analysis$treatment_code)
data_for_analysis <- data_for_analysis %>% filter(treatments_per_year < 6.5)
# end temporary fix ------------------------------------------------------------

# subset and rejoin ------------------------------------------------------------
subsets <- list()
for (ttype in 1:3){
  #subset <- create_data_subsets(data_for_analysis, ttype)
  subset <- data_for_analysis %>% filter(treatment_code == ttype)
  print(nrow(subset))
  #subset$treatment_code <- ttype
  subsets[[ttype]] <- subset
}
all_data_no_cost_analysis <- data.frame(rbind(subsets[[1]], subsets[[2]], subsets[[3]]))
all_data_no_cost_analysis <- saved_simulations
# end subset and rejoin --------------------------------------------------------

###############################################################################
# add on cost analyses ---------------------------------------------------------
nrow(subsets[[1]])
nrow(subsets[[2]])
nrow(subsets[[3]])
for (option in 1:3){
  system <- "Agro-pastoral" #"Agro-pastoral" or "Dairy"
  this_subset <- subsets[[option]]
  #this_subset <- head(this_subset, 20)
  print(nrow(this_subset))
  tic()
  this_cost_df <- economic_analysis(this_subset, system, option) # System can be "Dairy" or "Agro-pastoral"
  toc()
  if (option == 1){cost_df1 <- this_cost_df}
  if (option == 2){cost_df2 <- this_cost_df}
  if (option == 3){cost_df3 <- this_cost_df}
}
nrow(subsets[[1]])
nrow(subsets[[2]])
nrow(subsets[[3]])
nrow(cost_df1)
nrow(cost_df2)
nrow(cost_df3)

all_data_with_cost_analysis <- rbind(cost_df1, cost_df2, cost_df3)

###############################################################################
# save dataframe with cost analyses --------------------------------------------
this_path_for_economics <- paste0(this_path, "output_with_economics/")
cost_benefit_filename <- paste0(this_path_for_economics, filename, "_eco", ".Rda")
save(all_data_with_cost_analysis, file = cost_benefit_filename)
###############################################################################
