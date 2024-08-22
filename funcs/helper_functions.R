
# =========================================================
# Function Names: get_full_path, get_filename, merge_params_into_this_scenario, append_descriptor,
#                 merge_dfs_without_duplicate_columns, move_populations_first, convert_array_to_named_vector,
#                 convert_df_row_to_named_vector, convert_named_vector_to_wide_df, convert_named_vector_to_long_df,
#                 include_full_scenario, append_suffix_to_column_names, my_rootfun, get_formatted_time,
#                 get_latest_Rda_file, get_disease_free_equilibrium_for_PF_PS_and_CS
# Description: This script contains utility functions for file and data management in R. Functions include generating
#              full file paths, filenames based on user settings, merging parameters into data scenarios, handling
#              data frames to ensure no duplicate columns when merging, and other specific utilities for managing
#              complex data structures in epidemiological analysis.
#
# Parameters:
#   df - A dataframe to be manipulated or merged.
#   params - Named vectors or lists that contain parameters to be merged into data frames.
#   user_inputs, descriptor - Inputs and descriptors used to manage filenames and paths dynamically.
#
# Returns:
#   Depending on the function, returns full file paths, filenames, or modified data frames with merged parameters or appended descriptors.
#
# Example of use:
#   user_inputs <- list(folder="data/", general_descriptor="experiment1", current_descriptor="run1")
#   full_path <- get_full_path()
#   filename <- get_filename()
#   df <- read.csv("path/to/data.csv")
#   params <- c(param1 = "value1", param2 = "value2")
#   updated_df <- merge_params_into_this_scenario(df, params)
#   final_df <- append_descriptor(updated_df, "new_descriptor")
#
# Dependencies: Requires 'codetools', 'lubridate', and 'dplyr' packages.
#
# Author: Shaun Keegan & Louise Matthews
# Date Created: August 2024
# Last Modified: August 2024
# =========================================================

library(codetools)
library(lubridate)
library(dplyr)

#-------------------------------------------------------------------------------
# Function Name: get_full_path
#
# Parameters:
#   None - Uses user_inputs() to fetch required information.
#
# Outputs:
#   full_path - A string representing the full path constructed from user inputs.
#
# Dependencies:
#   None explicitly required.
#
#-------------------------------------------------------------------------------
get_full_path <- function() {
  user_inputs <- get_user_inputs()
  full_path <- paste0(user_inputs$folder, user_inputs$general_descriptor, user_inputs$current_descriptor)
  full_path
}

#-------------------------------------------------------------------------------
# Function Name: get_filename
#
# Parameters:
#   None - Uses user_inputs() and get_full_path() to fetch and compute required information.
#
# Outputs:
#   filename - A string representing the constructed filename, potentially with a timestamp.
#
# Dependencies:
#   get_full_path(), get_formatted_time()
#
#-------------------------------------------------------------------------------
get_filename <- function() {
  user_inputs <- get_user_inputs()
  full_path <- get_full_path()
  if (user_inputs$append_current_time_to_output_file == TRUE) {
    current_time <- get_formatted_time()
    filename <- paste0(full_path, "_", current_time, ".Rda")
  } else {
    filename <- paste0(full_path, ".Rda")
  }
  filename
}

#-------------------------------------------------------------------------------
# Function Name: merge_params_into_this_scenario
#
# Parameters:
#   df - Dataframe to which parameters are to be merged.
#   params - Named vector of parameters to be merged.
#
# Outputs:
#   merged_df - Dataframe after merging the parameters without duplicating columns.
#
# Dependencies:
#   dplyr, convert_named_vector_to_wide_df()
#
#-------------------------------------------------------------------------------
merge_params_into_this_scenario <- function(df, params) {
  params_df <- convert_named_vector_to_wide_df(params)
  merged_df <- merge_dfs_without_duplicate_columns(df, params_df)
  merged_df
}

#-------------------------------------------------------------------------------
# Function Name: append_descriptor
#
# Parameters:
#   df - Dataframe to be manipulated.
#   descriptor - String or factor to append as a new column.
#
# Outputs:
#   df - Modified dataframe with a new column for the descriptor.
#
# Dependencies:
#   dplyr
#
#-------------------------------------------------------------------------------
append_descriptor <- function(df, descriptor) {
  df <- df %>% mutate(descriptor = descriptor)
  df
}

#-------------------------------------------------------------------------------
# Function Name: merge_dfs_without_duplicate_columns
#
# Parameters:
#   df1, df2 - Dataframes to be merged.
#
# Outputs:
#   merged_df - Resulting dataframe after merging df1 and df2 without duplicate columns.
#
# Dependencies:
#   dplyr
#
#-------------------------------------------------------------------------------
merge_dfs_without_duplicate_columns <- function(df1, df2) {
  duplicated_names <- names(df2)[(names(df2) %in% names(df1))]
  df2_reduced <- df2 %>% select(-all_of(duplicated_names))
  merged_df <- merge(df1, df2_reduced)
  merged_df
}

#-------------------------------------------------------------------------------
# Function Name: move_populations_first
#
# Parameters:
#   df - Dataframe with population data.
#
# Outputs:
#   df - Dataframe with reordered columns prioritizing population data.
#
# Dependencies:
#   dplyr
#
#-------------------------------------------------------------------------------
move_populations_first <- function(df) {
  df <- df %>% select(NC, CS, PF, PS, NW, NV, everything())
  df
}

#-------------------------------------------------------------------------------
# Function Name: convert_array_to_named_vector
#
# Parameters:
#   this_array - Array to be converted.
#
# Outputs:
#   this_vector - Named vector converted from the array.
#
# Dependencies:
#   None explicitly required.
#
#-------------------------------------------------------------------------------
convert_array_to_named_vector <- function(this_array) {
  names <- colnames((this_array))
  this_vector <- as.vector(this_array)
  names(this_vector) <- names
  this_vector
}

#-------------------------------------------------------------------------------
# Function Name: convert_df_row_to_named_vector
#
# Parameters:
#   df_row - Single row from a dataframe.
#
# Outputs:
#   named_vector - Named vector converted from the dataframe row.
#
# Dependencies:
#   None explicitly required.
#
#-------------------------------------------------------------------------------
convert_df_row_to_named_vector <- function(df_row) {
  named_vector <- unlist(df_row)
  named_vector
}

#-------------------------------------------------------------------------------
# Function Name: convert_named_vector_to_wide_df
#
# Parameters:
#   named_vector - Named vector to be converted to a dataframe.
#
# Outputs:
#   df - Dataframe created from the named vector.
#
# Dependencies:
#   None explicitly required.
#
#-------------------------------------------------------------------------------
convert_named_vector_to_wide_df <- function(named_vector) {
  df <- data.frame(as.list(named_vector))
  df
}

#-------------------------------------------------------------------------------
# Function Name: convert_named_vector_to_long_df
#
# Parameters:
#   named_vector - Named vector to be converted to a long format dataframe.
#
# Outputs:
#   df - Long format dataframe created from the named vector.
#
# Dependencies:
#   None explicitly required.
#
#-------------------------------------------------------------------------------
convert_named_vector_to_long_df <- function(named_vector) {
  df <- data.frame(value = as.numeric(named_vector), name = names(named_vector))
  df
}

#-------------------------------------------------------------------------------
# Function Name: include_full_scenario
#
# Parameters:
#   params - Parameters to include in the scenario.
#   df - Dataframe to which the parameters are to be added.
#
# Outputs:
#   df_with_params - Combined dataframe including both parameters and original data.
#
# Dependencies:
#   convert_named_vector_to_wide_df()
#
#-------------------------------------------------------------------------------
include_full_scenario <- function(params, df) {
  params_df <- convert_named_vector_to_wide_df(params)
  df_with_params <- cbind(params_df, df)
  df_with_params
}

#-------------------------------------------------------------------------------
# Function Name: append_suffix_to_column_names
#
# Parameters:
#   df - Dataframe whose column names need suffixes appended.
#   suffix - Suffix to append to each column name.
#
# Outputs:
#   df - Modified dataframe with updated column names.
#
# Dependencies:
#   dplyr
#
#-------------------------------------------------------------------------------
append_suffix_to_column_names <- function(df, suffix) {
  df <- df %>% rename_with(~ paste0(., suffix))
  df
}

#-------------------------------------------------------------------------------
# Function Name: my_rootfun
#
# Parameters:
#   t - Time parameter for the function.
#   y - Current values of the variables being modeled.
#   params - Model parameters.
#
# Outputs:
#   Computed conditions based on model dynamics.
#
# Dependencies:
#   R_calc_sen_or_res()
#
#-------------------------------------------------------------------------------
my_rootfun <- function(t, y, params) {
  Nc <- y["CS"]
  Npf <- y["PF"]
  Nps <- y["PS"]
  Nw <- y["WS"]
  Nv <- y["VSt"] + y["VSf"]
  Rsen <- R_calc_sen_or_res(params, Nc, Npf, Nps, Nw, Nv, is_strain_sensitive = "yes", basic = "no")
  
  condition1 <- (Rsen - 1.01)
  condition2 <- (y["CIs"] - 1e-5)
  
  return(c(condition1, condition2))
}

#-------------------------------------------------------------------------------
# Function Name: get_formatted_time
#
# Parameters:
#   None - Uses system time.
#
# Outputs:
#   current_time_without_colons - Formatted current time without colons.
#
# Dependencies:
#   None explicitly required.
#
#-------------------------------------------------------------------------------
get_formatted_time <- function() {
  current_time <- format_ISO8601(Sys.time(), usetz = FALSE, precision = NULL)
  current_time_without_colons <- gsub(":", "", current_time)
  current_time_without_colons
}

#-------------------------------------------------------------------------------
# Function Name: get_latest_Rda_file
#
# Parameters:
#   None - Searches in a specified directory.
#
# Outputs:
#   latest_file - The name of the most recently created .Rda file.
#
# Dependencies:
#   None explicitly required.
#
#-------------------------------------------------------------------------------
get_latest_Rda_file <- function() {
  datafiles <- list.files("output", pattern = ".Rda", full.names = TRUE)
  info <- file.info(datafiles)
  most_recent_creation_time := max(info$ctime)
  latest_file := rownames(info[info$ctime == most_recent_creation_time, ])
  latest_file
}

#-------------------------------------------------------------------------------
# Function Name: get_disease_free_equilibrium_for_PF_PS_and_CS
#
# Parameters:
#   birth_c - Birth rate for cattle.
#   prop_prophylaxis_at_birth - Proportion of prophylaxis at birth.
#   NC - Total number of cattle.
#   death_c - Death rate for cattle.
#   waning_f2s - Waning rate from full to susceptible.
#   death_p - Death rate for prophylactically treated cattle.
#   waning - Waning rate for immunity.
#   proph_ongoing - Ongoing prophylaxis rate.
#
# Outputs:
#   answer - Vector of disease-free equilibrium values for different cattle populations.
#
# Dependencies:
#   None explicitly required.
#
#-------------------------------------------------------------------------------
get_disease_free_equilibrium_for_PF_PS_and_CS <- function(birth_c, prop_prophylaxis_at_birth, NC, death_c,
                                                          waning_f2s, death_p, waning, proph_ongoing) {
  # get matrix from odes for PF, PS and CS in absence of disease
  M := matrix(c(
    death_p + waning_f2s, -proph_ongoing, -proph_ongoing,
    waning_f2s, -(waning + death_p + proph_ongoing), 0,
    0, -waning, death_c + proph_ongoing
  ), byrow = TRUE, ncol = 3)
  
  this_vec := c(
    birth_c * (prop_prophylaxis_at_birth) * NC,
    0,
    birth_c * (1 - prop_prophylaxis_at_birth) * NC
  )
  
  # solve linear system of equations by inverting matrix and multiplying rhs
  answer := solve(M) %*% this_vec
  names(answer) := c("PF", "PS", "CS")
  answer
}


findGlobals(fun = get_disease_free_equilibrium_for_PF_PS_and_CS, merge = FALSE)$variables
findGlobals(fun = get_filename, merge = FALSE)$variables
findGlobals(fun = get_full_path, merge = FALSE)$variables
findGlobals(fun = get_latest_Rda_file, merge = FALSE)$variables
findGlobals(fun = get_formatted_time, merge = FALSE)$variables
findGlobals(fun = my_rootfun, merge = FALSE)$variables
findGlobals(fun = append_suffix_to_column_names, merge = FALSE)$variables
findGlobals(fun = include_full_scenario, merge = FALSE)$variables
findGlobals(fun = convert_named_vector_to_long_df, merge = FALSE)$variables
findGlobals(fun = convert_named_vector_to_wide_df, merge = FALSE)$variables
findGlobals(fun = convert_array_to_named_vector, merge = FALSE)$variables
findGlobals(fun = merge_dfs_without_duplicate_columns, merge = FALSE)$variables
findGlobals(fun = append_descriptor, merge = FALSE)$variables
findGlobals(fun = merge_params_into_this_scenario, merge = FALSE)$variables
findGlobals(fun = move_populations_first, merge = FALSE)$variables
