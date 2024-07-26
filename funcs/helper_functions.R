library(codetools)
library(lubridate)
library(dplyr)

get_full_path <- function() {
  user_inputs <- get_user_inputs()
  full_path <- paste0(user_inputs$folder, user_inputs$general_descriptor, user_inputs$current_descriptor)
  full_path
}


get_filename <- function() {
  user_inputs <- get_user_inputs()
  full_path  <- get_full_path()
  if (user_inputs$append_current_time_to_output_file == TRUE) {
    current_time <- get_formatted_time()
    filename <- paste0(full_path, "_", current_time, ".Rda")
  } else {
    filename <- paste0(full_path, ".Rda")
  }
  filename
}


merge_params_into_this_scenario <- function(df, params) {
  params_df <- convert_named_vector_to_wide_df(params)
  merged_df <- merge_dfs_without_duplicate_columns(df, params_df)
  merged_df
}

append_descriptor <- function(df, descriptor) {
  df <- df %>% mutate(descriptor = descriptor)
  df
}

merge_dfs_without_duplicate_columns <- function(df1, df2){
  duplicated_names <- names(df2)[(names(df2) %in% names(df1))]
  df2_reduced <- df2 %>% select(-all_of(duplicated_names))
  merged_df <- merge(df1, df2_reduced)
  merged_df
}

move_populations_first <- function(df){
  df <- df %>% select(NC, CS, PF, PS, NW, NV, everything())
  df
}

convert_array_to_named_vector <- function(this_array) {
  names <- colnames((this_array))
  this_vector <- as.vector(this_array)
  names(this_vector) <- names
  this_vector
}

convert_df_row_to_named_vector <- function(df_row) {
  named_vector <- unlist(df_row)
  named_vector
}

convert_named_vector_to_wide_df <- function(named_vector){
  df = data.frame(as.list(named_vector)) 
  df
}

convert_named_vector_to_long_df <- function(named_vector) {
  df <- data.frame(value = as.numeric(named_vector), name = names(named_vector))
  df
}

include_full_scenario <- function(params, df) {
  params_df <- convert_named_vector_to_wide_df(params)
  df_with_params <- cbind(params_df, df)
  df_with_params
}

append_suffix_to_column_names <- function(df, suffix) {
  df <- df %>% rename_with(~ paste0(., suffix))
}




my_rootfun <- function (t, y, params) {
  dstate <- unlist(AAT_AMR_dens_dep(t, y, params)) # rate of change vector
  Nc <- y['CS']
  Np <- y['PS']
  Nw <- y['WS']
  Nv <- y['VSt'] + y['VSf']
  Rsen <- R_calc_sen_or_res(params, Nc, Np, Nw, Nv, is_strain_sensitive = "yes", basic = "no")
  
  condition1 <- (Rsen - 1.01)
  condition2 <- (y['CIs'] - 1e-5)
  
  return(c(condition1, condition2))
}

get_formatted_time <- function() {
  current_time <- format_ISO8601(Sys.time(), usetz = FALSE, precision = NULL)
  current_time_without_colons <- gsub(":", "", current_time)
  current_time_without_colons
}


get_latest_Rda_file <- function() {
  datafiles <- list.files("output", pattern = ".Rda", full.names = TRUE)
  info <- file.info(datafiles) 
  most_recent_creation_time <- max(info$ctime)
  latest_file <- rownames(info[info$ctime == most_recent_creation_time,])
  latest_file
}


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



