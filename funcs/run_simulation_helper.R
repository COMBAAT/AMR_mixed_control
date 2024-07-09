
merge_params_into_this_scenario <- function(df, params) {
  params_df <- convert_named_vector_to_df(params)
  merged_df <- merge_dfs_without_duplicate_columns(df, params_df)
  merged_df
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


convert_named_vector_to_df <- function(named_vec){
  df = data.frame(as.list(named_vec)) 
  df
}

include_full_scenario <- function(params, df) {
  params_df <- convert_named_vector_to_df(params)
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
  Rsen <- r0_calc_sen_or_res(params, Nc, Np, Nw, Nv, is_strain_sensitive = "yes", basic = "no")
  
  condition1 <- (Rsen - 1.01)
  condition2 <- (y['CIs'] - 1e-5)
  
  return(c(condition1, condition2))
}

