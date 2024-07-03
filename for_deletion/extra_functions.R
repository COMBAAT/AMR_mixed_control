
add_columns_for_totals_and_Rvalues <-function(out){
  expanded_output <- add_totals(out)
  expanded_output <- add_R0(inits, expanded_output)
  expanded_output <- add_R_trajectories(params, expanded_output)
  expanded_output
}

create_summary_from_final_state_timepoint_of_simulation <- function(expanded_output){
  final_state <- tail(expanded_output, 1)
  
}

