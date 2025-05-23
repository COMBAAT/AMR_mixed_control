#################################################################################

#-------------------------------------------------------------------------------
# Function Name: calculate_R0
#
# Description:
#   This function calculates the basic reproduction number (R0) for both sensitive and resistant
#   strains of a disease within a host-vector population model. It utilizes the `R_calc_sen_or_res`
#   function to compute R0 values based on the provided epidemiological parameters.
#
# Parameters:
#   params - A named vector or list containing the model parameters, including:
#            - PF: Number of fully protected prophylactic cattle.
#            - PS: Number of partially protected prophylactic cattle.
#            - CS: Number of cattle susceptible to infection.
#            - NW: Number of wildlife susceptible to infection.
#            - NV: Number of vectors capable of transmitting the infection.
#
# Returns:
#   A named numeric vector with two elements:
#   - R0sen: The basic reproduction number for the sensitive strain.
#   - R0res: The basic reproduction number for the resistant strain.
#
# Example of use:
#   params <- c(PF = 500, PS = 300, CS = 1000, NW = 200, NV = 100, ...)
#   R0_values <- calculate_R0(params)
#   print(R0_values)
#
# Dependencies:
#   This function relies on the `R_calc_sen_or_res` function to compute the R0 values for both
#   sensitive and resistant strains. The `params` vector or list must contain all necessary
#   parameters with the correct names.
#
#-------------------------------------------------------------------------------


calculate_R0 <- function(params) {
  Npf <- params["PF"]
  Nps <- params["PS"]
  Nc <- params["CS"]
  Nw <- params["NW"]
  Nv <- params["NV"]
  R0sen <- R_calc_sen_or_res(params, Nc, Npf, Nps, Nw, Nv, is_strain_sensitive = "yes", basic = "yes")
  R0res <- R_calc_sen_or_res(params, Nc, Npf, Nps, Nw, Nv, is_strain_sensitive = "no", basic = "yes")
  R0sen2 <- R_calc_sen_or_res2(params, Nc, Npf, Nps, Nw, Nv, is_strain_sensitive = "yes", basic = "yes")
  R0res2 <- R_calc_sen_or_res2(params, Nc, Npf, Nps, Nw, Nv, is_strain_sensitive = "no", basic = "yes")
  
  c("R0sen" = R0sen, "R0res" = R0res, "R0sen2" = R0sen2, "R0res2" = R0res2)
}

#-------------------------------------------------------------------------------
# Function Name: calculate_R_from_row_of_df
#
# Description:
#   This function calculates the reproduction number (R) for both sensitive and resistant strains
#   of a disease based on a single row of data from a dataframe. The function extracts the relevant
#   population counts from the provided row and uses the `R_calc_sen_or_res` function to compute
#   the R values.
#
# Parameters:
#   params - A named vector or list containing the model parameters necessary for calculating the
#            reproduction number (R).
#   this_row - A single row from a dataframe containing the population counts for:
#              - CS: Number of cattle susceptible to infection.
#              - PS: Number of partially protected prophylactic cattle.
#              - PF: Number of fully protected prophylactic cattle.
#              - WS: Number of wildlife susceptible to infection.
#              - VSt: Number of susceptible vectors.
#              - VSf: Number of fully protected vectors.
#
# Returns:
#   A named numeric vector with two elements:
#   - Rsen: The reproduction number for the sensitive strain.
#   - Rres: The reproduction number for the resistant strain.
#
# Example of use:
#   df <- read.csv("path/to/data.csv")
#   this_row <- df[1, ]
#   R_values <- calculate_R_from_row_of_df(params, this_row)
#   print(R_values)
#
# Dependencies:
#   This function relies on the `R_calc_sen_or_res` function to compute the R values for both
#   sensitive and resistant strains. The `this_row` dataframe row must contain all necessary
#   population counts with the correct column names.
#
#-------------------------------------------------------------------------------

calculate_R_from_row_of_df <- function(params, this_row) {
  Nc <- this_row$CS
  Nps <- this_row$PS
  Npf <- this_row$PF
  Nw <- this_row$WS
  Nv <- this_row$VSt + this_row$VSf
  Rsen <- R_calc_sen_or_res(params, Nc, Npf, Nps, Nw, Nv, is_strain_sensitive = "yes", basic = "no")
  Rres <- R_calc_sen_or_res(params, Nc, Npf, Nps, Nw, Nv, is_strain_sensitive = "no", basic = "no")
  Rsen2 <- R_calc_sen_or_res2(params, Nc, Npf, Nps, Nw, Nv, is_strain_sensitive = "yes", basic = "no")
  Rres2 <- R_calc_sen_or_res2(params, Nc, Npf, Nps, Nw, Nv, is_strain_sensitive = "no", basic = "no")
  c("Rsen" = Rsen, "Rres" = Rres, "Rsen2" = Rsen2, "Rres2" = Rres2)
}

#-------------------------------------------------------------------------------
# Function Name: add_R_trajectories
#
# Description:
#   This function computes the reproduction number (R) trajectories for both sensitive and resistant
#   strains across all rows of a dataframe. It iterates through each row of the dataframe, calculates
#   the R values using the `calculate_R_from_row_of_df` function, and appends these values as new columns
#   (Rsen and Rres) to the dataframe.
#
# Parameters:
#   params - A named vector or list containing the model parameters necessary for calculating the
#            reproduction number (R).
#   df - A dataframe containing time series data for different population groups and their respective
#        stages of disease progression. The dataframe must include columns for:
#        - CS: Number of cattle susceptible to infection.
#        - PS: Number of partially protected prophylactic cattle.
#        - PF: Number of fully protected prophylactic cattle.
#        - WS: Number of wildlife susceptible to infection.
#        - VSt: Number of susceptible vectors.
#        - VSf: Number of fully protected vectors.
#
# Returns:
#   The input dataframe `df`, with two additional columns:
#   - Rsen: The reproduction number trajectory for the sensitive strain.
#   - Rres: The reproduction number trajectory for the resistant strain.
#
# Example of use:
#   df <- read.csv("path/to/data.csv")
#   df_with_R <- add_R_trajectories(params, df)
#   head(df_with_R)
#
# Dependencies:
#   This function relies on the `calculate_R_from_row_of_df` function to compute the R values for each
#   row in the dataframe. The input dataframe `df` must contain all necessary population counts with
#   the correct column names.
#
#-------------------------------------------------------------------------------

add_R_trajectories <- function(params, df) {
  Rsen_vec <- c()
  Rres_vec <- c()
  Rsen2_vec <- c()
  Rres2_vec <- c()
  for (i in 1:nrow(df)) {
    this_row <- df[i, ]
    Rsen_and_Rres <- calculate_R_from_row_of_df(params, this_row)
    Rsen <- Rsen_and_Rres["Rsen"]
    Rres <- Rsen_and_Rres["Rres"]
    Rsen2 <- Rsen_and_Rres["Rsen2"]
    Rres2 <- Rsen_and_Rres["Rres2"]
    Rsen_vec <- c(Rsen_vec, Rsen)
    Rres_vec <- c(Rres_vec, Rres)
    Rsen2_vec <- c(Rsen2_vec, Rsen2)
    Rres2_vec <- c(Rres2_vec, Rres2)
  }
  
  df$Rsen <- Rsen_vec
  df$Rsen2 <- Rsen2_vec
  df$Rres <- Rres_vec
  df$Rres2 <- Rres2_vec
  df
}

#-------------------------------------------------------------------------------
# Function Name: add_R0
#
# Description:
#   This function calculates the basic reproduction number (R0) for both sensitive and resistant
#   strains of a disease using the `calculate_R0` function. It then appends these R0 values as
#   new columns (`R0sen` and `R0res`) to the provided dataframe.
#
# Parameters:
#   params - A named vector or list containing the model parameters necessary for calculating the
#            basic reproduction number (R0).
#   df - A dataframe to which the calculated R0 values will be added as new columns.
#
# Returns:
#   The input dataframe `df`, with two additional columns:
#   - R0sen: The basic reproduction number for the sensitive strain.
#   - R0res: The basic reproduction number for the resistant strain.
#
# Example of use:
#   df <- read.csv("path/to/data.csv")
#   df_with_R0 <- add_R0(params, df)
#   head(df_with_R0)
#
# Dependencies:
#   This function relies on the `calculate_R0` function to compute the R0 values. The `params` vector
#   or list must contain all necessary parameters with the correct names.
#
#-------------------------------------------------------------------------------


add_R0 <- function(params, df) {
  R0sen_and_R0res <- calculate_R0(params)
  R0sen <- R0sen_and_R0res["R0sen"]
  R0res <- R0sen_and_R0res["R0res"]
  R0sen2 <- R0sen_and_R0res["R0sen2"]
  R0res2 <- R0sen_and_R0res["R0res2"]
  df$R0sen <- R0sen
  df$R0sen2 <- R0sen2
  df$R0res <- R0res
  df$R0res2 <- R0res2
  df
}


findGlobals(fun = R_calc_sen_or_res, merge = FALSE)$variables
findGlobals(fun = calculate_R0, merge = FALSE)$variables
findGlobals(fun = add_R0, merge = FALSE)$variables
# findGlobals(fun = add_R0, merge = FALSE)$variables
findGlobals(fun = calculate_R_from_row_of_df, merge = FALSE)$variables
findGlobals(fun = add_R_trajectories, merge = FALSE)$variables
findGlobals(fun = create_named_vector_of_transition_probabilities, merge = FALSE)$variables
findGlobals(fun = create_named_vector_of_time_in_state, merge = FALSE)$variables