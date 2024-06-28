## --------------------- Quality Check - qual_check
##
##
##
##





## ------------------------------------------------------ Check Inputs

library(crayon)
library(dplyr) #added by LM


qual_check_no0 <- function(input){
  
  test <- c(1:length(input))
  test[] <- NA
  
  for (i in 1: length(input)){
    if(input[i] < 0){cat(red("WARNING: ", names(input[i]), "  LESS THAN 0\n"))}
  }
  
  for(j in 1:length(input)){
    if (input[j] >= 0){test[j] <- 1}
  }

    if(sum(test[!is.na(test)]) == length(input)){cat(green("CHECK OK\n"))}
  
}

## ------------------------------------------------------ Add total cols to out

add_totals <- function(df){
  
  df_new <- df %>% mutate(Cattle_total = rowSums(select(., starts_with("C"))),
                                   Prophylactic_total = rowSums(select(., starts_with("P"))),
                                   Vector_total = rowSums(select(., starts_with("V"))),
                                   Wildlife_total = rowSums(select(., starts_with("W"))),
                                   All.cows = Cattle_total + Prophylactic_total)
  
  return(df_new)
  
}
