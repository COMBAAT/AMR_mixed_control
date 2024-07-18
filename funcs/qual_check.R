library(crayon)
library(codetools)

## ------------------------------------------------------ Check Inputs


qual_check_no0 <- function(input){
  
  test <- c(1:length(input))
  test[] <- NA
  
  for (i in 1: length(input)){
    if(input[i] < 0){cat(red("WARNING: ", names(input[i]), "  LESS THAN 0\n"))}
  }
  
  for(j in 1:length(input)){
    if (input[j] >= 0){test[j] <- 1}
  }

    #if(sum(test[!is.na(test)]) == length(input)){cat(green("CHECK OK\n"))}
  
}

findGlobals(fun = qual_check_no0, merge = FALSE)$variables

