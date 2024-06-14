
input_scenarios <- function(loops){
  
  if (loops == TRUE) {
    max_time <- 10000
    treatment_type = "F"  #F this means quick treatment
    N_wl <- c(0, 100, 250)
    treat.prop.vecA <- seq(0, 0.9, by = 0.2)      #full from 0-1
    treat.prop.vecB <- seq(0.91, 0.99, by = 0.02)
    treat.prop.vec <- c(treat.prop.vecA, treat.prop.vecB)
    K.vec <- c(10000, 6000, 2000, 1000, 500)
    fit.adj.vec <- c(0.95)
    birth.adj.vec <- c(2)
    # do not set prop.insecticide to 1 as generates infinite mortality and an error
    #prop.insecticide.vec <- c(0.0, 0.05, 0.10, 0.15, 0.2)
    prop.insecticide.vec <- c(0.0, 0.025, 0.05, 0.10, 0.15, 0.2, 0.3, 0.4, 0.5)
    prop.prophylaxis <- 0.0
    dose.adj.vec <- 1#c(1, 0.9, 0.7, 0.4, 0.2)
    emergence.adj.vec <- 1
  } else {
    max_time <- 5000
    treatment_type = "F" 
    N_wl <- 250
    treat.prop.vec <- 0.25
    fit.adj.vec <- c(0.95)
    K.vec <- 2000
    prop.insecticide.vec <- 0.0
    birth.adj.vec <- 2
    prop.prophylaxis <- 0.0
    dose.adj.vec <- 1.0
    emergence.adj.vec <- 1
  }
  
  df <- expand.grid(emergence.adj = emergence.adj.vec, 
                    dose.adj = dose.adj.vec, 
                    treat_prop = treat.prop.vec, 
                    NW = N_wl,
                    K = K.vec, 
                    fit.adj = fit.adj.vec,
                    prop.insecticide = prop.insecticide.vec, 
                    birth.adj = birth.adj.vec, 
                    prop.prophylaxis = prop.prophylaxis, 
                    treatment_type = treatment_type,
                    max_time = max_time
                    )
  df
  
}