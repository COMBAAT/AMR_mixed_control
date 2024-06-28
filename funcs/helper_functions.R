my_rootfun <- function(t, y, params) {
  return(c(y['CS'] - 20.0, y['CIs'] - 10.0))
}

my_rootfun2 <- function (t, y, params) {
  dstate <- unlist(AAT_AMR_dens_dep(t, y, params)) # rate of change vector
  condition1 <- (y['CIs'] - 1e-5)
  condition2 <- sum(abs(dstate)) - 1e-5
  return(c(condition1, condition2))
}

my_rootfun3 <- function (t, y, params) {
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
