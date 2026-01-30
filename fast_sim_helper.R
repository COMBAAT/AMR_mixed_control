
create_infection_states <- function(n_days, treat_prop, daily_incidence_per_head, 
                                    duration_Treated, duration_Untreated) {
  
  if (daily_incidence_per_head < 1/(10 * 365.25)) {  # less than once per 10 years
    daily_infection_state <- rep(0, n_days)
    infection_start_days_Untreated <- c()
    infection_states <- c("0")
  } else {
  
  mean_waiting_time <- 1 / daily_incidence_per_head
  mean_interval <- mean_waiting_time + treat_prop * duration_Treated + (1 - treat_prop) * duration_Untreated
  n_periods <- if ( ceiling(n_days / mean_interval) > 10 ) {ceiling(n_days / mean_interval)} else {50}
  
  treated <- rbinom(n_periods, 1, treat_prop)
  treated_state <- rep("Treated", n_periods)
  treated_state[treated == 0] <- "Untreated"
  
  infection_states <- rep(0, 2 * n_periods)
  for (i in 1:n_periods) {
    infection_states[2 * i] <- treated_state[i]
  }
  
  # Now assign durations
  durations_uninfected <- round(rexp(n_periods, daily_incidence_per_head))
  
  # expand to create daily state
  chunks <- vector("list", length(n_periods) * 2)
  infection_start_days_v2 <- integer(n_periods)
  infection_start_days_Untreated <- integer(n_periods)
  current_day <- 0L
  
  k <- 1
  for (i in seq_along(durations_uninfected)) {
    chunks[[k]] <- rep(0, durations_uninfected[i])
    k <- k + 1
    
    current_day <- current_day + durations_uninfected[i]
    # 2) infection starts the next day
    infection_start_days_v2[i] <- current_day + 1L
    
    if (treated[i] == 1) {
      chunks[[k]] <- rep(1, duration_Treated)
      current_day <- current_day + duration_Treated
    } else {
      chunks[[k]] <- rep(2, duration_Untreated)
      infection_start_days_Untreated[i] <- current_day + 1L
      current_day <- current_day + duration_Untreated
    }
    k <- k + 1
  }
  # remove the zeros from infection_start_days_Untreated
  infection_start_days_Untreated <- infection_start_days_Untreated[infection_start_days_Untreated > 0]
  
  infection_start_days <- cumsum(durations_uninfected + ifelse(treated, duration_Treated, duration_Untreated)) - ifelse(treated, duration_Treated, duration_Untreated) + 1
  
  daily_infection_state <- unlist(chunks, use.names = FALSE)
  }
  
  list(daily_infection_state = daily_infection_state, 
       infection_start_days_Untreated = infection_start_days_Untreated,
       infection_states = infection_states)
}
################################################################################
create_fertility_states <- function(count, gestation, lactation,
                                    fallow, risk_period,
                                    P_abort_preg, infection_start_days_Untreated) {

  # Per-infection abortion triggers: thin untreated infection start days
  if (length(infection_start_days_Untreated) == 0) {
    abortion_trigger_days <- integer(0)
  } else {
    abortion_trigger_days <- infection_start_days_Untreated[
      runif(length(infection_start_days_Untreated)) < P_abort_preg
    ]
  }

  max_days <- count

  daily <- rep(NA_character_, max_days)

  # Track cycle-level states too (optional but handy)
  states <- character(0)
  preg_end_days <- integer(0)
  abort_days <- integer(0)

  # Start in lactation as your original did
  day <- 1L
  states <- c(states, "L")
  l_end <- min(max_days, day + lactation - 1L)
  daily[day:l_end] <- "L"
  day <- l_end + 1L

  # Now alternate: P -> (FA or L) -> P -> ...
  while (day <= max_days) {

    ## Pregnancy block (post-T1 pregnancy of length 'gestation')
    p_start <- day
    p_end_nominal <- min(max_days, p_start + gestation - 1L)

    # Look for the earliest abortion-trigger day during this pregnancy window
    triggers_in_preg <- abortion_trigger_days[
      abortion_trigger_days >= p_start & abortion_trigger_days <= p_end_nominal
    ]

    if (length(triggers_in_preg) > 0) {
      abort_day <- min(triggers_in_preg)          # abortion occurs here
      p_end <- abort_day
      next_state <- "FA"
      abort_days <- c(abort_days, abort_day)
    } else {
      p_end <- p_end_nominal
      next_state <- "L"
    }

    daily[p_start:p_end] <- "P"
    states <- c(states, "P")
    preg_end_days <- c(preg_end_days, p_end)

    day <- p_end + 1L
    if (day > max_days) break

    ## Next block: lactation or fallow
    if (next_state == "L") {
      states <- c(states, "L")
      block_end <- min(max_days, day + lactation - 1L)
      daily[day:block_end] <- "L"
    } else { # "FA"
      states <- c(states, "FA")
      block_end <- min(max_days, day + fallow - 1L)
      daily[day:block_end] <- "FA"
    }

    day <- block_end + 1L
  }

  # Trim to requested horizon
  daily <- daily[seq_len(count)]

  list(
    states = states,
    daily_fertility_states = daily,
    preg_end_days = preg_end_days,
    abort_days = abort_days
  )
}


################################################################################
# create_fertility_states <- function(count, gestation, lactation,
#                                     fallow, risk_period,
#                                     P_abort_preg, infection_start_days_Untreated){
# 
# 
#   normal_calving_interval <- gestation + lactation
#   # simulate enough cyles to match or exceed the length of the infection state vectors
#   n_cycles <- 2 * ceiling(count / normal_calving_interval)
# 
#   abortion_trigger_days <- infection_start_days_Untreated[runif(length(infection_start_days_Untreated)) < P_abort_preg]
# 
#   # create sequence of "P", "L", "FA" accounting for abortions
#   states <- character(n_cycles)
#   states[1] <- "L"
#   next_state <- "P"
#   latest_state <- states[1]
#   current_day <- lactation
# 
#   for (i in 2:n_cycles){
# 
#     abortion_occurs <- 0
# 
#     if (latest_state %in% c("L", "FA")) {
#       next_state <- "P"
#       latest_state <- next_state
#       current_day <- current_day + gestation
#     } else {
#       current_day <- current_day + if (next_state == "L") lactation else fallow
#       start <- max(1L, current_day - risk_period)
#       abortion_occurs <- any(abortion_trigger_days >= start & abortion_trigger_days <= current_day)
# 
#       if (abortion_occurs >= 1) {
#         next_state <- "FA"
#       } else {
#         next_state <- "L"
#       }
#       latest_state <- next_state
#     }
#     states[i] <- next_state
#   }
#   states
# 
#   # now create daily fertility states
#   fertility_durations <- c("FA" = fallow,  "P" = gestation, "L" = lactation)
# 
#   chunks <- vector("list", length(n_cycles))
#   for (i in seq_along(states)) {
#     chunks[[i]] <- rep(states[i], fertility_durations[states[i]])
#   }
#   daily_fertility_states <- unlist(chunks, use.names = FALSE)
# 
#   list(states = states, daily_fertility_states = daily_fertility_states)
# }
################################################################################

################################################################################
trim_daily_states <- function(daily_infection_state, daily_fertility_states) {
  max_rows <- min(length(daily_fertility_states), length(daily_infection_state))
  daily_fertility_states <- daily_fertility_states[1:max_rows]
  daily_infection_state <- daily_infection_state[1:max_rows]
  list(daily_fertility_states = daily_fertility_states, daily_infection_state = daily_infection_state)
}
################################################################################

################################################################################
calculate_calves_and_milk <- function(infection_states, fertility_states, daily_infection_state, 
                                      daily_fertility_states, milk_output_reduct) {
  n_days <- length(daily_fertility_states)
  total_calves <- sum(fertility_states == "L")
  calves_per_year <- total_calves / n_days * 365.25
  total_calves2 <- sum(daily_fertility_states[-length(daily_fertility_states)] == "P" & daily_fertility_states[-1] == "L")
  calves_per_year2 <- total_calves2 / n_days * 365.25
  
  milk_days <- numeric(n_days)
  milk_days[daily_fertility_states == "L"] <- 1
  
  relative_milk_days <- numeric(n_days)
  relative_milk_days[milk_days == 1] <- 1
  relative_milk_days[milk_days == 1 & daily_infection_state == 2] <- 1 - milk_output_reduct
  total_milk_days = sum(relative_milk_days)
  milk_days_per_year <- total_milk_days / n_days * 365.25
  
  list(sim = "v2", calves_per_year = calves_per_year, 
       calves_per_year2 = calves_per_year2,
       milk_days_per_year = milk_days_per_year)
}
################################################################################

################################################################################
create_infection_and_fertility_simulation_v2 <- function(n_days, days_per_year,
                                                         herd_size, 
                                                         annual_incidence, 
                                                         treat_prop, 
                                                         P_abort_preg, 
                                                         end_first_trimester, 
                                                         cattle_infection_period,
                                                         cattle_treatment_period,
                                                         period_infection_bf_treatment,
                                                         gestation_period,
                                                         lactation_period,
                                                         fallow_period,
                                                         milk_output_reduct){
  
  daily_incidence_per_head = (annual_incidence / herd_size) / 365.25
  
  duration_Treated = cattle_treatment_period + period_infection_bf_treatment 
  duration_Untreated = cattle_infection_period
  out <- create_infection_states(n_days, treat_prop, daily_incidence_per_head, 
                                 duration_Treated = duration_Treated, 
                                 duration_Untreated = duration_Untreated)
  
  daily_infection_state <- out$daily_infection_state
  infection_start_days_Untreated <- out$infection_start_days_Untreated
  infection_states <- out$infection_states
  count_infection_states <- length(daily_infection_state)
  count_infection_states / 1000000 
  
  
  # Now map on the top the fertility states
  gestation_post_T1 <- gestation_period - end_first_trimester
  risk_period <- gestation_post_T1
  out2 <- create_fertility_states(count_infection_states, gestation_post_T1, lactation_period, 
                                  fallow_period, risk_period,
                                  P_abort_preg, infection_start_days_Untreated)
  daily_fertility_states <- out2$daily_fertility_states
  fertility_states <- out2$states
  length(daily_fertility_states)
  length(daily_infection_state)
  
  out3 <- trim_daily_states(daily_infection_state, daily_fertility_states)
  daily_infection_state <- out3$daily_infection_state
  daily_fertility_states <- out3$daily_fertility_states
  
  out4 <- calculate_calves_and_milk(infection_states, fertility_states, daily_infection_state, 
                                    daily_fertility_states, milk_output_reduct)
  
  list(version = "v2", 
       calves_per_year_per_adult_female = out4$calves_per_year, 
       calves_per_year_per_adult_female2 = out4$calves_per_year2, 
       milk_days_per_year_per_adult_female = out4$milk_days_per_year)
}