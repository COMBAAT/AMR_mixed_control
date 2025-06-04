# some exploratory plots showing final simulation in scenario set
R0_and_R_trajectories(expanded_output)


if (number_of_scenarios > 1) {
  Rplot <- all_simulations_summary %>%
    filter(R0sen < 50) %>%
    mutate(reaches_equilibrium = case_when(time_final < params["max_time"] ~ TRUE, time_final == params["max_time"] ~ FALSE)) %>%
    ggplot() +
    geom_point(aes(
      y = Rsen_final, x = R0sen, colour = as.factor(reaches_equilibrium),
      shape = as.factor(treatment_type)
    )) +
    expand_limits(x = 0, y = 0) +
    geom_abline(aes(slope = 1, intercept = 0), colour = "black") +
    geom_abline(aes(slope = 0.0, intercept = 1), colour = "red", linetype = "dashed")
  Rplot
  
  all_simulations_summary %>%
    filter(Rsen_final < 100) %>%
    ggplot() +
    geom_point(aes(y = R0sen2, x = R0sen, colour = as.factor(treatment_type))) +
    geom_abline(aes(slope = 1, intercept = 0), colour = "black")
  
  all_simulations_summary %>%
    filter(Rsen_final < 100) %>%
    ggplot() +
    geom_point(aes(y = Rsen2_final, x = Rsen_final, colour = as.factor(treatment_type))) +
    geom_abline(aes(slope = 1, intercept = 0), colour = "black")
}

all_simulations_summary %>% select(time_final, starts_with("R0"), starts_with("Rs"), starts_with("Rr"), prop_cattle_with_insecticide, treatment_type) #%>% glimpse()
