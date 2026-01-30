library(tictoc)
library(ggplot2)
library(dplyr)
source("run_fertility_model_core_function.R")
source("slow_sim_helper.R")
source("fast_sim_helper.R")

# An example of running the core model
n_days <- 1000000
herd_size <- 100
days_per_year <- 365.25
annual_incidence <- 0
treat_prop <- 0.1
P_abort_preg <- 0.178
end_first_trimester <- 93 
cattle_infection_period <- 100
cattle_treatment_period <- 3
period_infection_bf_treatment <- 14
gestation_period <- 283
lactation_period <- 305
fallow_period <- 90
milk_output_reduct <- 0.15
average_milk_production <- 1.17
sale_per_litre <- 0.348
labour_per_litre <- 0.07
prop_adult_female <- 0.44
calf_sale <- 92.4

version <- "v2"
create_infection_and_fertility_simulation <- if(version == "v1"){
  create_infection_and_fertility_simulation_v1 
} else {
  create_infection_and_fertility_simulation_v2
}

output_list <- create_infection_and_fertility_simulation(n_days, days_per_year,
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
                                                         milk_output_reduct)
output_list


# Now putting in a loop for multiple Incidences and taking parameters from a dataframe
#Incidence <- c(rep(1000, 10), rep(100, 10), rep(10, 10), rep(0, 10))
Incidence <- rep(c(0, 10, 100, 300, 1000), each = 5)

input_df <- data.frame(Incidence = Incidence, NC = 100, gestation_period = 283, lactation_period = 305, 
                       end_first_trimester = 93, days_fallow = 90, treat_prop = 0.1, P_abort_preg = 0.178,
                       cattle_infection_period = 100, cattle_treatment_period = 3,
                       period_infection_bf_treatment = 14, 
                       milk_output_reduct = 0.15, average_milk_production = 1.17,
                       sale_per_litre = 0.348, labour_per_litre = 0.07,
                       calf_sale = 92.4, prop_adult_female = 0.44)
input_df

n_days = 500000
version <- "v1"
tic()
updated_df_v1 <- calculate_fertility_outputs(input_df, n_days = n_days, version = version)
updated_df_v1$version <- version
toc()

version <- "v2"
tic()
updated_df_v2 <- calculate_fertility_outputs(input_df, n_days = n_days, version = version)
updated_df_v2$version <- version
toc()

updated_df <- rbind(updated_df_v1, updated_df_v2)

glimpse(updated_df)

updated_df %>% mutate(Incidence = factor(round(Incidence,0))) %>%
ggplot() +
  geom_boxplot(aes(y = Xcalves_per_adult_female, x = Incidence, colour = Incidence)) +
  ylim(0, 1) + facet_wrap(~ version)

updated_df %>% mutate(Incidence = factor(round(Incidence,0))) %>%
ggplot() +
  geom_boxplot(aes(y = Xmilk_days_per_year_per_adult_female, x = Incidence, colour = Incidence)) +
  ylim(0, 250) + facet_wrap(~ version)

updated_df %>% mutate(Incidence = factor(round(Incidence,0))) %>%
  ggplot() +
  geom_boxplot(aes(y = Xmilk_revenue_per_year_per_adult_female, x = Incidence, colour = Incidence)) +
  ylim(0, 100) + facet_wrap(~ version)

 