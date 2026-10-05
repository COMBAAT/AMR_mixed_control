library(ggplot2)
library(ggpattern)
library(ggtext)

source("funcs/plot_helper.R")
source("epi_plotting_functions/Federica_plots_helper_function.R")

# Settings
########################################################

this_insecticide_strategy <- "Collective"
this_baseline_vector_host_ratio <- 20
#this_wildlife <- 200
max_insecticide <- 0.95

########################################################
# Prepare data
########################################################
# Add alternative fitness values
unique(saved_simulations$prop_cattle_with_insecticide)
table(saved_simulations$prop_cattle_with_insecticide)

nrow(saved_simulations)
if (this_insecticide_strategy == "Individual") {
df <- saved_simulations %>% 
  filter(round(prop_cattle_with_insecticide, 3) %in% round(c(seq(0, 1.0, by = 0.05)), 3))
x_max <- 100
}
if (this_insecticide_strategy == "Collective") {
  df <- saved_simulations %>% filter(round(prop_cattle_with_insecticide, 3) %in% round(c(seq(0, 1.0, by = 0.02), 0.99), 3)) %>% 
    filter(prop_cattle_with_insecticide <= 0.5)
  x_max <- 50
}
nrow(df)
table(df$prop_cattle_with_insecticide)
default_fitness <- unique(df$fit_adj)

if (length(default_fitness) != 1) {
  stop("More than one default fitness value found")
}

df_all_fitnesses <- data.frame()
for (new_fitness in c(0.9, 0.8, 0.7, 0.6, 0.5)) {
  df_new <- df
  df_new$n <- 1:nrow(df_new)
  df_new$fit_adj_new <- new_fitness
  df_new$Rres_final <- df_new$Rres_final / default_fitness * df_new$fit_adj_new
  df_all_fitnesses <- rbind(df_all_fitnesses, df_new)
}

for_plotting <- df_all_fitnesses %>% 
  mutate(
    treat_percentage = 100 * treat_prop,
    No_trt_cat_curtailed = case_when(
      No_trt_cat > 120 ~ 125,
      TRUE ~ No_trt_cat,
    ),
    prop_insecticide_percent = 100 * prop_cattle_with_insecticide,
    Number_treated = cut(
      No_trt_cat, 
      breaks = c(0, 10, seq(20, 120, by = 20), Inf),
      labels = c(
        "0-10",
        "10-20",
        "20-40",
        "40-60",
        "60-80",
        "80-100",
        "100-120",
        ">120"
      ),
      include.lowest = TRUE
    ),
    drug_treatment = treat_prop + treatments_per_year,
    Insecticide = as.factor(prop_cattle_with_insecticide),
    Wildlife = as.factor(NW),
    Strategy = as.factor(treatment_code),
    prevalence = prevalence_new,
    Insecticide_strategy = case_when(
      maintain_vector_pop == FALSE ~ "Collective",
      maintain_vector_pop == TRUE ~ "Individual"
    ),
    Fitness = factor(
      fit_adj_new,
      levels = c(0.9, 0.8, 0.7, 0.6, 0.5)
    ),
    treatment_label = case_when(
      laXbel == "proph_ongoing" ~ "Longlasting\nongoing",
      laXbel == "responsive_curative" ~ "Responsive\ncurative",
      laXbel == "responsive_longlasting" ~ "Responsive\nlonglasting"
    )
  )

subset_for_plotting <- for_plotting %>% 
  filter(Baseline_vector_host_ratio == this_baseline_vector_host_ratio)

# Create subsets for responsive and ongoing
treat_prop_thresh <- 0.95
plot_this_responsive <- subset_for_plotting %>% 
  filter(
    treatment_code != 3,
    prop_cattle_with_insecticide <= max_insecticide,
    Insecticide_strategy == this_insecticide_strategy,
    treat_prop < treat_prop_thresh
    #NW == this_wildlife
  )

treatments_per_year_thresh <- 6
plot_this_ongoing <- subset_for_plotting %>%
  filter(
    treatment_code == 3,
    prop_cattle_with_insecticide <= max_insecticide,
    Insecticide_strategy == this_insecticide_strategy,
    treatments_per_year < treatments_per_year_thresh
  )

max_RiskA <- 0.5
########################################################
# 1.Responsive treatment strategies with treat prop on bdry
########################################################
variable_name <- "treat_prop"

this_fitness <- 0.8
plot_this3 <- plot_this_responsive %>% 
  filter(fit_adj_new == this_fitness) %>% mutate(
facet_label = paste0(treatment_label, "\nWildlife ", NW)
)

plot_merged_responsive <- make_plot_merged(
  plot_this3 = plot_this3,
  variable_name = variable_name,
  group_vars = c("Wildlife", "treatment_label"),
  Rres_threshold = 1
)

head(plot_merged_responsive, 20)

p_responsive <- make_safe_plot_original(
  plot_merged = plot_merged_responsive, 
  plot_this3 = plot_this3,
  fill_label = "Max recommended\ncase treatment\nproportion",
  fill_as_factor = FALSE,
  facet_type = "grid2",
  max_RiskA = max_RiskA
) +
scale_fill_viridis_c(
  option = "plasma"
)

p_responsive <- p_responsive + ggtitle(paste0(this_insecticide_strategy, " insecticide strategy"))

plot_name <- "9A_guidelines_responsive_strategy_by_treat_prop"
file_name <- paste0("output/ms_figs/", plot_name, "_",
                    this_baseline_vector_host_ratio, "_", this_insecticide_strategy, 
                    "_fitness_", this_fitness, ".pdf")
my_ggsave(
  p_responsive,
  file_name,
  height = 7,
  width = 13
)

##################################################################
# 2A.Responsive treatment strategies with number of treatments on bdry
##################################################################
variable_name <- "No_trt_cat_curtailed"

this_fitness <- 0.8
plot_this3 <- plot_this_responsive %>% 
  filter(fit_adj_new == this_fitness) %>% mutate(
    facet_label = paste0(treatment_label, "\nWildlife ", NW)
  )

plot_merged_responsive <- make_plot_merged(
  plot_this3 = plot_this3,
  variable_name = variable_name,
  group_vars = c("Wildlife", "treatment_label"),
  Rres_threshold = 1
)

head(plot_merged_responsive, 20)

p_responsive <- make_safe_plot_original(
  plot_merged = plot_merged_responsive,
  plot_this3 = plot_this3,
  fill_label = "Max recommended\nnumber of\ntreatments",
  fill_as_factor = FALSE,
  facet_type = "grid2",
  max_RiskA = max_RiskA,
  x_max = x_max
) +
  scale_fill_viridis_c(
    option = "plasma",
    direction = -1 
  )

p_responsive <- p_responsive + ggtitle(paste0(this_insecticide_strategy, " insecticide strategy"))

plot_name <- "9A_guidelines_for_responsive_strategy_by_treatments_deliveredA"
file_name <- paste0("output/ms_figs/", plot_name, "_",
                    this_baseline_vector_host_ratio, "_", this_insecticide_strategy,
                    "_fitness_", this_fitness, ".pdf")
my_ggsave(
  p_responsive,
  file_name,
  height = 7,
  width = 13
)


##################################################################
# 2B.Responsive treatment strategies with number of treatments on bdry
##################################################################
variable_name <- "Number_treated"

this_fitness <- 0.8
plot_this3 <- plot_this_responsive %>% 
  filter(fit_adj_new == this_fitness) %>% mutate(
    facet_label = paste0(treatment_label, "\nWildlife ", NW)
  )

plot_merged_responsive <- make_plot_merged(
  plot_this3 = plot_this3,
  variable_name = variable_name,
  group_vars = c("Wildlife", "treatment_label"),
  Rres_threshold = 1
)

head(plot_merged_responsive, 20)

p_responsive <- make_safe_plot_original(
  plot_merged = plot_merged_responsive,
  plot_this3 = plot_this3,
  fill_label = "Max recommended\nnumber of\ntreatments",
  fill_as_factor = FALSE,
  facet_type = "grid2",
  max_RiskA = max_RiskA,
  x_max = x_max
) +
  scale_fill_viridis_d(
    option = "plasma",
    direction = -1 
  )

p_responsive <- p_responsive + ggtitle(paste0(this_insecticide_strategy, " insecticide strategy"))

plot_name <- "9A_guidelines_for_responsive_strategy_by_treatments_deliveredB"
file_name <- paste0("output/ms_figs/", plot_name, "_",
                    this_baseline_vector_host_ratio, "_", this_insecticide_strategy,
                    "_fitness_", this_fitness, ".pdf")
my_ggsave(
  p_responsive,
  file_name,
  height = 7,
  width = 13
)

##########################################################################
# 4.Ongoing long-lasting treatment strategy with number of cases on boundary
##########################################################################
variable_name <- "Number_treated"

this_fitness <- 0.8
plot_this3 <- plot_this_ongoing %>% 
  filter(fit_adj_new == this_fitness) %>% mutate(
    facet_label = paste0(treatment_label, "\nWildlife ", NW)
  )

plot_merged_ongoing <- make_plot_merged(
  plot_this3 = plot_this3,
  variable_name = variable_name,
  group_vars = c("Wildlife", "treatment_label"),
  Rres_threshold = 1
)

head(plot_merged_ongoing, 20)


p_ongoing <- make_safe_plot_original(
  plot_merged = plot_merged_ongoing,
  plot_this3 = plot_this3,
  fill_label = "Max recommended\nnumber of\ntreatments",
  fill_as_factor = TRUE,
  facet_type = "grid2",
  max_RiskA = max_RiskA,
  x_max = x_max
 ) +
  scale_fill_viridis_d(
    option = "plasma",
    direction = -1 
  )

p_ongoing

plot_name <- "9A_guidelines_for_ongoing_strategy_by_treatments_delivered"
file_name <- paste0("output/ms_figs/", plot_name, "_",
                    this_baseline_vector_host_ratio, "_", this_insecticide_strategy, 
                    "_fitness_", this_fitness, ".pdf")
my_ggsave(
  p_ongoing,
  file_name,
  height = 4.5,
  width = 13
)

##############################################################
########################################################
# 5.Responsive treatment by fitness and wildlife
########################################################
for (this_treatment_code in c(1:2)) {
variable_name <- "treat_prop"
code <- this_treatment_code
if (code == 1) {
  this_ggtitle = "Responsive curative drug treatment & insecticide"
}
if (code == 2) {
  this_ggtitle = "Responsive longlasting drug treatment & insecticide"
}
plot_this3 <- plot_this_responsive %>% 
  filter(treatment_code == code, fit_adj_new %in% c(0.9, 0.7, 0.5))

plot_merged_responsive <- make_plot_merged(
  plot_this3 = plot_this3,
  variable_name = variable_name,
  group_vars = c("Wildlife", "Fitness"),
  Rres_threshold = 1
)

head(plot_merged_responsive)

p_responsive <- make_safe_plot_original(
  plot_merged = plot_merged_responsive, 
  plot_this3 = plot_this3,
  fill_label = "Max recommended\ncase treatment\nproportion",
  fill_as_factor = FALSE,
  facet_type = "grid1",
  add_points = TRUE,
  max_RiskA = max_RiskA,
  x_max = x_max,
  this_ggtitle = this_ggtitle
) +
  scale_fill_viridis_c(
    option = "plasma"
  )

p_responsive

plot_name <- "8A_guidelines_by_treat_prop"
file_name <- paste0("output/ms_figs/", plot_name, "_",
                    this_baseline_vector_host_ratio, "_", this_insecticide_strategy, 
                    "_all_fitness", "code_", code, "_max_RiskA_", max_RiskA, "_grid1", ".pdf")
my_ggsave(
  p_responsive,
  file_name,
  height = 7,
  width = 13
)
}
##############################################################################

##############################################################################
variable_name <- "treatments_per_year"
code <- 3
this_ggtitle = "Longlasting ongoing drug treatment & insecticide"
plot_this3 <- plot_this_ongoing %>% 
  filter(treatment_code == code, fit_adj_new %in% c(0.9, 0.7, 0.5))

plot_merged_ongoing <- make_plot_merged(
  plot_this3 = plot_this3,
  variable_name = variable_name,
  group_vars = c("Wildlife", "Fitness"),
  Rres_threshold = 1
)

head(plot_merged_ongoing, 20)

p_ongoing <- make_safe_plot_original(
  plot_merged = plot_merged_ongoing, 
  plot_this3 = plot_this3,
  fill_label = "Max recommended treatments\nper animal per year",
  fill_as_factor = FALSE,
  facet_type = "grid1",
  max_RiskA = max_RiskA,
  x_max = x_max,
  this_ggtitle = this_ggtitle
) +
  scale_fill_viridis_c(
    option = "plasma"
  )

#p_ongoing

plot_name <- "8A_guidelines_by_treatments_per_animal"
file_name <- paste0("output/ms_figs/", plot_name, "_",
                    this_baseline_vector_host_ratio, "_", this_insecticide_strategy, 
                    "_all_fitness", "code_", code, "_max_RiskA_", max_RiskA, "_grid1", ".pdf")
my_ggsave(
  p_ongoing,
  file_name,
  height = 7,
  width = 13
)


########################################################
# 5.Responsive treatment by treatment label and wildlife
########################################################
for (this_treatment_code in c(1:2)) {
  variable_name <- "Number_treated"
  code <- this_treatment_code
  if (code == 1) {
    this_ggtitle = "Responsive curative drug treatment & insecticide"
  }
  if (code == 2) {
    this_ggtitle = "Responsive longlasting drug treatment & insecticide"
  }
  plot_this3 <- plot_this_responsive %>% 
    filter(treatment_code == code, fit_adj_new %in% c(0.9, 0.7, 0.5))
  
  plot_merged_responsive <- make_plot_merged(
    plot_this3 = plot_this3,
    variable_name = variable_name,
    group_vars = c("Wildlife", "Fitness"),
    Rres_threshold = 1
  )
  
  head(plot_merged_responsive)
  
  p_responsive <- make_safe_plot_original(
    plot_merged = plot_merged_responsive, 
    plot_this3 = plot_this3,
    fill_label = "Max recommended number\nof treatments per year",
    fill_as_factor = FALSE,
    facet_type = "grid1",
    add_points = TRUE,
    max_RiskA = max_RiskA,
    x_max = x_max,
    this_ggtitle = this_ggtitle
  ) +
    scale_fill_viridis_d(
      option = "plasma",
      direction = -1 
    )
  
  p_responsive
  
  plot_name <- "8B_guidelines_by_number_treated"
  file_name <- paste0("output/ms_figs/", plot_name, "_",
                      this_baseline_vector_host_ratio, "_", this_insecticide_strategy, 
                      "_all_fitness", "code_", code, "_max_RiskA_", max_RiskA, "_grid1", ".pdf")
  my_ggsave(
    p_responsive,
    file_name,
    height = 7,
    width = 13
  )
}
##############################################################################

##############################################################################
variable_name <- "Number_treated"
code <- 3
this_ggtitle = "Longlasting ongoing drug treatment & insecticide"
plot_this3 <- plot_this_ongoing %>% 
  filter(treatment_code == code, fit_adj_new %in% c(0.9, 0.7, 0.5))

plot_merged_ongoing <- make_plot_merged(
  plot_this3 = plot_this3,
  variable_name = variable_name,
  group_vars = c("Wildlife", "Fitness"),
  Rres_threshold = 1
)

head(plot_merged_ongoing, 20)

p_ongoing <- make_safe_plot_original(
  plot_merged = plot_merged_ongoing, 
  plot_this3 = plot_this3,
  fill_label = "Max recommended treatments\n per year",
  fill_as_factor = FALSE,
  facet_type = "grid1",
  max_RiskA = max_RiskA,,
  x_max = x_max,
  this_ggtitle = this_ggtitle
) +
  scale_fill_viridis_d(
    option = "plasma",
    direction = -1 
  )

p_ongoing

plot_name <- "8B_guidelines_by_number_treated"
file_name <- paste0("output/ms_figs/", plot_name, "_",
                    this_baseline_vector_host_ratio, "_", this_insecticide_strategy, 
                    "_all_fitness", "code_", code, "_max_RiskA_", max_RiskA, "_grid1", ".pdf")
my_ggsave(
  p_ongoing,
  file_name,
  height = 7,
  width = 13
)
