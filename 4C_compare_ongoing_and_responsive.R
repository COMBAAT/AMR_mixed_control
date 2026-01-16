rm(list = ls()[!grepl("^(plot|df|this_NW_set|this_vector_measure)", ls())])
source("funcs/plot_helper.R")
source("funcs/compare_responsive_and_ongoing_helper.R")

# combine df for each treatment type
df_all_ttype <- rbind(df_ttype1_F, df_ttype2_F, df_ttype3_F,
                      df_ttype1_T, df_ttype2_T, df_ttype3_T)

# extract the scenarios
scenarios <- df_all_ttype %>% 
  filter(treat_prop == 0 & prop_cattle_with_insecticide == 0 & treatments_per_year == 0) %>%
  select(NW, hosts, K_host_ratio, K, maintain_vector_pop, treatment_type)
table(scenarios$treatment_type)
scenarios

# number the scenarios
scenarios$number <- 1:nrow(scenarios)

# add the scenarios to the output data
new <- left_join(df_all_ttype, scenarios)
nrow(new)

new <- new %>% mutate(Collective_or_local = case_when(maintain_vector_pop == TRUE ~ "Local",
                                                      maintain_vector_pop == FALSE ~ "Collective"),
                      Protocol = case_when(treatment_type == "curative" ~ "Responsive curative",
                                           treatment_type == "longlasting" ~ "Responsive longlasting",
                                           treatment_type == "proph_ongoing" ~ "Responsive longlasting"))


################################################################################
# plot 'safe' region for curtaive treatment and no maintenance of vectors
p1 <- new %>% mutate(NW = as.factor(NW), K_host_ratio = as.factor(K_host_ratio)) %>% 
  filter(treatment_type == "curative", maintain_vector_pop == FALSE) %>% 
  filter( Region %in% c("Sen outcompetes Res")) %>% 
  ggplot() +
  geom_point(aes(x = prop_cattle_with_insecticide, y = treat_prop)) +
  facet_wrap(~ NW + K_host_ratio) +
  xlim(0, 1) + ylim(0, 1)
p1

# add critica points to check algorithm
best_points <- new %>% 
  filter(treatment_type == "curative", maintain_vector_pop == FALSE) %>%
  filter(Region %in% c("Sen outcompetes Res")) %>% 
  group_by(number) %>% 
  arrange(desc(treat_prop), desc(prop_cattle_with_insecticide)) %>%
  slice(1) %>%
  ungroup() %>%
  glimpse()

p1 + geom_point(data = best_points, aes(x = prop_cattle_with_insecticide, y = treat_prop), colour = "red")

################################################################################
# repeat test for proph_ongoing treatments
p3 <- new %>% mutate(NW = as.factor(NW), K_host_ratio = as.factor(K_host_ratio)) %>% 
  filter(treatment_type == "proph_ongoing", maintain_vector_pop == FALSE) %>% 
  filter( Region %in% c("Sen outcompetes Res")) %>% 
  ggplot() +
  geom_point(aes(x = prop_cattle_with_insecticide, y = treat_prop)) +
  facet_wrap(~ NW + K_host_ratio) +
  xlim(0, 1) + ylim(0, 1)
p3

best_points3 <- new %>% 
  filter(treatment_type == "proph_ongoing", maintain_vector_pop == FALSE) %>%
  filter(Region %in% c("Sen outcompetes Res")) %>% 
  group_by(number) %>% 
  arrange(desc(treatments_per_year), desc(prop_cattle_with_insecticide)) %>%
  slice(1) %>%
  ungroup() %>%
  glimpse()

p3 + geom_point(data = best_points3, aes(x = prop_cattle_with_insecticide, y = treat_prop), colour = "red")

################################################################################
# Now get all the 'best points'
best_points_responsive <- new %>% 
  filter(treatment_type != "proph_ongoing") %>% 
  filter(Region %in% c("Sen outcompetes Res")) %>% 
  group_by(number) %>% 
  arrange(desc(treat_prop), desc(prop_cattle_with_insecticide)) %>%
  slice(1) %>%
  ungroup() %>%
  glimpse()

best_points_ongoing <- new %>% 
  filter(treatment_type == "proph_ongoing") %>% 
  filter(Region %in% c("Sen outcompetes Res")) %>% 
  group_by(number) %>% 
  arrange(desc(treatments_per_year), desc(prop_cattle_with_insecticide)) %>%
  slice(1) %>%
  ungroup() %>%
  glimpse()



p_responsive <- best_points_responsive %>% 
  mutate(NW = as.factor(NW), 
         K_host_ratio = as.factor(K_host_ratio),
         Baseline_vector_host_ratio = as.factor(Baseline_vector_host_ratio)) %>%
  ggplot() + 
  geom_point(aes(x = prop_cattle_with_insecticide, y = treat_prop, 
                 size = NW, 
                 colour = Baseline_vector_host_ratio)) +
  facet_wrap(~ Protocol + Collective_or_local) +
  xlim(0, 1) + ylim(-0.1, 1) +
  labs(colour = my_label("Baseline_vector_host_ratio"), size = my_label("Wildlife")) +
  xlab(my_label("prop_cattle_with_insecticide")) +
  ylab(my_label("treat_prop")) + my_theme() +
  ggtitle("Maximum drug treatments to avoid resistance spread")
p_responsive 
  
p_ongoing <- best_points_ongoing %>% 
  mutate(NW = as.factor(NW), K_host_ratio = as.factor(K_host_ratio),
         Baseline_vector_host_ratio = as.factor(Baseline_vector_host_ratio)) %>%
  ggplot() + 
  geom_point(aes(x = prop_cattle_with_insecticide, y = treatments_per_year, 
                 size = NW, 
                 colour = Baseline_vector_host_ratio)) +
  facet_wrap(~ Protocol + Collective_or_local) +
  xlim(0, 1) + ylim(-1, 6) +
  labs(colour = my_label("Baseline_vector_host_ratio"), size = my_label("Wildlife")) +
  xlab(my_label("prop_cattle_with_insecticide")) +
  ylab(my_label("treatments_per_year")) + my_theme()
p_ongoing
  
(p_responsive / p_ongoing) + plot_layout(heights = c(2,1), axes = "collect_y", guides = "collect") 





