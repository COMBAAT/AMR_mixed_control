library(tidyverse)

my_theme <- function() {
  theme_bw(base_size = 15) +
    theme(
      panel.background = element_rect(fill = "white"),
      plot.background  = element_rect(fill = "white", colour = NA),
      plot.title = element_text(hjust = 0.5, size = 1.0 * 15),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
      #axis.line        = element_line(colour = "#111111")
    )
}

cols <- c("#6B7C8C",  # grey-blue
          "#4A90C2",  # medium blue
          "#87CEEB") 

# cols <- c("#D6EAF8",  # very pale blue
#           "#5DADE2",  # mid blue
#           "#1B4F72")  # dark blue



df <- saved_simulations %>% 
  filter(Baseline_vector_host_ratio == 20, prop_cattle_with_insecticide == 0) %>%
  select(treatment_code, treat_prop, treatments_per_year, NW,
         R0sen_final, prevalence, RiskA, Rres_final)

df_long <- df %>%
  pivot_longer(
    cols = c(R0sen_final, prevalence, RiskA, Rres_final),
    names_to = "variable",
    values_to = "value"
  )

df_end <- df_long %>%
  group_by(variable) %>%
  slice_max(treat_prop, n = 1) %>%
  ungroup()

df_long1 <- df_long %>% filter(treatment_code == 1)

ggplot(df_long1, aes(x = treat_prop, y = value, 
                     colour = as.factor(NW),
                     linewidth = as.factor(NW))) +
  geom_line() +
  facet_wrap(~ variable, scales = "free_y", nrow = 4) +
  scale_linewidth_manual(values = c(1, 2, 3)) +
  scale_colour_manual(values = cols) +
  my_theme()


saved_simulations %>% 
  filter(near(prop_cattle_with_insecticide, 0.3),
         maintain_vector_pop == TRUE) %>%
  ggplot() +
  geom_point(aes(y = RiskA, x = Rres_final, 
                 colour = as.factor(Baseline_vector_host_ratio))) +
  facet_wrap(~NW + treatment_code, ncol = 3)

saved_simulations %>% 
  filter(maintain_vector_pop == FALSE, treatment_code == 2) %>%
  ggplot() +
  geom_point(aes(y = prop_cattle_with_insecticide, x = treat_prop, 
                 colour = Rres_final < 1)) +
  facet_wrap(~ Baseline_vector_host_ratio + NW)

saved_simulations %>% 
  filter(maintain_vector_pop == FALSE, treatment_code == 3) %>%
  ggplot() +
  geom_point(aes(y = prop_cattle_with_insecticide, x = treatments_per_year, 
                 colour = Rres_final < 1, alpha = R0sen_final > 0.001)) +
  facet_wrap(~ Baseline_vector_host_ratio + NW)

insecticide <- unique(saved_simulations$prop_cattle_with_insecticide)
length(insecticide)
insecticide <- unique(saved_simulations$prop_cattle_with_insecticide)[c(1,3,5,7,9)]

plot_this <- saved_simulations %>% 
  mutate(Incidence_wildlife = gamma_w * WEs_final * 365.25,
         prev_wildlife = case_when(NW != 0 ~WIs_final / NW, TRUE ~ 0),
         Total_incidence = Incidence_wildlife + Incidence_new,
         Incidence = Incidence_new,
         Untreated_cases = (1 - treat_prop) * Incidence_new + Incidence_wildlife,
         Treated_cases = (treat_prop) * Incidence_new,
         Refugia = Untreated_cases / (Incidence_new + Incidence_wildlife),
         Non_refugia = Treated_cases / (Incidence_new + Incidence_wildlife),
         Ratio = Treated_cases / Untreated_cases,
         Test = (Treated_cases + (1-prop_cattle_with_insecticide) * Untreated_cases) / (Incidence_new + Incidence_wildlife)) %>%
  filter(maintain_vector_pop == TRUE, prop_cattle_with_insecticide %in% insecticide,
         treatment_code == 2, 
         Rres_final < 3) 
  
ggplot(plot_this) +
  geom_point(aes(y = Rres_final, x = Ratio, colour = as.factor(prop_cattle_with_insecticide))) +
  facet_wrap(~ NW + Baseline_vector_host_ratio) #+
  #ylim(0, 3) + xlim(0, 3)

out <- lm(Rres_final ~ Ratio * prop_cattle_with_insecticide + Ratio * NW + Ratio * Baseline_vector_host_ratio, data = plot_this)
summary(out)

plot_this$pred <- predict(out, newdata = plot_this)

ggplot(plot_this) +
  geom_point(aes(y = Rres_final, x = Ratio, colour = as.factor(prop_cattle_with_insecticide))) +
  geom_line(aes(y = pred, x = Ratio,
                colour = as.factor(prop_cattle_with_insecticide),
                group = prop_cattle_with_insecticide)) +
  facet_wrap(~ NW + Baseline_vector_host_ratio) #+
  #ylim(0, 3) + xlim(0, 3)

out0 <- lm(Rres_final ~ Ratio, data = plot_this)

out1 <- lm(Rres_final ~ Ratio * prop_cattle_with_insecticide + Ratio * NW + Ratio * Baseline_vector_host_ratio, data = plot_this)
out2 <- lm(Rres_final ~ Ratio * prevalence + Ratio * prev_wildlife, data = plot_this)
out3 <- lm(Rres_final ~ Ratio * (Incidence) + Ratio * Incidence_wildlife, data = plot_this)
out4 <- lm(Rres_final ~ Ratio * (Incidence) + Ratio * Incidence_wildlife + Ratio * prop_cattle_with_insecticide, data = plot_this)
out5 <- lm(Rres_final ~ Refugia * (Incidence) + Refugia * Incidence_wildlife + Refugia * prop_cattle_with_insecticide, data = plot_this)

#summary(out0)
AIC(out0)

#summary(out1)
AIC(out1)

#summary(out2)
AIC(out2)

#summary(out3)
AIC(out3)

#summary(out4)
AIC(out4)

#summary(out5)
AIC(out5)


out <- out5
summary(out)
AIC(out)

plot_this$pred <- predict(out, newdata = plot_this)

ggplot(plot_this) +
  geom_point(aes(y = Rres_final, x = Ratio, colour = as.factor(prop_cattle_with_insecticide)), alpha = 0.2) +
  geom_line(aes(y = pred, x = Ratio,
                colour = as.factor(prop_cattle_with_insecticide),
                group = prop_cattle_with_insecticide)) +
  facet_wrap(~ NW + Baseline_vector_host_ratio, scales = "free") 
