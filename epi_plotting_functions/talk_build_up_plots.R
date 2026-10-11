# =========================================================
# Build-up plots for talks: single-panel versions of main figs 5-7
#
# Each call to plot_build_up() draws ONE panel at a given stage:
#   stage 1: insecticide only
#   stage 2: + all drug-treatment scenarios (grey)
#   stage 3: + Rres = 1 threshold line
#   stage 4: drug points coloured by the chosen outcome
#            (Rres_final, RiskA or No_trt_cat)
#
# Axes, panel size and legend position are fixed across stages, so the
# slides line up exactly and only the new layer appears on each click.
#
# Written for ggplot2 >= 4.0 (uses legend.position.inside, the guide
# `theme` argument and `linewidth` for label borders).
# =========================================================

library(dplyr)
library(ggplot2)

# ---- settings -------------------------------------------------------------
talk_base_size <- 22           # big text for projection
talk_width     <- 10           # inches (16:9 friendly)
talk_height    <- 6.5
talk_folder    <- "output/talk_figs/"
dir.create(talk_folder, showWarnings = FALSE, recursive = TRUE)

col_no_drug    <- "#4D4D4D"    # outline of insecticide-only points
col_drug_grey  <- "#C8C8C8"    # drug scenarios before colouring

# ---- data preparation (mirrors 4F_main_fig*.R) ---------------------------
prepare_talk_data <- function(saved_simulations,
                              this_treatment_code = 1,
                              this_NW = 100,
                              this_insecticide_strategy = "Individual",
                              this_vector_value = 20,
                              max_insecticide = 0.8) {
  saved_simulations %>%
    mutate(
      prop_insecticide_percent = 100 * prop_cattle_with_insecticide,
      drug_treatment = treat_prop + treatments_per_year,
      prevalence = prevalence_new,
      Insecticide_strategy = if_else(maintain_vector_pop, "Individual", "Collective")
    ) %>%
    filter(
      Baseline_vector_host_ratio == this_vector_value,
      treatment_code == this_treatment_code,
      NW == this_NW,
      Insecticide_strategy == this_insecticide_strategy,
      prop_cattle_with_insecticide <= max_insecticide
    )
}

# ---- colour scales (identical to the manuscript versions) ----------------
outcome_scale <- function(outcome) {
  switch(
    outcome,
    Rres_final = scale_colour_gradientn(
      colours = c("#D9F0D3", "#00A600", "#FDE725", "#FDAE61", "#D95F8D", "#7B3294"),
      # one value per colour; green stops just below 1, yellow starts at 1
      values = scales::rescale(c(0, 0.9999, 1, 2, 3.5, 5), from = c(0, 5)),
      limits = c(0, 5), oob = scales::squish,
      name = "Transmission\npotential"
    ),
    RiskA = scale_colour_gradientn(
      colours = c("#ABD9E9", "#74ADD1", "#FEE08B", "#FDAE61", "#7B3294"),
      values = scales::rescale(c(0, 1, 2, 4, 6, 8)),
      limits = c(0, 8), oob = scales::squish,
      name = "Selection\nopportunity"
    ),
    No_trt_cat = scale_colour_gradientn(
      colours = c("#DDEBFA", "#C77CFF", "#D95F8D", "#E76F51", "#C96A3D", "#F6A04D", "#FFF3B0"),
      limits = c(0, 250), breaks = c(0, 50, 100, 150, 200, 250),
      labels = c("0", "50", "100", "150", "200", ">250"),
      oob = scales::squish, na.value = "#FFF3B0",
      name = "Number\ntreated"
    )
  )
}

# ---- main plotting function ----------------------------------------------
plot_build_up <- function(plot_data,
                          stage = 4,
                          outcome = "Rres_final",
                          variable = "treat_prop",
                          variable_thresh = 0.95,
                          title = NULL,
                          label_curves = TRUE,
                          point_step = 0.02) {

  # points are thinned to every `point_step` of insecticide coverage
  # (0.02 matches the manuscript figures; 0.01 shows every simulated level).
  # The threshold line and insecticide-only curve always use the full grid.
  on_step <- function(x) abs(x / point_step - round(x / point_step)) < 1e-6

  no_drugs <- plot_data %>% filter(drug_treatment == 0)

  drugs <- plot_data %>%
    filter(drug_treatment > 0, .data[[variable]] < variable_thresh,
           on_step(prop_cattle_with_insecticide))

  # Rres = 1 boundary: lowest prevalence achievable with Rres < 1
  # (same definition as plot_boundary_tidy_Rres, over all drug levels)
  threshold_line <- plot_data %>%
    filter(Rres_final < 1) %>%
    group_by(prop_cattle_with_insecticide) %>%
    slice_min(order_by = prevalence_new, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(prop_cattle_with_insecticide) %>%
    # keep all positive points plus the FIRST point where prevalence hits 0,
    # so the line runs down to the axis and stops there
    filter(prevalence > 1e-6 | cumsum(prevalence <= 1e-6) == 1)

  y_max <- max(plot_data$prevalence) * 1.05

  p <- ggplot() +
    coord_cartesian(xlim = c(0, 80), ylim = c(0, y_max), expand = TRUE) +
    labs(x = "Cattle with insecticide (%)",
         y = "Prevalence in cattle",
         title = title)

  # stage 2+: drug scenarios
  if (stage >= 2) {
    if (stage >= 4) {
      p <- p +
        geom_point(data = drugs,
                   aes(prop_insecticide_percent, prevalence,
                       colour = .data[[outcome]]),
                   size = 2.6) +
        outcome_scale(outcome) +
        guides(colour = guide_colourbar(
          reverse = TRUE, nbin = 500,
          theme = theme(legend.key.height = grid::unit(5, "cm"),
                        legend.key.width  = grid::unit(0.6, "cm"))
        ))
    } else {
      p <- p +
        geom_point(data = drugs,
                   aes(prop_insecticide_percent, prevalence),
                   colour = col_drug_grey, size = 2.6)
    }
  }

  # stage 3+: threshold line
  if (stage >= 3) {
    p <- p +
      geom_line(data = threshold_line,
                aes(prop_insecticide_percent, prevalence),
                colour = "black", linewidth = 1.8)
  }

  # stage 1+: insecticide only (always on top)
  p <- p +
    geom_line(data = no_drugs,
              aes(prop_insecticide_percent, prevalence),
              colour = col_no_drug, linewidth = 0.6) +
    geom_point(data = no_drugs %>% filter(on_step(prop_cattle_with_insecticide)),
               aes(prop_insecticide_percent, prevalence),
               shape = 21, fill = "white", colour = col_no_drug,
               size = 3, stroke = 1.1)

  # direct labels instead of legends
  if (label_curves) {
    lab_no_drug <- no_drugs %>%
      filter(abs(prop_insecticide_percent - 20) < 1e-6)
    p <- p +
      annotate("text",
               x = lab_no_drug$prop_insecticide_percent + 2,
               y = lab_no_drug$prevalence + 0.07 * y_max,
               label = "Insecticide only", hjust = 0,
               size = talk_base_size / 3.2, colour = col_no_drug)

    if (stage %in% 2:3) {
      p <- p +
        annotate("text", x = 2, y = 0.03 * y_max,
                 label = "+ drug treatment", hjust = 0,
                 size = talk_base_size / 3.2, colour = "#8C8C8C")
    }

    if (stage >= 3 && nrow(threshold_line) > 0) {
      lab_line <- threshold_line %>%
        slice_min(abs(prop_insecticide_percent - 20), n = 1, with_ties = FALSE)
      p <- p +
        annotate("label",
                 x = lab_line$prop_insecticide_percent,
                 y = lab_line$prevalence,
                 label = "R[res] == 1", parse = TRUE,
                 size = talk_base_size / 3, fill = "white",
                 linewidth = 0, fontface = "bold")
    }
  }

  p +
    theme_classic(base_size = talk_base_size) +
    theme(
      plot.title = element_text(hjust = 0.5),
      # legend inside the empty top-right corner: panel size never changes
      legend.position = "inside",
      legend.position.inside = c(0.99, 0.99),
      legend.justification = c(1, 1),
      legend.background = element_blank(),
      legend.title = element_text(size = talk_base_size * 0.75),
      legend.text  = element_text(size = talk_base_size * 0.65)
    )
}

# ---- save a full series ---------------------------------------------------
save_build_up_series <- function(plot_data, name, outcome = "Rres_final",
                                 stages = 1:4, ...) {
  for (s in stages) {
    p <- plot_build_up(plot_data, stage = s, outcome = outcome, ...)
    f <- paste0(talk_folder, name, "_stage", s, ".pdf")
    ggsave(f, p, width = talk_width, height = talk_height,
           units = "in", device = cairo_pdf)
    ggsave(sub("\\.pdf$", ".png", f), p, width = talk_width,
           height = talk_height, units = "in", dpi = 200)
  }
  invisible(NULL)
}

# =========================================================
# Run
# =========================================================
# Load data: most recent Rda by default, or set a specific file
load_latest_file <- TRUE
if (load_latest_file) {
  source("funcs/helper_functions.R")
  data_file <- get_latest_Rda_file()
} else {
  data_file <- "output/11Oct2026_n6_high_insecticide_resolution.Rda"
}
message("Talk plots using: ", data_file)
load(data_file)

# Fig 7A, first panel: responsive curative, wildlife 100, individual insecticide
dat_resp_cur <- prepare_talk_data(saved_simulations, this_treatment_code = 1)

save_build_up_series(dat_resp_cur, "responsive_curative_Rres",
                     outcome = "Rres_final", stages = 1:4)

# Same panel recoloured for selection opportunity (fig 6A) - final stage only
save_build_up_series(dat_resp_cur, "responsive_curative_RiskA",
                     outcome = "RiskA", stages = 4)
