################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# jc_villasenor@miami.edu
# date
#
# Description
#
################################################################################
  
# SET UP #######################################################################

## Load packages ---------------------------------------------------------------
Sys.setenv(RGL_USE_NULL = TRUE)
library(fixest)
library(polars)
library(DIDmultiplegtDYN)
library(ggfixest)

## Load data -------------------------------------------------------------------
shrimp_panel <- readRDS(here("data", "estimation_panels", "shrimp_estimation_panel.rds"))

# PROCESSING ###################################################################

# Function to wrap around DID estimation
my_did <- function(var, p = 0, e = 1) {
  did_multiplegt_dyn(df = shrimp_panel |> 
                       drop_na(hours, fg_area_km, live_weight) |> 
                       filter(hours > 0,
                              fg_area_km > 0,
                              live_weight > 0) |> 
                       mutate(log_hours = log(hours),
                              log_area = log(fg_area_km),
                              log_weight = log(live_weight)),
                     outcome = var,
                     placebo = p,
                     effects = e,
                     group = "eu",
                     time = "year",
                     treatment = "treated",
                     # same_switchers = T,
                     # same_switchers_pl = T,
                     graph_off = T#,
                     # trends_nonparam = "region"
                     )
}
# Function to extract coefficients in a tidy way
coef_table <- function(model) {
  bind_rows(model$results$Effects |> as.data.frame() |> rownames_to_column(var = "term"),
            model$results$Placebos |> as.data.frame() |> rownames_to_column(var = "term")) |> 
    janitor::clean_names() |> 
    mutate(rel_time = ifelse(str_detect(term, "Placebo"), -1, 1) * as.numeric(str_extract(term, "[:digit:]")),
           label = ifelse(rel_time == 1, round(estimate, 2), "")) |> 
    bind_rows(tibble(rel_time = 0, estimate = 0, se = 0, lb_ci = 0, ub_ci = 0))
}


# Estimate models --------------------------------------------------------------
# First semi-elasticities with log outcomes
hours <- did_multiplegt_dyn(df = shrimp_panel |> 
                            drop_na(hours) |>
                              filter(hours > 0) |> 
                            mutate(h = log(hours)),
                            outcome = "h",
                            group = "eu",
                            time = "year",
                            treatment = "treated",
                            trends_nonparam = "region",
                            effects = 4,
                            placebo = 2,
                            graph_off = T)

area <- did_multiplegt_dyn(df = shrimp_panel |>
                             drop_na(fg_area_km) |> 
                            filter(fg_area_km > 0) |> 
                             mutate(a = log(fg_area_km)),
                           outcome = "a",
                           group = "eu",
                           time = "year",
                           treatment = "treated",
                           trends_nonparam = "region",
                           effects = 4,
                           placebo = 2,
                           graph_off = T)

landings <- did_multiplegt_dyn(df = shrimp_panel |>
                                 drop_na(landed_weight) |>
                                 filter(landed_weight > 0) |>
                                 mutate(l = log(landed_weight)),
                               outcome = "l",
                               group = "eu",
                               time = "year",
                               treatment = "treated",
                               trends_nonparam = "region",
                               effects = 4,
                               placebo = 2,
                               graph_off = T)

# Build the plot for text
p_logs <- list("Fishing time" = hours, "Fishing area" = area, "Landings" = landings) |> 
  map_dfr(coef_table, .id = "var") |> 
  mutate(var = fct_relevel(var, c("Fishing time", "Fishing area", "Landings"))) |> 
  ggplot(aes(x = rel_time, y = estimate, fill = var, color = var)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_linerange(aes(ymin = lb_ci, ymax = ub_ci),
                 color = "black",
                 linewidth = 0.5) +
  geom_linerange(aes(ymin = estimate - se, ymax = estimate + se),
                 linewidth = 1.5) +
  geom_point() +
  geom_text(aes(y = 1.2 * ub_ci, label = label)) +
  facet_wrap(~var, scales = "free_y", ncol = 2) +
  scale_fill_brewer(palette = "Set2", aesthetics = c("fill", "color")) +
  labs(x = "Relative time to last period before treatment changes (t = 0)",
       y = "Estimate ± (SE and 95ﬁ Conf.Int)") + 
  theme_minimal() +
  theme(legend.position = "None")


# Now in levels, for completeness ---------------------------------------------
event_studies <- c("hours", "fg_area_km", "live_weight") |> 
  map(my_did, p = 2, e = 4)

coefficients <- map_dfr(event_studies, coef_table, .id = "var") |> 
  mutate(var = case_when(var == 1 ~ "Fishing time",
                         var == 2 ~ "Fishing area",
                         var == 3 ~ "Landings"),
         var = fct_relevel(var, c("Fishing time", "Fishing area", "Landings")))

# VISUALIZE ####################################################################

## Now levels ------------------------------------------------------------------
p <- ggplot(data = coefficients,
       aes(x = rel_time, y = estimate, fill = var, color = var)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_linerange(aes(ymin = lb_ci, ymax = ub_ci),
                 color = "black",
                 linewidth = 0.5) +
  geom_linerange(aes(ymin = estimate - se, ymax = estimate + se),
                 linewidth = 1.5) +
  geom_point() +
  geom_text(aes(y = 1.2 * ub_ci, label = label)) +
  facet_wrap(~var, scales = "free_y", ncol = 2) +
  scale_fill_brewer(palette = "Set2", aesthetics = c("fill", "color")) +
  labs(x = "Relative time to last period before treatment changes (t = 0)",
       y = "Estimate ± (SE and 95ﬁ Conf.Int)") + 
  theme_minimal() +
  theme(legend.position = "None")

# EXPORT #######################################################################

## The final step --------------------------------------------------------------
ggsave(plot = p_logs,
       filename = here("content/figures/fig_enter_robustness_did_multiple_gt_dyn_logs.pdf"),
       width = 7,
       height = 5)
ggsave(plot = p,
       filename = here("content/figures/fig_enter_robustness_did_multiple_gt_dyn_levels.pdf"),
       width = 7,
       height = 5)
