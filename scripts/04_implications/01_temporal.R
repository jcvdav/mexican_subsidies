################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
# How many times does a vessel enter / exit the roster?
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  fixest,
  modelsummary,
  tidyverse
)

theme_set(theme_minimal(base_size = 10))

# Load data --------------------------------------------------------------------
shrimp_panel <- readRDS(here("data", "estimation_panels", "shrimp_estimation_panel.rds"))

semi_mod <- readRDS(here("data", "output", "semi_elasticity_twfe_model.rds"))
elasticity_mod <- readRDS(here("data", "output", "elasticity_twfe_model.rds"))


pcts <- c(0.1, 0.3, 0.5, 0.9)

## FUNCTIONS ####################################################################

calculate_semi_elasticity_counterfactual <- function(model, var_col) {
  
  # Extract coefficient and calculate % change
  semi_coef <- coef(model)[[1]]
  change <- as.numeric(exp(semi_coef) - 1)
  
  # Calculate counterfactual
  result <- shrimp_panel %>%
    select(year, eu, treated, !!sym(var_col)) %>%
    mutate(
      additional = ifelse(treated == 1, change * !!sym(var_col), 0),
      treated = ifelse(treated == 1, "Subsidized", "Not subsidized")
    ) %>%
    group_by(year, treated) %>%
    summarize(
      !!sym(var_col) := sum(!!sym(var_col)),
      subsidy = sum(additional),
      .groups = "drop"
    ) %>%
    mutate(baseline = !!sym(var_col) - subsidy) %>%
    select(-!!sym(var_col)) %>%
    pivot_longer(
      cols = c(subsidy, baseline),
      values_to = var_col,
      names_to = "source"
    ) %>%
    mutate(treated = paste(treated, source, sep = "-")) %>%
    filter(!!sym(var_col) > 0)
  
  return(result)
}

calculate_elasticity_counterfactual <- function(model, var_col, pct_reductions = c(0.1, 0.3, 0.5, 0.9)) {
  
  # Extract elasticity coefficient
  elasticity <- as.numeric(coef(model)[[1]])
  
  # Calculate counterfactual for different reduction scenarios
  result <- shrimp_panel %>%
    drop_na(!!sym(var_col)) %>%
    expand_grid(pct = pct_reductions) %>%
    mutate(
      factor = 1 + (((1 - pct)^elasticity) - 1),
      additional = treated * (!!sym(var_col) - (factor * !!sym(var_col))),
      treated = ifelse(treated == 1, "Subsidized", "Not subsidized")
    ) %>%
    group_by(year, treated, pct) %>%
    summarize(
      !!sym(var_col) := sum(!!sym(var_col)),
      subsidy = sum(additional),
      .groups = "drop"
    ) %>%
    mutate(baseline = !!sym(var_col) - subsidy) %>%
    select(year, treated, pct, subsidy, baseline) %>%
    pivot_longer(
      cols = c(subsidy, baseline),
      values_to = var_col,
      names_to = "source"
    ) %>%
    filter(
      !!sym(var_col) > 0,
      treated == "Subsidized",
      source == "subsidy"
    ) %>%
    mutate(treated = paste(treated, source, sep = "-"))
  
  return(result)
}

generate_counterfactual_summary <- function(data, var_col) {
  
  # Overall summary by year
  overall_stats <- data %>%
    group_by(year) %>%
    summarize(!!sym(var_col) := sum(!!sym(var_col)), .groups = "drop") %>%
    pull(!!sym(var_col)) %>%
    summary()
  
  # Summary for subsidized vessels only
  subsidized_stats <- data %>%
    filter(treated != "Not subsidized-baseline") %>%
    group_by(year) %>%
    summarize(!!sym(var_col) := sum(!!sym(var_col)), .groups = "drop") %>%
    pull(!!sym(var_col)) %>%
    summary()
  
  # Values attributable to subsidy
  subsidy_attributable_stats <- data %>%
    filter(treated != "Not subsidized-baseline") %>%
    filter(source == "subsidy") %>%
    pull(!!sym(var_col)) %>%
    summary()
  
  # Percentage of total attributable to subsidy
  pct_attributable_stats <- data %>%
    group_by(year) %>%
    mutate(pct_total = !!sym(var_col) / sum(!!sym(var_col))) %>%
    filter(source == "subsidy") %>%
    pull(pct_total) %>%
    summary()
  
  # Combine summaries
  summary_table <- rbind(
    data.frame(Statistic = "Overall", overall_stats),
    data.frame(Statistic = "Subsidized", subsidized_stats),
    data.frame(Statistic = "Subsidy Attributable", subsidy_attributable_stats),
    data.frame(Statistic = "Pct Attributable", pct_attributable_stats)
  )
  
  return(summary_table)
}

# ## PROCESSING ##################################################################

# Extract coefficients ---------------------------------------------------------
# For semi-elasticity
semi <- coef(semi_mod$`Fishing time`)[[1]] # Extract the semi-elasticity
change <- (exp(semi)-1) # Calculate the % change equivalent
# factor <- 1 - change # Calculate the reduction in the absence of subsidies

# For elasticity
elasticity <- coef(elasticity_mod$`Fishing time`)[[1]] # Extract elasticity coefficient

# Calculate the counterfactual under no subsidies
alternative_hours <- shrimp_panel %>% 
  mutate(#additional = treated * (hours - (factor * hours)),
         additional = ifelse(treated == 1, change * hours, 0),                  # Calculate total fishing effort attributable to subsidy
         treated = ifelse(treated == 1, "Subsidized", "Not subsidized")         # Convert to useful character
         ) %>% 
  # Now calculate total effort and effort caused by subsidy by treatment group
  group_by(year, treated) %>% 
  summarize(hours = sum(hours),                                           # Convert to millions of hours
            subsidy = sum(additional),                                    # Convert to millions of hours. Always zero for treated == "Not subsidized
            .groups = "drop") %>% 
  # Calculate the amount of effort that we would have expected to see in the absence of subsidies
  mutate(baseline = hours - subsidy) %>% 
  select(-hours) %>% # We can now get rid of the actual observed hours
  # And pivot the table so we now where the hours come from (subsidy or baseline)
  pivot_longer(cols = c(subsidy, baseline),
               values_to = "hours",
               names_to = "source") %>% 
  mutate(treated = paste(treated, source, sep = "-")) %>% 
  # And we now remove cases where hours is zero. This removes the "additional"
  #effort by vessels not subsidized, which is always zero by construction
  filter(hours > 0)

# We now repeat the same process as above, but this time using % changes to simulate policy reductions
alternative_hours2 <- shrimp_panel %>% 
  expand_grid(pct = pcts) %>% 
  mutate(factor = 1 + (((1 - pct)^elasticity)-1)) %>% 
  mutate(additional = treated * (hours - (factor * hours)),
         treated = ifelse(treated == 1,
                          "Subsidized",
                          "Not subsidized")) %>% 
  group_by(year, treated, pct) %>% 
  summarize(hours = sum(hours),
            subsidy = sum(additional),
            .groups = "drop") %>% 
  mutate(baseline = hours - subsidy) %>% 
  select(year, treated, pct, subsidy, baseline) %>% 
  pivot_longer(cols = c(subsidy, baseline),
               values_to = "hours",
               names_to = "source") %>% 
  filter(hours > 0,
         treated == "Subsidized",
         source == "subsidy") %>% 
  mutate(treated = paste(treated, source, sep = "-")) 

# Stats for text ---

# Summary of hours for fleet
alternative_hours %>% 
  group_by(year) %>% 
  summarize(hours = sum(hours), .groups = "drop") %>% 
  pull(hours) %>% 
  summary()

alternative_hours %>% 
  group_by(year) %>% 
  summarize(hours = sum(hours), .groups = "drop") %>% 
  pull(hours) %>% 
  sd()

# Summary of hours for subsidized vessels
alternative_hours %>% 
  filter(treated != "Not subsidized-baseline") %>% 
  group_by(year) %>% 
  summarize(hours = sum(hours), .groups = "drop") %>% 
  pull(hours) %>% 
  summary()

alternative_hours %>% 
  filter(treated != "Not subsidized-baseline") %>% 
  group_by(year) %>% 
  summarize(hours = sum(hours), .groups = "drop") %>% 
  pull(hours) %>% 
  sd()

# Range of hours by subsidized vessels, and attributable to the subsidy
alternative_hours %>% 
  filter(treated != "Not subsidized-baseline") %>% 
  filter(source == "subsidy") %>%
  pull(hours) %>%
  summary()

alternative_hours %>% 
  filter(treated != "Not subsidized-baseline") %>% 
  filter(source == "subsidy") %>%
  pull(hours) %>%
  sd()

# % of total hours attributable to subsidy
alternative_hours %>% 
  group_by(year) %>%
  mutate(pct_hours = hours / sum(hours)) %>%
  filter(source == "subsidy") %>%
  pull(pct_hours) %>%
  range()

## Area stuff ##############################################################
a_semi <- coef(semi_mod$`Fishing area`)[[1]]
a_change <- (exp(a_semi)-1)

a_elasticity <- coef(elasticity_mod$`Fishing area`)[[1]]

alternative_area <- shrimp_panel %>% 
  drop_na(fg_area_km) %>% 
  select(year, eu, treated, fg_area_km) %>% 
  mutate(additional = treated * (fg_area_km - (a_factor * fg_area_km)),
         treated = ifelse(treated == 1, "Subsidized", "Not subsidized")) %>% 
  group_by(year, treated) %>% 
  summarize(fg_area_km = sum(fg_area_km) / 1e6,
            subsidy = sum(additional) / 1e6) %>% 
  mutate(baseline = fg_area_km - subsidy) %>% 
  select(year, treated, subsidy, baseline) %>% 
  pivot_longer(cols = c(subsidy, baseline),
               values_to = "fg_area_km",
               names_to = "source") %>% 
  mutate(treated = paste(treated, source, sep = "-")) %>% 
  filter(fg_area_km > 0)

alternative_area2 <- shrimp_panel %>% 
  drop_na(fg_area_km) %>% 
  expand_grid(pct = pcts) %>% 
  mutate(factor = 1 + (((1 - pct)^a_elasticity)-1)) %>% 
  mutate(additional = treated * (fg_area_km - (factor * fg_area_km)),
         treated = ifelse(treated == 1,
                          "Subsidized",
                          "Not subsidized")) %>% 
  group_by(year, treated, pct) %>% 
  summarize(fg_area_km = sum(fg_area_km) / 1e6,
            subsidy = sum(additional) / 1e6) %>% 
  mutate(baseline = fg_area_km - subsidy) %>% 
  select(year, treated, pct, subsidy, baseline) %>% 
  pivot_longer(cols = c(subsidy, baseline),
               values_to = "fg_area_km",
               names_to = "source") %>% 
  filter(fg_area_km > 0,
         treated == "Subsidized",
         source == "subsidy") %>% 
  mutate(treated = paste(treated, source, sep = "-")) 

# Range of annual area
alternative_area %>% 
  group_by(year) %>% 
  summarize(fg_area_km = sum(fg_area_km)) %>% 
  pull(fg_area_km) %>% 
  range()

# Range of annual area by subsidized vessels
alternative_area %>% 
  filter(treated != "Not subsidized-baseline") %>% 
  group_by(year) %>% 
  summarize(fg_area_km = sum(fg_area_km)) %>% 
  pull(fg_area_km) %>% 
  range()

# Range of area by subsidized vessels, and attributable to the subsidy
alternative_area %>%
  filter(treated != "Not subsidized-baseline") %>% 
  filter(source == "subsidy") %>%
  pull(fg_area_km) %>%
  range()

# As percent
alternative_area %>%
  ungroup() %>% 
  group_by(year) %>%
  mutate(pct = fg_area_km / sum(fg_area_km)) %>% 
  filter(source == "subsidy") %>%
  pull(pct) %>%
  range()


## Landings stuff ##############################################################
l_semi <- coef(semi_mod$`Landings`)[[1]]
l_change <- (exp(l_semi)-1)
l_factor <- 1 - l_change

l_elasticity <- coef(elasticity_mod$`Landings`)[[1]]

alternative_landings <- shrimp_panel %>% 
  drop_na(landed_weight) %>% 
  select(year, eu, treated, landed_weight) %>% 
  mutate(additional = treated * (landed_weight - (l_factor * landed_weight)),
         treated = ifelse(treated == 1, "Subsidized", "Not subsidized")) %>% 
  group_by(year, treated) %>% 
  summarize(landed_weight = sum(landed_weight) / 1e6,
            subsidy = sum(additional) / 1e6) %>% 
  mutate(baseline = landed_weight - subsidy) %>% 
  select(year, treated, subsidy, baseline) %>% 
  pivot_longer(cols = c(subsidy, baseline),
               values_to = "landed_weight",
               names_to = "source") %>% 
  mutate(treated = paste(treated, source, sep = "-")) %>% 
  filter(landed_weight > 0)

alternative_landings2 <- shrimp_panel %>% 
  drop_na(landed_weight) %>% 
  expand_grid(pct = pcts) %>% 
  mutate(factor = 1 + (((1 - pct)^l_elasticity)-1)) %>% 
  mutate(additional = treated * (landed_weight - (factor * landed_weight)),
         treated = ifelse(treated == 1,
                          "Subsidized",
                          "Not subsidized")) %>% 
  group_by(year, treated, pct) %>% 
  summarize(landed_weight = sum(landed_weight) / 1e6,
            subsidy = sum(additional) / 1e6) %>% 
  mutate(baseline = landed_weight - subsidy) %>% 
  select(year, treated, pct, subsidy, baseline) %>% 
  pivot_longer(cols = c(subsidy, baseline),
               values_to = "landed_weight",
               names_to = "source") %>% 
  filter(landed_weight > 0,
         treated == "Subsidized",
         source == "subsidy") %>% 
  mutate(treated = paste(treated, source, sep = "-")) 

# Range of annual landings
alternative_landings %>% 
  group_by(year) %>% 
  summarize(landed_weight = sum(landed_weight)) %>% 
  pull(landed_weight) %>% 
  range()

# Range of annual landings by subsidized vessels
alternative_landings %>% 
  filter(treated != "Not subsidized-baseline") %>% 
  group_by(year) %>% 
  summarize(landed_weight = sum(landed_weight)) %>% 
  pull(landed_weight) %>% 
  range()

# Range of landings by subsidized vessels, and attributable to the subsidy
alternative_landings %>%
  filter(treated != "Not subsidized-baseline") %>% 
  filter(source == "subsidy") %>%
  pull(landed_weight) %>% 
  range()

alternative_landings %>%
  ungroup() %>% 
  group_by(year) %>%
  mutate(pct = landed_weight / sum(landed_weight)) %>% 
  filter(source == "subsidy") %>%
  pull(pct) %>%
  range()
  

## VISUALIZE ###################################################################

# Hours ------------------------------------------------------------------------
palette <- c(
  "Not subsidized-baseline" = "#B3B3B3",
  "Subsidized-baseline" = "#66C2A5",
  "Subsidized-subsidy" = "#A6C2A5"
)

p1 <- ggplot(data = alternative_hours,
             mapping = aes(x = year, y = hours, fill = treated)) +
  stat_summary(aes(x = year, y = hours),
               geom = "line",
               fun = "sum",
               position = "stack",
               inherit.aes = F) +
  stat_summary(geom = "area", fun = "sum",
               position = "stack") +
  geom_line(data = alternative_hours2,
            mapping = aes(x = year,
                          y = hours,
                          linetype = paste0(pct * 100, "%"),
                          group = pct),
            inherit.aes = F) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(2011, 2019, by = 2),
                     limits = c(2011, 2019.5)) +
  scale_fill_manual(values = palette) +
  labs(title = "Fishing time",
       x = "Year",
       y = "Total activity\n(Millions of hours)",
       fill = "Source",
       linetype = "% Subsidy reduction") +
  theme(legend.position = "None")

# Area -------------------------------------------------------------------------
palette <- c(
  "Not subsidized-baseline" = "#B3B3B3",
  "Subsidized-baseline" = "#FC8D62",
  "Subsidized-subsidy" = "#CC8D62"
)

p2 <- ggplot(data = alternative_area,
             mapping = aes(x = year, y = fg_area_km, fill = treated)) +
  stat_summary(aes(x = year, y = fg_area_km),
               geom = "line",
               fun = "sum",
               position = "stack",
               inherit.aes = F) +
  stat_summary(geom = "area", fun = "sum",
               position = "stack") +
  geom_line(data = alternative_area2,
            mapping = aes(x = year,
                          y = fg_area_km,
                          linetype = paste0(pct * 100, "%"),
                          group = pct),
            inherit.aes = F) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(2011, 2019, by = 2),
                     limits = c(2011, 2019.5)) +
  scale_fill_manual(values = palette) +
  labs(title = "Fishing area",
       x = "Year",
       y = "Total area\n(Millions of Km2)",
       fill = "Source",
       linetype = "% Subsidy reduction") +
  theme(legend.position = "None")


# Landings ---------------------------------------------------------------------

palette <- c(
  "Not subsidized" = "#B3B3B3",
  "Subsidized" = "#8DA0CB",
  "Subsidy" = "#ADA0CB"
)

p3 <- ggplot(data = alternative_landings %>% 
               mutate(treated = str_remove(treated, "-baseline"),
                      treated = ifelse(treated == "Subsidized-subsidy", "Subsidy", treated)),
             mapping = aes(x = year, y = landed_weight, fill = treated)) +
  stat_summary(aes(x = year, y = landed_weight),
               geom = "line",
               fun = "sum",
               position = "stack",
               inherit.aes = F) +
  stat_summary(geom = "area", fun = "sum",
               position = "stack") +
  geom_line(data = alternative_landings2,
            mapping = aes(x = year,
                          y = landed_weight,
                          linetype = paste0(pct * 100, "%"),
                          group = pct),
            inherit.aes = F) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(2011, 2019, by = 2),
                     limits = c(2011, 2019.5)) +
  scale_fill_manual(values = palette) +
  labs(title = "Landings",
       x = "Year",
       y = "Total Landings\n(Thousand tonnes)",
       fill = "Source",
       linetype = "% Subsidy reduction") +
  theme(legend.position = "None")

leg <- cowplot::get_plot_component(
  p3 +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(ncol = 1, title.position = "top"),
           linetype = guide_legend(ncol = 2, title.position = "top")),
  pattern = "guide-box-bottom",
  return_all = T
)

plot <- cowplot::plot_grid(p1, p2, p3, leg,
                           ncol = 2, labels = c("a)", "b)", "c)"),
                           align = "hv")

ggsave(plot = plot,
       filename = here("results", "img", "fig_temporal_attribution.pdf"),
       width = 7,
       height = 4)



pp <- bind_rows(alternative_hours2 %>% rename(value = hours),
          alternative_area2 %>% rename(value = fg_area_km),
          alternative_landings2 %>% rename(value = landed_weight),
          .id = "variable") %>% 
  mutate(variable = case_when(variable == 1 ~ "d) Fishing time (Millions of hours)",
                              variable == 2 ~ "e) Fishing area (Millions of Km2)",
                              variable == 3 ~ "f) Landings (Tones)"),
         variable = fct_relevel(variable,
                                "d) Fishing time (hours)",
                                "e) Fishing area (Km2)",
                                "f) Landings (Kg)")) %>% 
  ggplot(aes(x = pct, y = -value, color = variable)) +
  stat_summary(geom = "pointrange", fun.data = mean_cl_normal, color = "black") +
  stat_summary(geom = "pointrange", fun.data = mean_se, linewidth = 1.5) +
  facet_wrap(~variable, scales = "free_y", ncol = 3) +
  scale_x_continuous(labels = scales::percent) +
  scale_color_brewer(palette = "Set2") +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "%Change in subsidy allocations",
       y = "Mean reduction") +
  theme_minimal() +
  theme(legend.position = "None")

ggsave(plot = pp,
       filename = here("results", "img", "fig_pct_reductions.pdf"),
       width = 6,
       height = 2)
