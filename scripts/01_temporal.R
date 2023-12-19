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

# Load data --------------------------------------------------------------------
shrimp_panel <- readRDS(here("data", "estimation_panels", "shrimp_estimation_panel.rds"))

semi_mod <- readRDS(here("results", "models", "semi_elasticity_twfe.rds"))
elasticity_mod <- readRDS(here("results", "models", "elasticity_twfe.rds"))



c("#66C2A5" "#FC8D62" "#8DA0CB" "#E78AC3" "#A6D854" "#FFD92F" "#E5C494")



# ## PROCESSING ##################################################################
# Implications -----------
semi <- coef(semi_mod$`Fishing time`)[[1]]
change <- (exp(semi)-1)
factor <- 1 - change

elasticity <- coef(elasticity_mod$`Fishing time`)[[1]]

palette <- c(
   "Not subsidized-baseline" = "#B3B3B3",
  "Subsidized-baseline" = "#66C2A5",
  "Subsidized-subsidy" = "#A6C2A5"
)

total_outcomes <- shrimp_panel %>% 
  mutate(treated = ifelse(treated == 1, "Subsidized", "Not subsidized")) %>% 
  group_by(year, treated) %>% 
  summarize(hours = sum(hours) / 1e6)

alternative_outcomes <- shrimp_panel %>% 
  mutate(additional = treated * (hours - (factor * hours)),
         treated = ifelse(treated == 1, "Subsidized", "Not subsidized")) %>% 
  group_by(year, treated) %>% 
  summarize(hours = sum(hours) / 1e6,
            subsidy = sum(additional) / 1e6) %>% 
  mutate(baseline = hours - subsidy) %>% 
  select(year, treated, subsidy, baseline) %>% 
  pivot_longer(cols = c(subsidy, baseline),
               values_to = "hours",
               names_to = "source") %>% 
  mutate(treated = paste(treated, source, sep = "-")) %>% 
  filter(hours > 0)

# Stats for text 
alternative_outcomes %>%
  filter(source == "subsidy") %>%
  pull(hours) %>%
  range()

alternative_outcomes %>%
  group_by(year) %>%
  mutate(pct_hours = hours / sum(hours)) %>%
  filter(source == "subsidy") %>%
  pull(pct_hours) %>%
  range()

alternative_outcomes2 <- shrimp_panel %>% 
  expand_grid(pct = seq(0.1, 0.9, by = 0.2)) %>% 
  mutate(factor = 1 + (((1 - pct)^elasticity)-1)) %>% 
  mutate(additional = treated * (hours - (factor * hours)),
         treated = ifelse(treated == 1,
                          "Subsidized",
                          "Not subsidized")) %>% 
  group_by(year, treated, pct) %>% 
  summarize(hours = sum(hours) / 1e6,
            subsidy = sum(additional) / 1e6) %>% 
  mutate(baseline = hours - subsidy) %>% 
  select(year, treated, pct, subsidy, baseline) %>% 
  pivot_longer(cols = c(subsidy, baseline),
               values_to = "hours",
               names_to = "source") %>% 
  filter(hours > 0,
         treated == "Subsidized",
         source == "subsidy") %>% 
  mutate(treated = paste(treated, source, sep = "-")) 

ggplot(data = total_outcomes,
       mapping = aes(x = year, y = hours)) +
  stat_summary(geom = "area", fun = "sum") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(2011, 2019, by = 2),
                     limits = c(2011, 2019.5)) +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1)) +
  labs(x = "Year",
       y = "Total activity\n(Millions hours)")

ggplot(data = alternative_outcomes,
       mapping = aes(x = year, y = hours, fill = treated)) +
  stat_summary(aes(x = year, y = hours),
               geom = "line",
               fun = "sum",
               position = "stack",
               inherit.aes = F) +
  stat_summary(geom = "area", fun = "sum",
               position = "stack") +
  geom_line(data = alternative_outcomes2,
            mapping = aes(x = year,
                          y = hours,
                          linetype = paste0(pct * 100, "%"),
                          group = pct),
            inherit.aes = F) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(2011, 2019, by = 2),
                     limits = c(2011, 2019.5)) +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1)) +
  guides(linetype = guide_legend(ncol = 2),
         fill = "none") +
  labs(x = "Year",
       y = "Total activity\n(Millions of hours)",
       fill = "Subsidy status",
       linetype = "% Subsidy reduction") +
  scale_fill_manual(values = palette)

# Landings stuff:
l_decrease <- (exp(coef(models$`Landings`)[[1]])-1)
l_factor <- 1 - l_decrease

alternative_landings <- shrimp_panel %>% 
  drop_na(landed_weight) %>% 
  mutate(additional = treated * (landed_weight - (l_factor * landed_weight)),
         treated = ifelse(treated == 1, "Subsidized", "Not subsidized")) %>% 
  group_by(year, treated) %>% 
  summarize(landed_weight = sum(landed_weight) / 1e6,
            subsidy = sum(additional) / 1e6) %>% 
  mutate(baseline = landed_weight - subsidy) %>% 
  select(year, treated, subsidy, baseline) %>% 
  pivot_longer(cols = c(subsidy, baseline),
               values_to = "landings",
               names_to = "source") %>% 
  mutate(treated = paste(treated, source, sep = "-")) %>% 
  filter(landings > 0)


alternative_landings %>%
  filter(source == "subsidy") %>%
  pull(landings) %>%
  range()

alternative_landings %>%
  group_by(year) %>%
  mutate(pct = landings / sum(landings)) %>%
  filter(source == "subsidy") %>%
  pull(pct) %>%
  range()
  

## SPatiall attribution


