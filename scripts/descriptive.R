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


# ## PROCESSING ##################################################################
# Implications -----------
decrease <- (exp(coef(models$`Hours`)[[1]])-1)
factor <- 1 - decrease

palette <- c(
  "#E41A1C",
  "#377EB8",
  "steelblue1"
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
  mutate(factor = 1 + (((1 - pct)^coefficients(elasticity_twfe[[1]])[1])-1)) %>% 
  mutate(additional = treated * (hours - (factor * hours)),
         treated = ifelse(treated == 1, "Subsidized", "Not subsidized")) %>% 
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

ggplot(data = total_outcomes,
       mapping = aes(x = year, y = hours, fill = treated)) +
  stat_summary(geom = "area", fun = "sum", position = "stack") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(2011, 2019, by = 2),
                     limits = c(2011, 2019.5)) +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1)) +
  labs(x = "Year",
       y = "Total activity\n(Millions of hours)",
       fill = "Subsidy status") +
  scale_fill_manual(values = palette)

ggplot(data = alternative_outcomes,
       mapping = aes(x = year, y = hours, fill = treated)) +
  stat_summary(aes(x = year, y = hours), geom = "line", fun = "sum", position = "stack", inherit.aes = F) +
  stat_summary(geom = "area", fun = "sum", position = "stack") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(2011, 2019, by = 2),
                     limits = c(2011, 2019.5)) +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1)) +
  labs(x = "Year",
       y = "Total activity\n(Millions of hours)",
       fill = "Subsidy status") +
  scale_fill_manual(values = palette)

ggplot(data = alternative_outcomes,
       mapping = aes(x = year, y = hours, fill = treated)) +
  stat_summary(aes(x = year, y = hours),
               geom = "line", fun = "sum",
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


mex <- rnaturalearth::ne_countries(country = "Mexico", returnclas = "sf")

continent <- rnaturalearth::ne_countries(continent = "North America", returnclass = "sf") %>% 
  sf::st_crop(sf::st_buffer(mex, dist = 1.5))

treated_in_2019 <- shrimp_panel %>% 
  filter(year == 2019,
         treated == 1) %>% 
  pull(eu)

shrimp_tracks <- readRDS(here("data", "processed",  "2019_shrimp_tracks.rds"))

res <- 0.1

tracks_info <- shrimp_tracks %>% 
  mutate(lon = (floor(lon / res) * res) + (res / 2),
         lat = (floor(lat / res) * res) + (res / 2),
         treated = 1 * (eu_rnpa %in% treated_in_2019)) %>% 
  # expand_grid(pct = seq(0.1, 0.9, by = .2)) %>%
  mutate(factor = factor) %>% 
  mutate(additional = treated * (hours - (factor * hours))) %>% 
  group_by(lat, lon) %>% 
  summarize(hours = sum(hours, na.rm = T),
            additional = sum(additional, na.rm = T),
            n_eus = n_distinct(eu_rnpa)) %>% 
  filter(lat < 35) %>% 
  mutate(difference = additional / hours)


# Baseline plot
ggplot() +
  geom_sf(data = continent, color = "black") +
  geom_tile(data = tracks_info, aes(x = lon, y = lat, fill = log(hours))) +
  geom_sf(data = mex, color = "black") +
  scale_fill_viridis_c() +
  guides(fill = guide_colorbar(title = "log(Hours)",
                               frame.colour = "black",
                               ticks.colour = "black")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "",
       y = "")

# % Subsidy
ggplot() +
  geom_tile(data = tracks_info, aes(x = lon,
                                    y = lat,
                                    fill = difference)) +
  geom_sf(data = continent, color = "black") +
  geom_sf(data = mex, color = "black") +
  scale_fill_viridis_c(labels = scales::percent, option = "B") +
  guides(fill = guide_colorbar(title = "% Attributable to subsidy",
                               frame.colour = "black",
                               ticks.colour = "black")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "",
       y = "")

ggplot() +
  geom_tile(data = tracks_info, aes(x = lon, y = lat, fill = log(additional))) +
  geom_sf(data = continent, color = "black") +
  geom_sf(data = mex, color = "black") +
  scale_fill_viridis_c() +
  guides(fill = guide_colorbar(title = "log(Subsidized Hours)",
                               frame.colour = "black",
                               ticks.colour = "black")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "",
       y = "")

ggplot() +
  geom_tile(data = tracks_info %>% mutate(pct = paste0(pct * 100, "% reduction")), aes(x = lon, y = lat, fill = difference)) +
  geom_sf(data = continent, color = "black") +
  geom_sf(data = mex, color = "black") +
  scale_fill_viridis_c(labels = scales::percent, option = "B") +
  # scale_fill_gradient(low = "steelblue", high = "red", labels = scales::percent) +
  guides(fill = guide_colorbar(title = "% Reduction",
                               frame.colour = "black",
                               ticks.colour = "black",
                               title.position = "top",
                               direction = "horizontal")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_minimal() +
  theme(legend.position = c(1, 0),
        legend.justification = c(1, 0)) +
  labs(x = "",
       y = "",
       title = "Expected reduction in fishing activity from five\nsubsidy reduction policies",
       subtitle = "Maps are produced from 2019 fishing activity on a 0.1° grid") +
  facet_wrap(~pct, ncol = 2)

