
overfishing <- readRDS(file.path(project_path, "data", "output_data", "simulated_counterfactuals.rds")) %>% 
  select(year, species, alpha, eu_rnpa, fuel_consumption_l, q_counter, pct, overfishing) %>% 
  mutate(response = ifelse(alpha == 1, "Marginal", "Average"))

overfishing %>% 
  mutate(species = case_when(species == "shrimp plus" ~ "shrimp",
                             species == "any" ~ "finfish",
                             T ~ species)) %>% 
  group_by(year, species, alpha, response) %>% 
  summarize(fuel_consumption_l = sum(fuel_consumption_l),
            q_counter = sum(q_counter),
            overfishing = sum(overfishing)) %>% 
  mutate(pct = overfishing / q_counter) %>% 
  ggplot(aes(x = year, y = pct, linetype = response)) +
  geom_line() +
  facet_wrap(~species, scales = "free_y") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Year", y = "% Additional fuel consumption") +
  guides(linetype = guide_legend("Response")) +
  theme(legend.justification = c(1, 1),
        legend.position = c(1, 1))

overfishing %>% 
  mutate(species = case_when(species == "shrimp plus" ~ "shrimp",
                             species == "any" ~ "finfish",
                             T ~ species)) %>% 
  group_by(year, species, alpha, response) %>% 
  summarize(fuel_consumption_l = sum(fuel_consumption_l),
            q_counter = sum(q_counter),
            overfishing = sum(overfishing)) %>% 
  mutate(pct = overfishing / q_counter) %>% 
  ggplot(aes(x = year, y = overfishing / 1e6, linetype = response)) +
  geom_line() +
  facet_wrap(~species, scales = "free_y") +
  scale_y_continuous() +
  labs(x = "Year", y = "Additional fuel consumption\n(Million liters)") +
  guides(linetype = guide_legend("Response")) +
  theme(legend.justification = c(1, 1),
        legend.position = c(1, 1))
