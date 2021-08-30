
overfishing <- readRDS(file.path(project_path, "data", "output_data", "simulated_counterfactuals.rds")) %>% 
  select(year, species, alpha, eu_rnpa, fuel_consumption_l, q_counter, pct, overfishing) %>% 
  mutate(response = ifelse(alpha == 1, "Marginal", "Average"))


fisheries_plot <- overfishing %>% 
  mutate(species = case_when(species == "shrimp plus" ~ "shrimp",
                             species == "any" ~ "Others",
                             T ~ species),
         species = str_to_sentence(species)) %>% 
  group_by(year, species, alpha, response) %>% 
  summarize(fuel_consumption_l = sum(fuel_consumption_l),
            q_counter = sum(q_counter),
            overfishing = sum(overfishing)) %>% 
  mutate(pct = overfishing / q_counter) %>% 
  ggplot(aes(x = year, y = overfishing / 1e3, linetype = response)) +
  geom_line() +
  geom_point(aes(size = pct), fill = "black") +
  facet_wrap(~species, scales = "free_y") +
  scale_x_continuous(breaks = seq(2011, 2019, by = 2), labels = seq(2011, 2019, by = 2)) +
  scale_size_continuous(labels = scales::percent) +
  labs(x = "Year", y = "Additional fuel consumption\n(Thousand liters)") +
  guides(linetype = guide_legend("Response"),
         size = guide_legend("% Change")) 

ggsave(fisheries_plot,
       filename = file.path(project_path, "results", "figures", "fisheries_subsidy_effect_timeseries.png"),
       width = 9,
       height = 5)
