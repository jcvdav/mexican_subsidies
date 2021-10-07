mdl_raw <- read_csv(file.path(project_path, "data", "raw_data", "maximum_daily_liters.csv"))        # Maximum dayl liters for each engine size
mdl <- mdl_raw %>% 
  filter(year == 2019,
         fuel_type == "diesel") %>% 
  select(engine_power_hp, maximum_daily_liters)

p1 <- expand_grid(potencia = 1:4e3,
            binned_engine_power_hp = sort(unique(mdl$engine_power_hp))) %>% 
  drop_na() %>% 
  filter(binned_engine_power_hp <= potencia) %>% 
  mutate(dif = abs(potencia - binned_engine_power_hp)) %>% 
  group_by(potencia) %>% 
  top_n(n = -1, dif) %>% 
  select(-dif) %>% 
  ungroup() %>% 
  left_join(mdl, by = c("binned_engine_power_hp" = "engine_power_hp")) %>% 
  ggplot(aes(x = potencia, y = binned_engine_power_hp, color = maximum_daily_liters)) +
  geom_step(size = 1) +
  guides(color = guide_colorbar(title = "Maximum daily liters",
                                frame.colour = "black",
                                ticks.colour = "black")) +
  scale_color_viridis_c() +
  labs(x = "Vessel's engine size (HP)", y = "Discrete engine bin (HP)") +
  coord_equal() +
  theme_bw() +
  theme(legend.justification = c(0, 1),
        legend.position = c(0, 1),
        legend.background = element_blank()) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")

ggsave(plot = p1, filename = file.path(project_path, "results", "figures", "mdl_plot.png"), width = 4, height = 4)
