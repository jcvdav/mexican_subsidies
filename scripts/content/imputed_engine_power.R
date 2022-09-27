
shrimp_registry <- tbl(mex_fisheries, "vessel_info") %>% 
  group_by(vessel_rnpa) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n == 1,
         fuel_type == "Diesel",
         shrimp == 1,
         tuna == 0,
         sardine == 0,
         others == 0) %>% 
  collect()

p <- ggplot(shrimp_registry,
            aes(x = log(vessel_length_m),
                y = log(engine_power_hp),
                fill = imputed_engine_power)) +
  geom_point() +
  labs(x = "log(Length Overall [m])",
       y = "log(Engine Power [HP])",
       fill = "Imputed") +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1))


ggsave(plot = p,
       filename = here::here("results", "img", "imputed_engine_power.pdf"),
       width = 6,
       height = 4.5)
