library(tidyverse)
library(trelliscopejs)

dpc <- read.csv(file.path(project_path, "raw_data", "days_per_cycle.csv"))
mdl <- read.csv(file.path(project_path, "raw_data", "maximum_daily_liters.csv"))

fuel_prices <- tibble(fuel_type = c("diesel", "gasoline"),
                      P = c(21, 23))


a <- dpc %>% 
  left_join(mdl, by = c("year", "fuel_type")) %>% 
  left_join(fuel_prices, by = "fuel_type") %>% 
  mutate(L = days_per_cycle * maximum_daily_liters,
         La = L * adjustment_factor)



for(i in 1:length(l)){
  pm[i] <- mean(P[1:i])
}

plot(P ~ l, type = "S")
lines(pm ~ l)


mean_p <- function(L, Ls, P){
  l <- c(seq(-150, Ls, by = 10), seq(Ls + 1, L, by = 10))
  p <- ifelse(l<=Ls, P - 2, P)
  tibble(l,
         p,
         p_mean = cumsum(p) / 1:length(p))
}






# GOOD STUFF STARTS HERE
mean_prices <- a %>% 
  as_tibble() %>% 
  mutate(group = paste(fuel_type, species, engine_power_hp, sep = "_")) %>% 
  filter(str_detect(group, "tuna_3600"),
         location == "pacific",
         year == 2019) %>% 
  mutate(data = pmap(.l = list(L, La, P), mean_p)) %>% 
  unnest(data)



power_match <- expand_grid(vessel_power = unique(pmin(floor(effort$engine_power_hp / 10) * 10, 3600)),
                           engine_power_hp = sort(unique(mdl$engine_power_hp))) %>% 
  drop_na() %>% 
  mutate(dif = abs(vessel_power - engine_power_hp)) %>% 
  group_by(vessel_power) %>% 
  top_n(n = -1, dif) %>% 
  select(-dif) %>% 
  ungroup()

a_subset <- a %>% 
  select(fuel_type, species, engine_power_hp, L, La) %>% 
  distinct()

effort <- effort %>% 
  drop_na(engine_power_hp) %>% 
  mutate(fuel_type = "diesel") %>% 
  select(-engine_power_hp) %>% 
  inner_join(power_match, by = c("vessel_power")) %>% 
  left_join(a_subset, by = c("species", "fuel_type", "engine_power_hp")) %>% 
  mutate(group = paste(fuel_type, species, engine_power_hp ,sep = "_")) %>% 
  # filter(group %in% unique(mean_prices$group)) %>% 
  mutate(active_liters_norm = active_liters / L,
         fishing_liters_norm = fishing_liters / L,
         La_norm = La / L)

ggplot(data = mean_prices, aes(x = l, group = group)) +
  geom_line(aes(y = p_mean)) +
  geom_line(aes(y = p)) +
  geom_hline(yintercept = 21, linetype = "dashed") +
  geom_rug(data = effort, aes(x = active_liters, group = group)) +
  facet_trelliscope(~fuel_type + species + engine_power_hp, nrow = 1, ncol = 2, scales = "free", path = here::here("test"))

short_mean <- mean_p(L = 1500, Ls = 400, P = 21) %>% 
  mutate(L = l / 1000)

incentive_and_effort_by_eu  %>% 
  mutate(subsidized = subsidy_liters > 0) %>% 
  ggplot() +
  geom_rug(aes(x = liters / I, color = subsidized), alpha = 0.5) +
  # geom_rug(aes(x = fishing_liters_norm), sides = "t", color = "red", alpha = 0.5) +
  geom_hline(yintercept = 21, linetype = "dashed", size = 1) +
  geom_step(data = short_mean, aes(x = L, y = p), color = "red", size = 1) +
  geom_line(data = short_mean, aes(x = L, y = p_mean), linetype = "dotted", size = 1) +
  # geom_hline(yintercept = 19, linetype = "dashed") +
  labs(x = "Normalized liters [L used / (MDL x DPC)]",
       y = "Fuel price ($/L)") +
  # scale_y_continuous(limits = c(15, 23))  +
  # ggtitle("all fisheries") +
  # geom_vline(xintercept = 0.4, linetype = "dashed") +
  # geom_vline(xintercept = 1, linetype = "dashed") +
  scale_y_continuous(labels = c("p", "p'"), breaks = c(21, 19)) +
  scale_x_continuous(labels = c("L", "I"), breaks = c(1, 0.4), limits = c(0, NA)) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(~year, ncol = 2)

ggplot(data = effort) +
  geom_rug(aes(x = active_liters_norm), color = "steelblue", alpha = 0.5) +
  geom_rug(aes(x = fishing_liters_norm), sides = "t", color = "red", alpha = 0.5) +
  geom_step(data = short_mean, aes(x = L, y = p)) +
  geom_line(data = short_mean, aes(x = L, y = p_mean)) +
  geom_hline(yintercept = 21, linetype = "dashed") +
  geom_hline(yintercept = 19, linetype = "dashed") +
  labs(x = "Normalized liters (L used / (daily liters * days per cycle)") +
  scale_y_continuous(limits = c(15, 23)) +
  facet_wrap(~species)



effort %>% 
  head(4) %>%
  mutate(start = 0,
         fuel_type = "diesel") %>% 
  pivot_longer(cols = c(L, La, start), names_to = "variable", values_to = "value") %>% 
  mutate(p_real = ifelse(fuel_type == "diesel", 21, 23),
         p = ifelse(variable == "L", p_real, p_real - 2),
         group = paste(fuel_type, , nom_power ,sep = "_")) %>% 
  # select(group, variable, value, p, fuel_type, fishery, engine_power_hp) %>%
  ggplot() +
  geom_vline(aes(xintercept = active_liters, color = year), linetype = "dashed", size = 0.1) +
  geom_step(aes(x = value, y = p), direction = "vh", size = 1)  +
  geom_line(data = mean_prices, aes(x = l, y = p_mean)) +
  geom_hline(aes(yintercept = p), linetype = "dashed") +
  geom_vline(aes(xintercept = value), color = "red") +
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_continuous(expand = c(0, 0)) +
  facet_wrap(~fuel_type + fishery + nom_power, nrow = 1, ncol = 2, scales = "free") +
  # facet_trelliscope(~fuel_type + fishery + nom_power, nrow = 1, ncol = 2, scales = "free", path = here::here("docs")) +
  labs(x = "Liters", y = "Fuel price (MXP / Liter)")






























