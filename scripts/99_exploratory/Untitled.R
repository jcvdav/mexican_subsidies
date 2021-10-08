

library(tidyverse)


theme_update(
  axis.title.y = element_text(hjust = 1),
  axis.title.x = element_text(hjust = 1)
)

p <- 30 * 1e3
q <- 1e-9
X <- 1e7

c <- 23
c_low <- 23 - 2
beta <- 1.3

L_fun<- function(p, q, X, c, beta){
  ((p*q*X)/(beta * c)) ^ (1 / (beta - 1))
}

# One demand curve

demand_curve <- tibble(c = 15:27) %>% 
  mutate(L = L_fun(p, q, X, c, beta)) %>% 
  ggplot(aes(x = L, y = c)) +
  geom_line() +
  labs(x = "Fuel consumption (L)",
       y = "Fuel price ($/L)") +
  geom_segment(x = 0, xend = L_fun(p, q, X, c = c, beta),
               y = c, yend = c,
               linetype = "dashed",
               color = "black") +
  geom_segment(x = L_fun(p, q, X, c = c, beta), xend = L_fun(p, q, X, c = c, beta),
               y = 0, yend = c,
               linetype = "dashed",
               color = "black") +
  geom_segment(x = 0, xend = L_fun(p, q, X, c = c_low, beta),
               y = c_low, yend = c_low,
               linetype = "dotted",
               color = "black") +
  geom_segment(x = L_fun(p, q, X, c = c_low, beta), xend = L_fun(p, q, X, c = c_low, beta),
               y = 0, yend = c_low,
               linetype = "dotted",
               color = "black")

startR::lazy_ggsave(plot = demand_curve,
                    filename = "vanilla_demand_curve",
                    width = 10,
                    height = 10)

# Two demand curves

demand_curves <- expand_grid(c = 15:27,
                             q = c(0.75, 1.25) * q) %>% 
  mutate(vessel = ifelse(q == min(q), "vessel B", "vessel A"),
         L = L_fun(p, q, X, c, beta)) %>% 
  ggplot(aes(x = L, y = c, color = vessel, label = vessel)) +
  geom_segment(x = 0, xend = L_fun(p, 1.25 * q, X, c = c, beta),
               y = c, yend = c,
               linetype = "dashed",
               color = "black") +
  geom_segment(x = L_fun(p, 1.25 * q, X, c = c, beta), xend = L_fun(p, 1.25 * q, X, c = c, beta),
               y = 0, yend = c,
               linetype = "dashed",
               color = "red") +
  geom_segment(x = L_fun(p, 0.75 * q, X, c = c, beta), xend = L_fun(p, 0.75 * q, X, c = c, beta),
               y = 0, yend = c,
               linetype = "dashed",
               color = "steelblue") +
  geom_segment(x = 0, xend = L_fun(p, 1.25 * q, X, c = c_low, beta),
               y = c_low, yend = c_low,
               linetype = "dashed",
               color = "black") +
  geom_segment(x = L_fun(p, 1.25 * q, X, c = c_low, beta), xend = L_fun(p, 1.25 * q, X, c = c_low, beta),
               y = 0, yend = c_low,
               linetype = "dashed",
               color = "red") +
  geom_segment(x = L_fun(p, 0.75 * q, X, c = c_low, beta), xend = L_fun(p, 0.75 * q, X, c = c_low, beta),
               y = 0, yend = c_low,
               linetype = "dashed",
               color = "steelblue") +
  geom_line(size = 1) +
  labs(x = "Fuel consumption (L)",
       y = "Fuel price ($/L)") +
  theme(legend.justification = c(1, 1),
        legend.position = c(1, 1)) +
  scale_x_continuous(limits = c(0, NA), expand = c(0, 0)) +
  scale_color_brewer(palette = "Set1") +
  guides(color = guide_legend(title = "Vessel"))

startR::lazy_ggsave(plot = demand_curves,
                    filename = "vanilla_demand_curves",
                    width = 10,
                    height = 10)

# MDL and DPC as a function of engine power
dpc <- read.csv(file.path(project_path, "raw_data", "days_per_cycle.csv")) %>% 
  filter(year == 2019,
         fuel_type == "diesel")
mdl <- read.csv(file.path(project_path, "raw_data", "maximum_daily_liters.csv")) %>% 
  filter(fuel_type == "diesel",
         year == 2019)

mdl_plot <- ggplot(mdl, aes(x = engine_power_hp, y = maximum_daily_liters)) +
  geom_step(direction = "hv") +
  labs(x = "Engine power (HP)",
       y = "Maximum Daily Liters")

startR::lazy_ggsave(plot = mdl_plot,
                    filename = "mdl_plot_2019",
                    width = 10,
                    height = 6)

# Kink in price

tibble(c = 15:27) %>% 
  mutate(L = L_fun(p, q, X, c, beta)) %>% 
  ggplot(aes(x = L, y = c)) +
  geom_line() +
  geom_segment(x = 0, xend = Inf,
               y = c_low, yend = c_low) +
  geom_segment(x = 0, xend = Inf,
               y = c, yend = c)


























