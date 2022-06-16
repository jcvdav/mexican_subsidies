######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

library(here)
library(cowplot)
library(tidyverse)

states <- c("Baja california",
            "Baja california sur",
            "Campeche",
            "Chiapas",
            "Nayarit",
            "Oaxaca",
            "Quintana roo",
            "Sinaloa",
            "Sonora",
            "Tamaulipas",
            "Veracruz")

prices <- readRDS(file.path(
  project_path,
  "data",
  "processed_data",
  "annual_national_diesel_prices_2011_2020.rds")) %>% 
  filter(between(year, 2012, 2019))

annual_state_diesel_prices <- 
  readRDS(
    file = file.path(
      project_path,
      "data",
      "processed_data",
      "annual_state_diesel_prices.rds"
    )
  ) %>% 
  left_join(prices %>% select(year, rate), by = "year") %>% 
  mutate(mean_diesel_price_mxn_l = rate * mean_diesel_price_mxn_l) %>% 
  filter(between(year, 2012, 2019),
         state %in% states)



ts <- ggplot(data = prices,
       mapping = aes(x = year,
                     y = mean_diesel_price_mxn_l)) + 
  geom_line(data = annual_state_diesel_prices,
            aes(x = year, y = mean_diesel_price_mxn_l, group = state),
            size = 0.1) +
  geom_line() + 
  geom_point(size = 4) +
  labs(x = "Year",
       y = bquote("Diesel price ("~MXP[2019]/L~")"))

pct_sub <- ggplot(data = prices,
                  mapping = aes(x = year, y = 2 / mean_diesel_price_mxn_l)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Year",
       y = "Percent price subsidized\n(% of market price)")


fuel_prices_plot <- plot_grid(ts,
                              pct_sub,
                              ncol = 1,
                              labels = "AUTO", label_x = 0.95)


ggsave(plot = fuel_prices_plot,
       filename = here("results", "img", "fuel_prices_plot.pdf"),
       width = 6,
       height = 6)


