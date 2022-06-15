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


# Read data

monthly_state_diesel_prices <-
  readRDS(
    file = file.path(
      project_path,
      "data",
      "processed_data",
      "monthly_state_diesel_prices.rds"
    )
  )

daily_national_diesel_prices <-
  readRDS(
    file = file.path(
      project_path,
      "data",
      "processed_data",
      "daily_national_diesel_prices.rds"
    )
  )

annual_state_diesel_prices <- 
  readRDS(
    file = file.path(
      project_path,
      "data",
      "processed_data",
      "annual_state_diesel_prices.rds"
    )
  )

annual_national_diesel_prices <- 
  readRDS(
    file = file.path(
      project_path,
      "data",
      "processed_data",
      "annual_national_diesel_prices.rds"
    )
  )


# Plot

ts <- ggplot() + 
  geom_line(data = monthly_state_diesel_prices,
            aes(x = date, y = diesel_price_mxn_l,
                group = state),
            size = 1,
            alpha = 0.1) + 
  geom_line(data = daily_national_diesel_prices,
            aes(x = date, y = diesel_price_mxn_l),
            color = "steelblue") + 
  geom_point(data = annual_national_diesel_prices,
             aes(x = lubridate::ymd(paste(year, "6", "1")), y = mean_diesel_price_mxn_l),
             fill = "steelblue",
             shape = 21,
             size = 4) +
  geom_jitter(data = annual_state_diesel_prices,
              aes(x = lubridate::ymd(paste(year, "6", "1")), y = mean_diesel_price_mxn_l),
              size = 1,
              alpha = 0.5,
              shape = 20,
              height = 0,
              width = 10) +
  labs(x = "Year",
       y = "Diesel price (MXP / L)",
       subtitle = "Each faded black line is a monthly state\nAnnual means are centered on June 1")


pct_sub <- ggplot(data = annual_national_diesel_prices,
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
       height = 4.5)
