######################################################
#title#
######################################################
# 
# Purpose
#
######################################################

library(here)
library(tidyverse)

p <- tibble(Q = 0:10,
            P1 = 20 - (3 * Q),
            P2 = 10 - (3 * Q)) %>% 
  pivot_longer(cols = c(P1, P2),
               names_to = "vessel",
               values_to = "P") %>% 
  filter(P > 0) %>% 
  ggplot() +
  geom_line(aes(x = Q, y = P, color = vessel), size = 1) +
  geom_segment(x = 0, xend = 10, y = 8, yend = 8, linetype = "dashed", size = 0.5) +
  geom_segment(x = 0, xend = 3, y = 3, yend = 3, size = 0.5) +
  geom_segment(x = 3, xend = 10, y = 8, yend = 8, size = 0.5) +
  geom_segment(x = 3, xend = 3, y = 3, yend = 8, size = 0.5) +
  geom_segment(x = 2/3, xend = 2/3, y = 0, yend = 8, linetype = "dashed", size = 0.5, color = "steelblue") +
  geom_segment(x = 7/3, xend = 7/3, y = 0, yend = 3, size = 0.5, color = "steelblue") +
  geom_segment(x = 4, xend = 4, y = 0, yend = 8, size = 0.5, color = "red") +
  scale_x_continuous(expand = c(0, 0),
                     breaks = c(2/3, 7/3, 3, 4),
                     labels = c(expression(Q[i]),
                                expression(Q[i]^"*"),
                                expression(bar(Q[i])),
                                expression(Q[i]))) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = c(8, 3),
                     labels = c(expression(P[mkt]),
                                expression(P^"*"))) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "None") +
  labs(x = "Fuel consumption (L)",
       y = "Fuel price ($/L)")



ggsave(plot = p,
       filename = here("results", "img", "vanilla_kink.pdf"),
       width = 6,
       height = 4.5)
