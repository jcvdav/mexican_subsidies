library(startR)
library(tidyverse)


####### RESPONSIVENESS

data <- tibble(id = LETTERS[1:n],
       slope = c(-5, -60),
       int = 800) %>% 
  expand_grid(price_tbl) %>% 
  mutate(pl = ph - 4,
         qh = int + (slope * ph),
         ql = int + (slope * pl))


(
  p <- ggplot(data = data) +
    geom_abline(aes(intercept = -int / slope, slope = 1/slope, color = id)) +
    geom_hline(yintercept = p_market) +
    geom_segment(aes(x = qh, xend = qh, y = 0, yend = ph), linetype = "dashed") +
    geom_point(aes(x = qh, y = ph), fill = "black") +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 900), breaks = NULL, labels = NULL) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 14), breaks = NULL, labels = NULL) +
    labs(x = "Fuel Consumption (L)", y = "Fuel Price ($ / L)") +
    scale_color_brewer(palette = "Set1") +
    theme(legend.position = "None")
)

(
  p2 <- p +
    geom_hline(yintercept = p_market - 4) +
    geom_point(aes(x = ql, y = pl), fill = "black") +
    geom_segment(aes(x = ql, xend = ql, y = 0, yend = pl), linetype = "dashed")
)


ggsave(plot = p,
       filename = file.path(project_path, "results", "figures", "response1.png"),
       width = 5,
       height = 3,
       units = "in")

ggsave(plot = p2,
       filename = file.path(project_path, "results", "figures", "response2.png"),
       width = 5,
       height = 3,
       units = "in")


########### PROGRAM DESIGN


pd_data <- tibble(ph = 10) %>% 
  mutate(slope =  -60,
         int = 800) %>% 
  mutate(pl = ph - 4,
         qh = int + (slope * ph),
         ql = int + (slope * pl))

program <- tibble(x = 1:900) %>% 
  mutate(y = ifelse(x < 212, 0 + x / 25, 8.5))


(
  pd <- ggplot(data = pd_data) +
    geom_abline(aes(intercept = -int / slope, slope = 1/slope), color = "steelblue") +
    geom_hline(yintercept = 10) +
    geom_segment(aes(x = qh, xend = qh, y = 0, yend = ph), linetype = "dashed") +
    geom_point(aes(x = qh, y = ph), fill = "black") +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 900), breaks = NULL, labels = NULL) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 15), breaks = NULL, labels = NULL) +
    labs(x = "Fuel Consumption (L)", y = "Fuel Price ($ / L)") +
    scale_color_brewer(palette = "Set1") +
    theme(legend.position = "None") +
    geom_text(x = 100, y = 9.5, label = "Market price")
)


(
  pd2 <- pd +
    geom_hline(yintercept = 10 - 4) +
    geom_point(aes(x = ql, y = pl), fill = "black") +
    geom_segment(aes(x = ql, xend = ql, y = 0, yend = pl), linetype = "dashed") +
    geom_segment(aes(x = qh, xend = ql, y = 1, yend = 1),
                 arrow = arrow(length = unit(10, "pt")),
                 color = "red") +
    geom_text(x = 100, y = 5.5, label = "Subsidized price") +
    geom_text(x = 300, y = 1.5, label = "Additional effort")
)

ggsave(plot = pd,
       filename = file.path(project_path, "results", "figures", "program_design1.png"),
       width = 5,
       height = 3,
       units = "in")

ggsave(plot = pd2,
       filename = file.path(project_path, "results", "figures", "program_design2.png"),
       width = 5,
       height = 3,
       units = "in")












  
  
  
  