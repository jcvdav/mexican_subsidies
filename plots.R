tribble(~"HP", ~"MDL",
        40,	215,
        50,	250,
        60,	325,
        70,	350,
        80,	390,
        85,	460,
        90,	480,
        100,	545,
        110,	590,
        125,	630,
        150,	715,
        175,	880,
        185,	880,
        200,	985,
        225,	1075,
        250,	1155,
        275,	1290,
        300,	1425,
        325,	1525,
        350,	1560,
        365,	1618,
        370,	1640,
        375,	1660,
        400,	1775,
        402,	1857,
        425,	1975,
        450,	2090,
        475,	2200,
        500,	2380,
        525,	2530,
        550,	2660,
        575,	2735,
        600,	2795,
        625,	2895,
        650,	2975,
        675,	3050,
        700,	3225,
        750,	3880,
        800,	3995,
        825,	4010,
        850,	4050,
        875,	4130,
        900,	4175,
        1050,	4875,
        1150,	5330,
        1300,	5495,
        1350,	5856,
        2200,	9480,
        2400,	10440,
        2600,	11310,
        2875,	12290,
        3000,	12985,
        3300,	14470,
        3600,	15455) %>% 
    ggplot(aes(x = HP,
               y = MDL)) + 
  geom_step(direction = "hv")

effect_plot <- function(data, var = hours, n = 1){
  # browser()
  fill <- palette.colors(n = 3, palette = "Set 2")[n]
  
  plot_data <- data %>% 
    mutate(treatment = ifelse(treated == 1,
                              "Subsidized",
                              "Not subsidized"),
           subsidy_frequency = str_to_sentence(subsidy_frequency),
           subsidy_frequency = fct_relevel(subsidy_frequency, "Never", "Sometimes", "Always"))
  
  vals <- plot_data %>% 
    filter(subsidy_frequency == "Sometimes") %>%
    group_by(treatment) %>%
    summarize(var = mean(log({{var}})), .groups = "drop") %>% 
    mutate(a = diff(var) / var)
  
  num <- vals %>% 
    pull(a) %>% 
    head(1) %>% 
    scales::percent(accuracy = 0.01)
  
  height <- vals %>% 
    pull(var) %>% 
    max()
  
  
  pos <- position_jitter(width = 0.5,
                         height = 0,
                         seed = 1)
  
  ggplot(data = plot_data,
         mapping = aes(x = treatment,
                       y = log({{var}}),
                       shape = subsidy_frequency)) + 
    stat_summary(geom = "linerange",
                 fun.data = mean_cl_normal,
                 color = fill,
                 linewidth = 1,
                 position = pos) +
    stat_summary(geom = "line",
                 fun = mean,
                 color = fill,
                 linetype = "dashed",
                 aes(group = subsidy_frequency),
                 position = pos) +
    stat_summary(geom = "pointrange",
                 fun.data = mean_se,
                 size = 3,
                 linewidth = 1.5,
                 fatten = 1,
                 fill = fill,
                 position = pos) +
    labs(x = "Status") +
    theme(legend.position = "None") +
    scale_shape_manual(values = c(22, 21, 23)) +
    annotate(x = 1.5,
             y = height,
             geom = "text",
             label = paste0(num, " change"),
             color = fill)
}

hrs <- effect_plot(data = shrimp_panel,
                   var = hours) +
  labs(title = "Fishing time (hours)",
       y = "log(time)")

area <- effect_plot(data = shrimp_panel %>% 
                      filter(!is.na(fg_area_km),
                             fg_area_km > 0),
                    var = fg_area_km,
                    n = 2) +
  labs(title = "Fishing area (km2)",
       y = "log(area)")

landings <- effect_plot(data = shrimp_panel %>% 
                          filter(!is.na(landed_weight),
                                 landed_weight > 0),
                        var = landed_weight,
                        n = 3) +
  labs(title = "Landings (Kg)",
       y = "log(landings)")

leg <- cowplot::get_legend(
  landings +
    theme(legend.position = "bottom") +
    guides(shape = guide_legend(title = "Sub-sample",
                                override.aes = list(size = 1,
                                                    fill = "transparent")),
           linetype = "none")
)

p1 <- cowplot::plot_grid(hrs, area, landings,
                         ncol = 3,
                         align = "hv")
p2 <- cowplot::plot_grid(p1, leg, ncol = 1,
                         rel_heights = c(1, 0.2))


ggsave(plot = p2,
       filename = here("results", "img", "fig_enter.pdf"),
       width = 7,
       height = 3.5,
       units = "in")

elasticity_plot <- function(data, var) {
  ggplot(data %>% 
           filter(treated == 1,
                  n_times_sub >= 2),
         aes(x = log(subsidy_pesos),
             y = log({{var}}))) +
    geom_smooth(method = "lm") +
    geom_smooth(method = "lm",
                aes(group = eu),
                color = "red",
                linewidth = 0.1,
                se = F) +
    geom_point(color = "black",
               fill = "black",
               alpha = 0.1)
}

el_hrs <- elasticity_plot(data = shrimp_panel,
                   var = hours) +
  labs(title = "Fishing time")

el_area <- elasticity_plot(data = shrimp_panel,
                    var = fg_area_km) +
  labs(title = "Fishing area")

el_landings <- elasticity_plot(data = shrimp_panel,
                        var = landed_weight) +
  labs(title = "Landings")

cowplot::plot_grid(el_hrs, el_area, el_landings, ncol = 3)







