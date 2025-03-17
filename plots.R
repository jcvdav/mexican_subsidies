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







