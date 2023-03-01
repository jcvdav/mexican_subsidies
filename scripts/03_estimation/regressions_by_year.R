######################################################
#title#
######################################################
# 
# Run regressions by year
#
######################################################



by_year <- function(data){
  feols(log(fuel_consumption_l) ~ treated + total_hp, data = data) %>% 
    broom::tidy()
}

l_by_year <- left_pred %>% 
  group_by(year) %>% 
  nest() %>% 
  mutate(m = map(data, by_year)) %>% 
  unnest(m) %>% 
  filter(term == "treated") %>% 
  mutate(source = "left")

r_by_year <- right_pred %>% 
  group_by(year) %>% 
  nest() %>% 
  mutate(m = map(data, by_year)) %>% 
  unnest(m) %>% 
  filter(term == "treated") %>% 
  mutate(source = "right")


d <- rbind(l_by_year, r_by_year)

b_in_time <- ggplot(d, aes(x = year, y = estimate, ymin = estimate - std.error, ymax = estimate + std.error, fill = source)) +
  geom_pointrange(color = "black", shape = 21, size = 1) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Year",
       y = bquote(beta[i]),
       fill = "Side") 

ggsave(plot = b_in_time,
       filename = here("results", "img", "b_in_time.pdf"),
       width = 6,
       height = 4)