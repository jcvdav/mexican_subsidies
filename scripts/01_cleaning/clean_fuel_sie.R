######################################################
#title#
######################################################
# 
# Combines SIE data
# https://sie.energia.gob.mx/bdiController.do?action=cuadro&subAction=applyOptions
######################################################

library(readxl)


str_2011_2015 <- read_excel("/Users/juancarlosvillasenorderbez/Downloads/anual_nacional_2011_2015.xls",
                            skip = 8) %>% 
  select(1:6) %>% 
  magrittr::set_colnames(value = c("variable", 2011:2015)) %>% 
  filter(variable == "Pemex Diesel (1)(11)") %>% 
  select(-variable) %>% 
  mutate_all(as.numeric) %>% 
  pivot_longer(cols = c(1:5), names_to = "year", values_to = "mean_diesel_price_mxn_l") %>% 
  mutate(mean_diesel_price_mxn_l = mean_diesel_price_mxn_l / 1e3)


str_2016 <- read_excel("/Users/juancarlosvillasenorderbez/Downloads/anual_nacional_2016.xls",
                            skip = 7) %>% 
  magrittr::set_colnames(value = c("variable", 2016)) %>% 
  filter(variable == "Pmax") %>% 
  tail(1) %>%                                                                    # Diuesel is at the end
  select(-variable) %>% 
  pivot_longer(cols = 1, names_to = "year", values_to = "mean_diesel_price_mxn_l")



nat_2017_2020 <- readRDS(
  file.path(
    project_path,
    "data",
    "processed_data",
    "annual_national_diesel_prices.rds")) %>% 
  mutate()

stat_2017_2020 <- readRDS(file.path(
  project_path,
  "data",
  "processed_data",
  "annual_state_diesel_prices.rds")) %>% 
  mutate()

annual_national_diesel_prices_2011_2020 <- rbind(str_2011_2015, str_2016, nat_2017_2020) %>% 
  mutate(year = as.integer(year))

saveRDS(object = annual_national_diesel_prices_2011_2020,
        file = file.path(
          project_path,
          "data",
          "processed_data",
          "annual_national_diesel_prices_2011_2020.rds"))


ggplot(annual_national_diesel_prices_2011_2020, aes(x = year, y = mean_diesel_price_mxn_l)) + 
  geom_point()





#Combien SIE data from Valor de las ventas internas de petrolíferos por entidad federativa
# https://sie.energia.gob.mx/bdiController.do?action=cuadro&subAction=applyOptions

vol <- read_excel(path = file.path(project_path, "data", "processed_data", "SIE_volumen_ventas_estado.xlsx"),
                  sheet = 2, na = "N/D") %>% 
  select(-Fuente) %>% 
  rename(state = Estado, variable = Variable) %>% 
  filter(str_detect(variable, "Diesel")) %>% 
  drop_na() %>% 
  mutate(state = str_to_sentence(state)) %>% 
  pivot_longer(cols = c(3:11), names_to = "year", values_to = "volume") %>% 
  group_by(year, state) %>% 
  summarize(volume = sum(volume, na.rm = T))
  

val <- read_excel(path = file.path(project_path, "data", "processed_data", "SIE_volumen_ventas_estado.xlsx"),
                  sheet = 3, na = "N/D") %>% 
  filter(str_detect(Variable, "Diesel")) %>% 
  rename(state = Estado, variable = Variable) %>% 
  filter(str_detect(variable, "Diesel")) %>% 
  drop_na() %>% 
  mutate(state = str_to_sentence(state)) %>% 
  pivot_longer(cols = c(3:11), names_to = "year", values_to = "value") %>% 
  group_by(year, state) %>% 
  summarize(value = sum(value, na.rm = T))


p <- left_join(vol, val, by = c("state", "year")) %>% 
  mutate(year = as.integer(year),
         p = value / (volume)) %>% 
  select(year, state, p_vv = p)



ggplot() +
  geom_line(data = p, aes(x = year, y = p, group = state), color = "red") +
  geom_line(data = stat_2017_2020, aes(x = year, y = mean_diesel_price_mxn_l, group = state), color = "blue")


stat_2017_2020 %>% 
  ungroup() %>% 
  mutate(state = str_to_sentence(state)) %>% 
  filter(year <= 2019) %>% 
  left_join(p, by = c("year", "state")) %>% 
  mutate(a = mean_diesel_price_mxn_l / p) %>% 
  select(year, a) %>% 
  drop_na(a) %>% 
  arrange(year) %>% View()

7416 + 298.8 + 1221.963607 - 2566.4675 + 98.56044983 + 529.1753552

5.959 + 1.2 + 4.7875 + 4.58 + 0 + 0.2075 + 2.339166667


