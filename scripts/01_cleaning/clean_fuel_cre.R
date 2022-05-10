######################################################
#title#
######################################################
# 
# Clean mean fuel prcies from DatosAbiertos
# https://datos.gob.mx/busca/dataset/ubicacion-de-gasolineras-y-precios-comerciales-de-gasolina-y-diesel-por-estacion
# Email del publicador	jcuellar@cre.gob.mx
# Frecuencia	R/P1D
# GUID	ubicacion-de-gasolineras-y-precios-comerciales-de-gasolina-y-diesel-por-estacion
# Idioma	es
# Inicio del periodo temporal	2017-03-23T00:00:00Z
# Final del periodo temporal	2017-12-06T00:00:00Z
# Modificado	2017-04-11T00:00:00Z
# Nombre del publicador	CRE
# Publicado	2017-10-20T16:53:59Z
# Tema	Energía Y Medio Ambiente
# Tipo del publicador	Javier Cuellar
# Diccionario de Datos	Consultar
#
# Description: Muestra los precios promedio diarios y los precios promedio mensuales de gasolinas y diésel
# (Joint with Histórico de número de estaciones de servicio (gasolineras) por municipio)
######################################################


library(janitor)
library(tidyverse)

cre_fuel_prices <- read_csv(
  file = file.path(
    project_path,
    "data",
    "raw_data",
    "fuel_prices_CRE",
    "PreciosPromedioMensuales.csv"
    ), skip = 1
  ) %>% 
  clean_names()

# Select data
# Monthly state-level prices
monthly_state_diesel_prices <- cre_fuel_prices %>% 
  select(state = entidad_federativa_11,
         diesel_price_mxn_l = disel_12,
         year = ao_reporte_13,
         month = mes_14) %>% 
  mutate(date = lubridate::ymd(paste(year, month, "15", sep = "-"))) %>% 
  select(state, year, month, date, diesel_price_mxn_l) %>% 
  filter(year <= 2020)

# Daily mean prices
daily_national_diesel_prices <- cre_fuel_prices %>% 
  select(date = fecha_calendario,
         diesel_price_mxn_l = disel_19) %>% 
  drop_na(date) %>% 
  mutate(date = lubridate::dmy(date),
         year = lubridate::year(date),
         month = lubridate::month(date)) %>% 
  filter(year <= 2020)

# Annual state mean
annual_state_diesel_prices <- monthly_state_diesel_prices %>% 
  group_by(state, year) %>% 
  summarize(mean_diesel_price_mxn_l = mean(diesel_price_mxn_l))

# Annual national
annual_national_diesel_prices <- daily_national_diesel_prices %>% 
  group_by(year) %>% 
  summarize(mean_diesel_price_mxn_l = mean(diesel_price_mxn_l))


# EXPORT DATA
saveRDS(
  object = monthly_state_diesel_prices,
  file = file.path(
    project_path,
    "data",
    "processed_data",
    "monthly_state_diesel_prices.rds"
  )
)

saveRDS(
  object = daily_national_diesel_prices,
  file = file.path(
    project_path,
    "data",
    "processed_data",
    "daily_national_diesel_prices.rds"
  )
)

saveRDS(
  object = annual_state_diesel_prices,
  file = file.path(
    project_path,
    "data",
    "processed_data",
    "annual_state_diesel_prices.rds"
  )
)

saveRDS(
  object = annual_national_diesel_prices,
  file = file.path(
    project_path,
    "data",
    "processed_data",
    "annual_national_diesel_prices.rds"
  )
)


  