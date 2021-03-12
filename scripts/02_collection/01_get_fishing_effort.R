# Download fishing effort

# Load packages
library(connections)
library(bigrquery)
library(tidyverse)

bq_auth("juancarlos@ucsb.edu")

# Establish a connection to BigQuery
con <- connection_open(
  bigquery(),
  project = "world-fishing-827",
  dataset = "pipe_mexico_production_v20190128",
  billing = "emlab-gcp",
  use_legacy_sql = FALSE,
  allowLargeResults = TRUE
)

con2 <- connection_open(
  bigquery(),
  project = "world-fishing-827",
  dataset = "gfw_research",
  billing = "emlab-gcp",
  use_legacy_sql = FALSE,
  allowLargeResults = TRUE
)

# Query the main vessel activity table and create a gridded version
effort_query <- tbl(con, "messages_scored_*") %>% 
  filter(nnet_score == 1) %>%
  mutate(
    year = sql("EXTRACT(YEAR FROM timestamp)"),               # Extract the year from the date
    # lat = (floor(lat / 0.1) * 0.1 + 0.05),                    # Grid latitude
    # lon = (floor(lon / 0.1) * 0.1 + 0.05),                    # Grid longitude
    hours = 1                                                 # VMS pings every hour, so this is our best estimate of time spent
  ) %>%
  group_by(ssvid, year, rnp) %>% 
  summarize(hours = sum(hours, na.rm = T)) %>% 
  ungroup()


mxn_vessels <- tbl(con2, "vi_ssvid_byyear_v20210301") %>% 
  # bq_unnest(select_columns = c("year", "best", "ssvid"),
  #           array_column = c("activity.fishing_hours"),
  #           unnested_columns = "value") %>% 
  filter(best$best_flag == "MEX",
         on_fishing_list_best) %>% 
  mutate(fishing_hours = activity$fishing_hours) %>% 
  select(ssvid, year, fishing_hours) %>% 
  collect()

# At the time of running this, the collection isn't working because of a BQAPI-side error:
# >>>>>>> Invalid value at 'start_index' (TYPE_UINT64), "1e+05" [invalid]
# A quick hack suggested by Hadley is linked here:
# https://github.com/r-dbi/bigrquery/issues/395#issuecomment-666514531
# And I'll run it below.

options(scipen = 20)                                          # Fix index error

# Collect the query
effort <- effort_query %>%
  collect()                                                   # Force computation of the query (actually querying now)


# Export the data
saveRDS(object = effort,
        file = here("data", "vessel_anual_effort.rds"))

saveRDS(object = mxn_vessels,
        file = here("data", "vessel_anual_effort_AIS.rds"))


## END OF SCRIPT ##



