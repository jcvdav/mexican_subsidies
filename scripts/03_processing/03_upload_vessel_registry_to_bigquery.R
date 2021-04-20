######################################################
#         upload vessel registry to bigquery         #
######################################################
# Load packages
library(bigrquery)

# Load data
vessel_registry <- readRDS(here("data", "vessel_registry.rds"))

# Upload to BigQuery ##########################################################################

bq_auth("juancarlos@ucsb.edu")                                # Authenticate using local token

# Create dataset
ds <- bq_dataset(project = "emlab-gcp",                       # Define project
                 dataset = "jc_mxn_subsidies")                # Define dataset

bq_vessel_registry <- bq_table(ds, "vessel_registry")         # Create a table in BQ
bq_perform_upload(bq_vessel_registry,                         # Upload the data
                  vessel_registry)
bq_table_exists(bq_vessel_registry)                           # Was the upload succesful?

# END OF SCRIPT ################################################################################