# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------





# Cleaning ----------------------------------------------------------------

#* Column Names -----------------------------------------------------------



#* Misc -------------------------------------------------------------------
# convert string sentinels ("NA", "", ...) and NaN to real NA
df <- recode_missing(df)


# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
# Enter dataset ID here
meta_data <- read_sheet(METADATA_URL)
dataset_info <- meta_data |>
  filter(dataset_id == "000X")
variable_data <- read_sheet(pull(dataset_info, "Coding File URL"))


# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, dataset_info, variable_data)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "000X_NAME_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
write_metadata("000X", meta_data = meta_data, variable_data = variable_data)
