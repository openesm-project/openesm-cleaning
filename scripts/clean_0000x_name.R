# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
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
metadata <- read_metadata("000X")


# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, metadata)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "000X_NAME_ts.tsv"))
}
