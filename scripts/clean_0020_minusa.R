# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(jsonlite)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------





# Cleaning ----------------------------------------------------------------

#* Column Names -----------------------------------------------------------



#* Misc -------------------------------------------------------------------


# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
metadata <- read_metadata("0020")


# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, metadata)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0020_minusa_ts.tsv"))
}
