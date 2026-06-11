# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# File was sent to us directly, so no online download for now


# or read from local file if already downloaded
data_raw <- readr::read_csv(here::here("data", "raw", "0076_bayer_ts_raw.csv"))

# Cleaning ----------------------------------------------------------------

#* Column Names -----------------------------------------------------------
df <- data_raw |>
  dplyr::rename(

  )





#* Misc -------------------------------------------------------------------
# recode any "NA" to proper NA
df <- df |>
  mutate(across(where(is.character), ~ na_if(., "NA"))) |>
  # same for Nan
  mutate(across(where(is.character), ~ na_if(., "NaN")))

# add beep (always 1 per day)
df <- df |>
  dplyr::mutate(beep = 1)


# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0076_bayer_ts.tsv"))
}

# export column names
write_csv(tibble(colnames(df)), here("data", "column_names", "0076_bayer_column_names.csv"), col_names = FALSE)


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(dataset_id == "0076") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0076") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0076_bayer_metadata.json"))
