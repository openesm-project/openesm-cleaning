# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
library(haven)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
df_raw <- haven::read_sav(here::here("data", "raw", "0012_dejonckheere_ts_raw.sav"))


# Cleaning ----------------------------------------------------------------

#* Column Names -----------------------------------------------------------
# Rename columns
df <- df_raw |>
  janitor::clean_names() |>
  rename(
    id = pid,
    counter = beepnum,
    angry = anger,
    stressed = stress
  )

# For each id, create a new beep column (7x/day) and a day column (14 days)
df <- df |>
  arrange(id) |>
  mutate(
    beep = rep(rep(1:7, times = 14), times = 100),
    day = rep(rep(1:14, each = 7), times = 100)
  )

#* Misc -------------------------------------------------------------------
# split off cross-sectional information
df_demographics <- df |>
  group_by(id) |>
  distinct(ces_dmean, bd_imean, era, rrs_br)

# save the demographics data
write_tsv(df_demographics, here("data", "clean", "0012_dejonckheere_static.tsv"))

# remove cross-sectional information from the main data
df <- df |>
  select(-ces_dmean, -bd_imean, -era, -rrs_br)

# remove aggregate computations from data
df <- df |>
  select(-c(pa, na))



# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0012_dejonckheere_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(dataset_id == "0012") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0012") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0012_dejonckheere_metadata.json"))
