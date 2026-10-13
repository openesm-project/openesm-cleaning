# Packages ----------------------------------------------------------------
library(tidyverse)
library(osfr)
library(here)
library(jsonlite)
source(here("scripts", "functions_data.R"))


# Data --------------------------------------------------------------------
if(!file.exists(here("data", "raw", "0002_nestler_ts_raw.txt"))){
  osf_retrieve_file("https://osf.io/gmz7e") |>
    osf_download(path = here("data", "raw"))


  # rename data to 0001_fried.csv
  file_name <- osf_retrieve_file("https://osf.io/gmz7e") |> pull(name)
  file.rename(here("data", "raw", file_name), here("data", "raw", "0002_nestler_ts_raw.txt"))
}

df <- read.table(here("data", "raw", "0002_nestler_ts_raw.txt"), header = TRUE)



# Cleaning ----------------------------------------------------------------
#* Column names ------------------------------------------------------------
df <- df |>
  janitor::clean_names() |>
  # remove all "gm_"
  rename_with(~ str_remove(., "^gm_")) |>
  rename(
    self_esteem = se
  )

#* Misc --------------------------------------------------------------------
# add beep column (although irrelevant here)
df$beep <- 1

# remove person-specific means and modeling columns
df <- df |>
  select(-starts_with("m_")) |>
  select(-c(train, last))

# split off demographic data to separate file
df_demographics <- df |>
  group_by(id) |>
  distinct(no_meas, age, sex, pa, na, swls, pow, ach, aff, int, fear)

# save demographic data
write_tsv(df_demographics, here("data", "clean", "0002_nestler_static.tsv"))

# remove demographic columns from main data
df <- df |>
  select(-c(no_meas, age, sex, pa, na, swls, pow, ach, aff, int, fear))

# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
# Enter dataset ID here
metadata <- read_metadata("0002")


# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, metadata)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0002_nestler_ts.tsv"))
}
