# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(jsonlite)
library(osfr)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# download file if not already downloaded
if(!file.exists(here("data", "raw", "0042_mostajabi_ts_raw.csv"))){
  osf_retrieve_file("https://osf.io/d4me2") |>
    osf_download(path = here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/d4me2") |> pull(name)
  file.rename(here("data", "raw", file_name), here("data", "raw", "0042_mostajabi_ts_raw.csv"))
}

df_raw <- read.csv(here("data", "raw", "0042_mostajabi_ts_raw.csv"))

# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
df <- df_raw |>
  janitor::clean_names() |>
  rename(
    id = participant_id
  ) |>
  rename_with(~str_remove(., "_now"), ends_with("_now"))


#* Misc -------------------------------------------------------------------
# change some columns to PosixCt
df <- df |>
  mutate(across(all_of(c(contains("begin"), contains("finish"))),
         ~ as.POSIXct(.x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")))


# remove irrelevant variables that can be computed from data
df <- df |>
  select(!c(total_ema, first_ema, pa, neg_aff, pa_interaction, neg_aff_interaction))


# remove (re-)centered columns which end on _p or _pc
df <- df |>
  select(!ends_with("_p")) |>
  select(!ends_with("_pc"))


# remove demographic information, already present in static dataset
df <- df |>
  select(!c(gender, gender_other, trans, age, handedness, contains("race"), ethnicity,
            language, language_other, sexuality, sexuality_other, marital, grade, degree, degree_other,
            employment, occupation, military_affil, income, religion, religion_other))



# create day variable
df <- df |>
 group_by(id) |>
  mutate(date = as.Date(begin_day_ema)) |>
  mutate(
    day = as.integer(date - min(date)) + 1
  ) |>
  ungroup() |>
  select(!date)


# create beep variable
df$beep <- NA



# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
metadata <- read_metadata("0042")

# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, metadata)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0042_mostajabi_ts.tsv"))
}
