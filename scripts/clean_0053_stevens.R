# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(jsonlite)
library(osfr)
library(haven)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# read in data
if(!file.exists(here::here("data", "raw", "0053_stevens_ts_raw.sav"))){
  osf_retrieve_file("https://osf.io/ehks7") |>
    osf_download(path = here::here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/ehks7") |> pull(name)
  file.rename(here::here("data", "raw", file_name), here::here("data", "raw", "0053_stevens_ts_raw.sav"))
}

# read data
df_raw <- haven::read_spss(here("data", "raw", "0053_stevens_ts_raw.sav"))



# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
df <- df_raw |>
  janitor::clean_names() |>
  rename(
    id = participant_id,
    counter = notification_no,
    hour = day_hour,
    weekend = day_type,
    response_lag = resp_lag_minutes,
    response_time = resp_time_minutes,
    triple_exposure = triple,
    weight_satisfaction = weightsat,
    appearance_satisfaction = appearsat,
    shape_satisfaction = shapesat
  )


#* Misc -------------------------------------------------------------------
# remove lagged columns and unnecessary columns
df <- df |>
  select(!c(ends_with("_1"))) |>
  select(!c(master_list, enough_cases, prompt_lag_hours))

# split off demographic variables
df_demographics <- df |>
  select(c(
    id, age, sex_mf, race_wa, race_wo, bmi, ed_ny, prompts_received, prompts_completed
  )) |>
  group_by(id) |>
  distinct() |>
  ungroup()

# save demographics
write_tsv(df_demographics, here("data", "clean", "0053_stevens_static.tsv"))

# remove from main df
df <- df |>
  select(!c(age, sex_mf, race_wa, race_wo, bmi, ed_ny, prompts_received, prompts_completed))

# create empty day and beep columns
df$beep <- NA
df$day <- NA

# better order
df <- df |>
  select(id, counter,beep, day, hour, weekend, everything())

# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
metadata <- read_metadata("0053")

# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, metadata)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0053_stevens_ts.tsv"))
}
