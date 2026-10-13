# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(jsonlite)
library(osfr)
source(here::here("scripts", "functions_data.R"))

# remove some packages from previous dataset scripts
unloadNamespace("summarytools")
unloadNamespace("rapportools")
unloadNamespace("reshape2")
unloadNamespace("plyr")



# Data --------------------------------------------------------------------
# Read in data -----------------------------------------------------------
# State data
if(!file.exists(here::here("data", "raw", "0071_scharbert_ts_states_raw.csv"))){
  osf_retrieve_file("https://osf.io/rhu7c") |>
    osf_download(path = here::here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/rhu7c") |> pull(name)
  file.rename(here::here("data", "raw", file_name), here::here("data", "raw", "0071_scharbert_ts_states_raw.csv"))
}
df_states <- read_delim(here::here("data", "raw", "0071_scharbert_ts_states_raw.csv"),
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

# daily data
if(!file.exists(here::here("data", "raw", "0071_scharbert_ts_daily_raw.csv"))){
  osf_retrieve_file("https://osf.io/fd3z7") |>
    osf_download(path = here::here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/fd3z7") |> pull(name)
  file.rename(here::here("data", "raw", file_name), here::here("data", "raw", "0071_scharbert_ts_daily_raw.csv"))
}
df_daily <- read_delim(here::here("data", "raw", "0071_scharbert_ts_daily_raw.csv"),
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

# cross-sectional data
if(!file.exists(here::here("data", "raw", "0071_scharbert_static_raw.csv"))){
  osf_retrieve_file("https://osf.io/v3n85") |>
    osf_download(path = here::here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/v3n85") |> pull(name)
  file.rename(here::here("data", "raw", file_name), here::here("data", "raw", "0071_scharbert_static_raw.csv"))
}

# merge daily and state data
df_raw <- bind_rows(df_states, df_daily) |>
  select(-c("...1")) |>
  arrange(participant, day)

# save as csv
write_csv(df_raw, here::here("data", "raw", "0071_scharbert_ts_raw.csv"))

# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
df <- df_raw |>
  janitor::clean_names() |>
  rename(
    id = participant,
    date = day,
    angry = state_na1,
    anxious = state_na2,
    sad = state_na3,
    happy = state_pa1,
    excited = state_pa2,
    relaxed = state_pa3,
    feeling_country = prejudices_general,
    threat_country = threat_general,
    similarity_country = similarity_general
  )


#* Misc -------------------------------------------------------------------
# check for character NA
df <- df |>
  mutate(across(where(is.character), ~na_if(., "NA")))

# create day column
df <- df |>
  group_by(id) |>
  mutate(
    day = as.integer(date - min(date, na.rm = TRUE)) + 1
  ) |>
  ungroup()

# add empty beep column
df$beep <- NA

# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
metadata <- read_metadata("0071")

# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, metadata)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0071_scharbert_ts.tsv"))
}
