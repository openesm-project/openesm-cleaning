# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(jsonlite)
library(osfr)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# download data
if(!file.exists(here::here("data", "raw", "0049_pronizius_ts_raw.xlsx"))){
  osf_retrieve_file("https://osf.io/rf36u") |>
    osf_download(path = here::here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/rf36u") |> pull(name)
  file.rename(here::here("data", "raw", file_name), here::here("data", "raw", "0049_pronizius_ts_raw.xlsx"))
}

# read data
df_raw <- readxl::read_excel(here("data", "raw", "0049_pronizius_ts_raw.xlsx"))


# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
df <- df_raw |>
  janitor::clean_names() |>
  rename(
    id = participant_e,
    beep = mzp,
    counter = count,
    helping_binary = helping_bin,
    social_binary = soc_bin,
    weekday = wdwe,
    stressed = stress
  )


#* Misc -------------------------------------------------------------------
# split off demographic data
df_demographics <- df |>
  select(id, age, gender, country_name, pss, loneliness, depression, virusconcern, helpers) |>
  group_by(id) |>
  distinct()

write_tsv(df_demographics, here("data", "clean", "0049_pronizius_static.tsv"))

# split off from time series data
df <- df |>
  select(!c(age, gender, country_name, pss, loneliness, depression, virusconcern, helpers))

# nicer column order
df <- df |>
  select(id, day, beep, counter, ema_time, everything())

# recode ema time to actual hour of the day
df <- df |>
  mutate(ema_time = ema_time + 10)

# recode day of the week from numeric to actual days
df <- df |>
  mutate(weekday = case_when(
    weekday == 1 ~ "Monday",
    weekday == 2 ~ "Tuesday",
    weekday == 3 ~ "Wednesday",
    weekday == 4 ~ "Thursday",
    weekday == 5 ~ "Friday",
    weekday == 6 ~ "Saturday",
    weekday == 7 ~ "Sunday"
  ))

df <- df |>
  mutate(day = as.numeric(day))

# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
metadata <- read_metadata("0049")

# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, metadata)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0049_pronizius_ts.tsv"))
}
