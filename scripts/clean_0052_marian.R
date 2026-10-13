# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(osfr)
library(jsonlite)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# read in data
# load data
if(!file.exists(here::here("data", "raw", "0052_marian_ts_raw.csv"))){
  osf_retrieve_file("https://osf.io/bnemp") |>
    osf_download(path = here::here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/bnemp") |> pull(name)
  file.rename(here::here("data", "raw", file_name), here::here("data", "raw", "0052_marian_ts_raw.csv"))
}

# read data
df_raw <- read.csv(here("data", "raw", "0052_marian_ts_raw.csv"))


# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
df <- df_raw |>
  janitor::clean_names() |>
  rename(
    counter = obs_nr,
    day = day_no,
    beep = tp_nr,
    start_time = start,
    end_time = end,
    completion_time = seconds,
    # anxiety questions (GAD)
    worry = gad_q1,
    difficulty_control_worry = gad_q2,
    restless = gad_q3,
    fatiguability = gad_q4,
    difficulty_concentrating = gad_q5,
    irritable = gad_q6,
    muscle_tension = gad_q7,
    sleep_difficulty = gad_q8,

    # depression questions (PHQ)
    anhedonia = phq1,
    depressed = phq2,
    sleep_problems = phq3,
    tired = phq4,
    appetite_change = phq5,
    worthlessness = phq6,
    concentration_trouble = phq7,
    psychomotor_change = phq8,
    suicidal_ideation = phq9,

    # negative thoughts
    something_wrong = thought1,
    cant_go_on = thought2,
    dont_like_self = thought3,
    no_good = thought4,
    nobody_likes = thought5,
    alone = thought6,
    fear_sickness = thought7,
    awful_happen = thought8,
    life_not_worth = thought9,
    nobody_cares = thought10,
    cant_stand = thought11,
    never_make_it = thought12,
    need_change = thought13
  )


#* Misc -------------------------------------------------------------------
# recode all IDs to numeric because they had different formats
df <- df |>
  mutate(id = as.numeric(as.factor(id)))

# remove unclear and/or unnecessary columns
df <- df |>
  select(!c(row_number,
            # aggregate columns
            resp_per_day, gad, phq))

# split off demographic columns
# contains one duplicate for 12268 due to waking_hours
# don't remove that for now
df_demographics <- df |>
  dplyr::select(
    c(
      id,
      sex,
      age,
      education,
      contains("_demo"),
      contains("occupation"),
      rural_urban,
      marital,
      waking_hour
    )
  ) |>
  dplyr::group_by(id) |>
  dplyr::distinct() |>
  dplyr::ungroup()

# save
write_tsv(df_demographics, file = here::here("data", "clean", "0052_marian_static.tsv"))

# remove from main dataframe
df <- df |>
  dplyr::select(!c(
    sex,
    age,
    education,
    contains("_demo"),
    contains("occupation"),
    rural_urban,
    marital,
    waking_hour
  ))


# format time columns
df <- df |>
  dplyr::mutate(
    start_time = as.POSIXct(start_time, format = "%Y-%m-%dT%H:%M:%S%z", tz = "Europe/Bucharest"),
    end_time = as.POSIXct(end_time, format = "%Y-%m-%dT%H:%M:%S%z", tz = "Europe/Bucharest")
  )


# better order
df <- df |>
  dplyr::select(id, day, beep, counter, start_time, end_time, everything())

# nicer order
df <- df |>
  dplyr::select(id, day, beep, counter, everything())

# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
metadata <- read_metadata("0052")

# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, metadata)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0052_marian_ts.tsv"))
}
