# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
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
    depressed_mood = phq2,
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
  select(
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
  group_by(id) |>
  distinct() |>
  ungroup()

# save
write_tsv(df_demographics, file = here::here("data", "clean", "0052_marian_static.tsv"))

# remove from main dataframe
df <- df |>
  select(!c(
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
  mutate(
    start_time = as.POSIXct(start_time, format = "%Y-%m-%dT%H:%M:%S%z", tz = "Europe/Bucharest"),
    end_time = as.POSIXct(end_time, format = "%Y-%m-%dT%H:%M:%S%z", tz = "Europe/Bucharest")
  )


# better order
df <- df |>
  select(id, day, beep, counter, start_time, end_time, everything())


# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0052_marian_ts.tsv"))
}

# nicer order
df <- df |>
  select(id, day, beep, counter, everything())

# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(id == "0052") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0052") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0052_marian_metadata.json"))
