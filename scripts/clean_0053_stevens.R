# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
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

# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0053_stevens_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(dataset_id == "0053") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0053") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0053_stevens_metadata.json"))
