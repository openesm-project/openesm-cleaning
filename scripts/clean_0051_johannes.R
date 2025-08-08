# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
library(osfr)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
if(!file.exists(here("data", "raw", "0051_johannes_ts_raw.csv"))){
  osf_retrieve_file("https://osf.io/wj2ne") |>
    osf_download(path = here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/wj2ne") |> pull(name)
  file.rename(here("data", "raw", file_name), here("data", "raw", "0051_johannes_ts_raw.csv"))
}

# read in data
df_raw <- read.csv(here("data", "raw", "0051_johannes_ts_raw.csv"))

# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
# Rename columns to snake_case
df <- df_raw |>
  janitor::clean_names() |>
  # remove "well_being_" as it is redundant
  rename_with(~ str_remove(., "well_being_")) |>
  rename(
    id = pp,
    date = day_probe,
    time_probe = time_probe,
    day = day_number,
    beep = number_probe_day,
    counter = number_probe_overall,
    probe_duration = form_duration,
    phone_use = monitoring_d_seconds,
    social_media_use = monitoring_c_seconds,
    social_media_use_no_notification = monitoring_b_seconds,
    reactibility_survey = reactibility_c_seconds,
    reactibility_notification_avg = reactibility_b_avg_seconds,
    reactibility_notification_sd = reactibility_b_std_seconds,
    reactibility_social_notification_avg = reactibility_a_avg_seconds,
    reactibility_social_notification_sd = reactibility_a_std_seconds,
    concentrated_activity_binary = conflict_yes_no,
    concentrated_activity_alone = conflict_filler,
    concentration_extent = conflict_extent,
    social_pressure_phone = social_pressure,
    scheduled_time = extracted_scheduled_times
  ) |>
  # use prompt instead of probe
  rename_with(~ str_replace(., "probe", "prompt"))


#* Misc -------------------------------------------------------------------
# remove columnns before their transformation to seconds
df <- df |>
  select(!c(starts_with("monitoring"),
            reactibility_a_avg, reactibility_a_std,
            reactibility_b_avg, reactibility_b_std,
            reactibility_c))


# convert time columns to PosixCt etc.
df <- df |>
  mutate(date = as.Date(date)) |>
  mutate(time_prompt = as.POSIXct(paste(date, time_prompt))) |>
  mutate(time_survey = as.POSIXct(paste(date, time_survey))) |>
  mutate(scheduled_time = as.POSIXct(scheduled_time))


# mutate character NA to proper NA
df <- df |>
  mutate(across(where(is.character),
                ~ na_if(., "NA")))

# recode binary to numbers
df <- df |>
  mutate(survey_taken = if_else(survey_taken == "Yes", 1, 0)) |>
  mutate(concentrated_activity_binary = if_else(concentrated_activity_binary == "Yes", 1, 0))

# recode IDs to numeric factors instead of emails
df <- df |>
  mutate(id = as.integer(as.factor(id)))


# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0051_johannes_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(dataset_id == "0051") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0051") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0051_johannes_metadata.json"))
