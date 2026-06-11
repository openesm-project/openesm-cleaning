# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# File was sent to us directly, so no online download for now

# or read from local file if already downloaded
data_raw <- readr::read_csv(here::here("data", "raw", "0076_bayer_ts_raw.csv"))

# Cleaning ----------------------------------------------------------------

#* Column Names -----------------------------------------------------------
df <- data_raw |>
  janitor::clean_names() |>
   dplyr::rename(
    id = p,
    day = d,
    beep = n,
    group = g,
    mobility_survey = m,
    start_time = start_date,
    end_time = end_date,
    response_duration = duration_in_seconds,
    positive_affect = a1,
    energy = a2,
    fidgety = a3,
    want_other_people = a4,
    sleep_duration = d1_sleep,
    healthy = d2_health,
    exercise = d3_exer,
    stressed = d4_stress,
    self_esteem = d5_self,
    productive = d6_lazy,
    location_simulation = l1,
    location_overstimulation = l2,
    location_familiar = l3,
    location_interesting = l4,
    location_refreshing = l5,
    location_compatible_personality = l6,
    interaction_type = s1,
    interaction_when = s2,
    interaction_number = s3,
    interaction_pleasant = s4,
    interatcion_playful = s5,
    interaction_meaningful = s6,
    interaction_frequency = s7,
    mobile_data_access = c1a,
    mobile_data_speed = c1b,
    mobile_data_speed_open = c1c,
    public_wifi = c2a,
    public_wifi_speed = c2b,
    public_wifi_speed_open = c2c,
    time_perception_clock = t1,
    time_orientation = t2,
    focus_activity = t3,
    problem_thoughts = t4,
    problem_progress = t5,
    time_available = t6,
    survey_error = flag

  )





#* Misc -------------------------------------------------------------------
# convert string sentinels ("NA", "", ...) and NaN to real NA
df <- recode_missing(df)

# time variables were already posixct

# export column names
write_csv(
  tibble(variable_name = names(df)),
  here("data", "column_names", "0076_bayer_variable_names.csv")
)

# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
# Enter dataset ID here
meta_data <- read_sheet(METADATA_URL)
dataset_info <- meta_data |>
  filter(dataset_id == "0076")
variable_data <- read_sheet(pull(dataset_info, "Coding File URL"))


# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, dataset_info, variable_data)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0076_bayer_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
write_metadata("0076", meta_data = meta_data, variable_data = variable_data)
