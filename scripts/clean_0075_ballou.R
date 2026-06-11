# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# Download directly from GitHub
if(!file.exists(here::here("data", "raw", "0075_ballou_ts_raw.tsv"))){
url <- "https://raw.githubusercontent.com/digital-wellbeing/open-play/main/data/clean/survey_daily.csv.gz"
data_raw <- readr::read_csv(url)
# save raw data
readr::write_tsv(data_raw, here::here("data", "raw", "0075_ballou_ts_raw.tsv"))
}



# or read from local file if already downloaded
data_raw <- readr::read_tsv(here::here("data", "raw", "0075_ballou_ts_raw.tsv"))

# Cleaning ----------------------------------------------------------------

#* Column Names -----------------------------------------------------------
df <- data_raw |>
  dplyr::rename(
    id = pid,
    day = wave,
    # BANGS items
    gaming_autonomy = bangs_1,           # played way I wanted
    gaming_wish_else = bangs_2,          # wished do something else
    gaming_progress = bangs_3,           # made progress
    gaming_disappointed = bangs_4,       # disappointed with performance
    gaming_relationships = bangs_5,      # formed relationships
    gaming_toxic = bangs_6,              # toxic interactions

    # Social gaming context
    social_singleplayer = social_gaming_4,
    social_friends_realworld = social_gaming_5,
    social_friends_online = social_gaming_6,
    social_strangers = social_gaming_7,

    # BPNSFS items (Basic Psychological Need Satisfaction and Frustration Scale)
    needs_autonomy_satisfied = bpnsfs_1,       # able to do valued things
    needs_autonomy_frustrated = bpnsfs_2,     # forced to do things
    needs_competence_satisfied = bpnsfs_3,     # could do things well
    needs_competence_frustrated = bpnsfs_4,   # insecure about abilities
    needs_relatedness_satisfied = bpnsfs_5,    # felt close and connected
    needs_relatedness_frustrated = bpnsfs_6,  # felt excluded

    life_satisfaction = life_sat,

    # remove sleep diary parts from column names
    sleep_quality = sleep_diary_quality,
    day_type = sleep_diary_day_type
  )





#* Misc -------------------------------------------------------------------
# recode any "NA" to proper NA
df <- recode_missing(df)

# add beep (always 1 per day)
df <- df |>
  dplyr::mutate(beep = 1)


# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
# Enter dataset ID here
meta_data <- read_sheet(METADATA_URL)
dataset_info <- meta_data |>
  filter(dataset_id == "0075")
variable_data <- read_sheet(pull(dataset_info, "Coding File URL"))


# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, dataset_info, variable_data)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0075_ballou_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
write_metadata("0075", meta_data = meta_data, variable_data = variable_data)
