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

    life_satisfaction = life_sat
  )





#* Misc -------------------------------------------------------------------
# recode any "NA" to proper NA
df <- df |>
  mutate(across(where(is.character), ~ na_if(., "NA"))) |>
  # same for Nan
  mutate(across(where(is.character), ~ na_if(., "NaN")))

# add beep (always 1 per day)
df <- df |>
  dplyr::mutate(beep = 1)


# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0075_ballou_ts.tsv"))
}

# export column names
write_csv(tibble(colnames(df)), here("0075_ballou_column_names.csv"), col_names = FALSE)


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(dataset_id == "0075") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0075") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0075_ballou_metadata.json"))
