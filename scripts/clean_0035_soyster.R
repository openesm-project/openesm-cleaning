# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
library(osfr)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# downloaded zip from https://osf.io/q7upd/
# unzip data
unzip(here("data", "raw", "0035_soyster_ts_raw.zip"),
      exdir = here("data", "raw", "0035_soyster_ts_raw"))

# load all csv files in the folder and combine
# into one data frame
file_paths <- list.files(here("data", "raw", "0035_soyster_ts_raw"),
                     pattern = "*.csv",
                     recursive = TRUE,
                     full.names = TRUE)

file_indices <- file_paths |>
  # only include files whose file name starts with "P"
  str_extract("[^/]+$") |>
  str_starts("P")

files_filtered <- file_paths[file_indices]

# irrelevant files
# need to list them manually because it is a bit messy
irrelevant_files <- c(
  "P003-raw copy.csv",
  "P003-raw.csv",
  "P005-raw copy.csv",
  "P005-raw.csv",
  "P008-raw copy.csv",
  "P008-raw.csv",
  "P009-raw copy.csv",
  "P009-raw.csv"
)

# remove irrelevant files
files_filtered <- files_filtered[!basename(files_filtered) %in% irrelevant_files]

# read all csv files in the folder and combine into one list
list_raw <- files_filtered |>
  map(~read_csv(.x, col_names = TRUE))

# deal with wrong specifications in specific data sets
list_raw[[30]] <- list_raw[[30]] |>
  rename(
    "Survey Creation Date" = "Survey Creation"
  )


list_raw[[33]] <- list_raw[[33]] |>
  # change all logical columns to numeric
  mutate(across(where(is.logical), as.numeric))

# combine all data sets to dataframe
df_raw <- list_raw |>
  bind_rows(.id = "id") |>
  # clean up ID names
  mutate(id = substr(id, 1, 4))

# remove tobacco column which was only present in 2 data sets
df_raw <- df_raw |>
  select(-contains("tobacco"))


# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
# rename columns
df <- df_raw |>
  janitor::clean_names()

# give shorter column names
df <- df |>
  rename(
      creation_time = survey_creation_date,
      completion_time = survey_completion_date,
      drink_number = since_your_last_survey_how_many_alcoholic_drinks_have_you_had,
      comfortable = i_feel_comfortable_in_my_current_location,
      stressed = i_feel_stressed,
      depressed = i_feel_down_depressed,
      calm = i_feel_calm_relaxed,
      pressure_to_drink = i_currently_feel_pressure_to_drink,
      enthusiastic = i_feel_enthusiastic,
      happy = i_feel_happy,
      conflict = i_am_having_conflict_fighting_with_others,
      craving = i_am_craving_alcohol,
      impulsive = i_am_feeling_impulsive,
      drink_feel_better = a_drink_would_make_me_feel_better_right_now,
      perceived_peer_drinking = what_percentage_of_berkeley_students_do_you_think_are_drinking_alcohol_right_now,
      want_to_drink = i_would_like_to_drink,
      delay_gratification = i_feel_able_to_delay_gratification,
      angry = i_feel_angry,
      drinks_today_estimate = how_many_alcoholic_drinks_do_you_think_you_number_39_ll_consume_today,
      restless_sleep = last_night_my_sleep_was_restless_or_unsatisfying,
      sleep_difficulty = last_night_i_had_difficulty_falling_or_staying_asleep,
      sleep_hours = how_many_hours_did_you_sleep_last_night
    )



#* Misc -------------------------------------------------------------------




# mutate creation and completion to PosixCt
df <- df |>
  mutate(across(c(creation_time, completion_time), ~as.POSIXct(., format = "%m/%d/%Y %H:%M")))

# day and beep column
df <- df |>
  mutate(date = as.Date(creation_time)) |>
  # add day number
  group_by(id) |>
  mutate(
    day = as.integer(date - min(date)) + 1
  ) |>
  ungroup() |>
  select(!date)

df$beep <- NA

# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0035_soyster_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(dataset_id == "0035") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0035") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0035_soyster_metadata.json"))
