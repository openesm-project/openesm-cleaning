# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# obtained from kaggle
# https://www.kaggle.com/datasets/subigyanepal/college-experience-dataset/data
df_raw <- read.csv(here("data", "raw", "0040_nepal_ts_raw.csv"))
df_covid <- read.csv(here("data", "raw", "0040_nepal_ts_2_raw.csv"))

# Combined data sets based on id and date
df <- df_raw |>
  left_join(df_covid, by = c("uid", "day"))

# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
df <- df |>
  janitor::clean_names() |>
  rename(
    id = uid,
    date = day,
    affect = pam,
    nervous = phq4_1,
    worrying = phq4_2,
    depressed = phq4_3,
    anhedonia = phq4_4,
    stressed = stress,
    perception_worry = sse3_1,
    appearence_pleased = sse3_2,
    feel_smart = sse3_3,
    feel_good = sse3_4,
    covid_concern_general = covid_1,
    covid_daily_activities = covid_2,
    covid_behavior_change = covid_3,
    covid_concern_self = covid_4,
    covid_concern_classmates = covid_5,
    covid_concern_family = covid_6,
    covid_concern_supplies = covid_7,
    covid_felt_support = covid_8,
    covid_support_others = covid_9,
    social_media_use = covid_10,
    avg_ema_rt = avg_ema_spent_time
  )


#* Misc -------------------------------------------------------------------
# remove aggregate columns
df <- df |>
  select(!c(contains("_mean"), contains("_median"), "phq4_score"))

# proper date encoding
df <- df |>
  mutate(date = as.Date(as.character(date), format = "%Y%m%d"))


# create day column
df <- df |>
  group_by(id) |>
  mutate(
    day = as.integer(date - min(date)) + 1
  ) |>
  ungroup()

# create beep column
df$beep <- 1


# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0040_nepal_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(id == "0040") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0040") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0040_nepal_metadata.json"))
