# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
library(readxl)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
df_raw <- read_excel(here::here("data", "raw", "0008_westhoff_ts_raw.xlsx"),
                                    skip = 1)


# Cleaning ----------------------------------------------------------------

#* Column Names -----------------------------------------------------------
df <- df_raw |>
  janitor::clean_names() |>
  rename(id = name,
         depressed = stopd_1,
         anxious = stopd_2,
         stressed = stopd_3,
         angry = stopd_4,
         lacking_support = stopd_5,
         affect_valence = core_affect_valence,
         affect_arousal = core_affect_arousal,
         negative_affect_behavior = pbat_1,
         positive_affect_behavior = pbat_2,
         negative_cognition_behavior = pbat_3,
         positive_cognition_behavior = pbat_4,
         negative_attention_behavior = pbat_5,
         positive_attention_behavior = pbat_6,
         negative_social_connection_behavior = pbat_7,
         positive_social_connection_behavior = pbat_8,
         negative_motivation_behavior = pbat_9,
         positive_motivation_behavior = pbat_10,
         negative_overt_behavior = pbat_11,
         positive_overt_behavior = pbat_12,
         negative_physical_health_behavior = pbat_13,
         positive_physical_health_behavior = pbat_14,
         negative_change_behavior = pbat_15,
         positive_change_behavior = pbat_16,
         negative_behavior_retention = pbat_17,
         positive_behavior_retention = pbat_18,
         sleep_duration = sleep_hour)


#* Misc -------------------------------------------------------------------
# replace -1 with NA in different column types
df <- df |>
  mutate(across(where(is.character), ~na_if(., "-1"))) |>
  mutate(across(where(is.character), ~na_if(., "-1.0"))) |>
  # replace -1 with NA in numeric columns
  mutate(across(where(is.numeric), ~na_if(., -1.0)))

# correct day and beep columns
df <- df |>
  mutate(beep = as.double(session),
         counter = as.double(session_id)) |>
  select(-c(session, session_id)) |>
  mutate(day = as.double(day))

# split demographic data to separate file
df_demographics <- df |>
  select(id, gender, age, relationship, livingstatus, degree, occupation) |>
  distinct()

# save as static dataframe
write_tsv(df_demographics, here("data", "clean", "0008_westhoff_static.tsv"))

# remove irrelevant id column
df <- df |>
  select(-vid)

# remove demographic columns from main data
df <- df |>
  select(-c(gender, age, relationship, livingstatus, degree, occupation))

# also remove fully empty or redundant columns
df <- df |>
  select(-c(recorded_date, progress))

# save date related columns as posixct
df <- df |>
  mutate(across(c(scheduled_time, response_time, start_date, end_date), ~as.POSIXct(., format = "%Y-%m-%d %H:%M:%S")))

# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0008_westhoff_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(dataset_id == "0008") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0008") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0008_westhoff_metadata.json"))
