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
  rename(id = name)


#* Misc -------------------------------------------------------------------
# replace -1 with NA in different column types
df <- df |>
  mutate(across(where(is.character), ~na_if(., "-1"))) |>
  mutate(across(where(is.character), ~na_if(., "-1.0"))) |>
  # replace -1 with NA in numeric columns
  mutate(across(where(is.numeric), ~na_if(., -1.0)))

# correct day and beep columns
df <- df |>
  mutate(beep = as.numeric(session),
         counter = as.numeric(session_id)) |>
  select(-c(session, session_id))

# split demographic data to separate file
df_demographics <- df |>
  select(id, gender, age, relationship, livingstatus, degree, occupation) |>
  distinct()

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
  filter(id == "0008") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0008") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0008_westhoff_metadata.json"))
