# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
library(readr)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# Consists of multiple datasets
# "data_participants" was already renamed to static data
# "data_context" was already renamed to weekly data
df_raw <- read_delim(here("data", "raw", "0018_bailon_ts_raw.csv"),
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)


# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
df <- df_raw |>
  janitor::clean_names() |>
  rename(
    id = participant,
    timestamp_issued = timestamp,
    timestamp_answer = answer_timestamp,
    arousal_slider_initial = arousal_scale_ini,
    valence_slider_initial = valence_scale_ini
  )




#* Misc -------------------------------------------------------------------
# convert timestamps to POSIXct
df <- df |>
  mutate(
    timestamp_issued = as.POSIXct(timestamp_issued, format = "%Y-%m-%d %H:%M:%S"),
    timestamp_answer = as.POSIXct(timestamp_answer, format = "%Y-%m-%d %H:%M:%S")
  )

# create day variable
df <- df |>
  group_by(id) |>
  mutate(
    date = date(timestamp_answer),
    day = as.integer(date - min(date)) + 1
  ) |>
  ungroup()

# add empty beep variable
df$beep <- NA


# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0018_bailon_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(dataset_id == "0018") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0018") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0018_bailon_metadata.json"))
