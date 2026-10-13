# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
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

# check redundancy of date column
df |>
  mutate(day_answer = as.Date(timestamp_answer)) |>
  mutate(date = as.Date(date)) |>
  select(day_answer, date) |>
  mutate(same_date = day_answer == date) |>
  filter(!same_date)

# remove
df <- df |>
  select(!date)




# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
metadata <- read_metadata("0018")

# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, metadata)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0018_bailon_ts.tsv"))
}
