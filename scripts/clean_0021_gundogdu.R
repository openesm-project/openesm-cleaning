# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(jsonlite)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# read in data
df_raw <- read.csv(here("data", "raw", "0021_gundogdu_ts_raw.csv"))

df_passive <- read.csv(here("data", "raw", "0021_gundogdu_passive_raw.csv"))


# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
# rename columns
df <- df_raw |>
  janitor::clean_names() |>
  rename(
    openness = creativity
  )


#* Misc -------------------------------------------------------------------
# create date column based on timestamp (unix time)
# double checked with https://github.com/didemgundogdu/RoyalOpenSciencePersonalityDynamics/blob/f56e1ce936c24bbb8625735305897661e8515b1e/survey_with_traits_date.cs
df <- df |>
  mutate(
    date = as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC"))

# create day variable for each user
df <- df |>
  group_by(id) |>
  mutate(
    day = as.integer(as.Date(date) - min(as.Date(date))) + 1
  ) |>
  ungroup()

# create empty beep
df$beep <- NA


# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
metadata <- read_metadata("0021")

# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, metadata)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0021_gundogdu_ts.tsv"))
}
