# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
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


# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0021_gundogdu_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(id == "0021") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0021") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0021_gundogdu_metadata.json"))
