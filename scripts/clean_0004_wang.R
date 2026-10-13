# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(jsonlite)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# download dataset from GitHub
if(!file.exists(here("data", "raw", "0004_wang_ts_raw.csv"))){
  download.file("https://raw.githubusercontent.com/CornellPACLab/data_heterogeneity/refs/heads/main/data/studentlife_daily_data_cleaned_w_sameday_08282021.csv",
                destfile = here("data", "raw", "0004_wang_ts_raw.csv"))
}
df_raw <- read.csv(here("data", "raw", "0004_wang_ts_raw.csv"))


# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
df <- df_raw |>
  janitor::clean_names()

# some renaming
df <- df |>
  # remove some redundant column name parts
  rename_with(~str_remove_all(., "_mood|_behavior"), everything()) |>
  rename_with(~str_remove_all(., "ema_"), everything()) |>
  rename(
    id = study_id,
    sleep_quality = sleep_rate,
    open_to_experiences = experiences,
    difficult_stay_awake = sleep_social
  )




#* Misc -------------------------------------------------------------------
# remove unclear columns
df <- df |>
  select(!c(pam_picture_idx, missing_days))


# clean day column
df <- df |>
  mutate(day = gsub(day, pattern = " 00:00:00-04:00", replacement = "")) |>
  mutate(date = as.Date(day, format = "%Y-%m-%d")) |>
  # personal day variable
  group_by(id) |>
  mutate(
    day = as.integer(date - min(date)) + 1
  ) |>
  ungroup()


# add beep
df$beep <- 1

# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
# Enter dataset ID here
metadata <- read_metadata("0004")


# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, metadata)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0004_wang_ts.tsv"))
}
