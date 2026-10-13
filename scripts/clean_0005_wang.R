# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(jsonlite)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# download dataset from GitHub
if(!file.exists(here("data", "raw", "0005_wang_ts_raw.csv"))){
  download.file("https://raw.githubusercontent.com/CornellPACLab/data_heterogeneity/refs/heads/main/data/crosscheck_daily_data_cleaned_w_sameday.csv",
                destfile = here("data", "raw", "0005_wang_ts_raw.csv"))
}
df_raw <- read.csv(here("data", "raw", "0005_wang_ts_raw.csv"))




# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
df <- df_raw |>
  janitor::clean_names() |>
  rename_with(~str_remove_all(., "ema_"), everything()) |>
  rename(
    id = study_id,
    think_clearly = think
  )



#* Misc -------------------------------------------------------------------

# remove unclear/irrelevant columns
df <- df |>
  select(!c(x, eureka_id, missing_days))

# remove sum scores
df <- df |>
  select(!c(neg_score, pos_score, score))

# clean day column
df <- df |>
  mutate(date = as.Date(date, format = "%Y-%m-%d")) |>
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
metadata <- read_metadata("0005")


# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, metadata)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0005_wang_ts.tsv"))
}
