# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
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

# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0005_wang_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(id == "0005") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0005") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0005_wang_metadata.json"))
