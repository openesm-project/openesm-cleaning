# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
library(readxl)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
df_raw <- read_excel(here::her("data", "raw", "0029_drukker_ts_raw.xls"))


# Cleaning ----------------------------------------------------------------

#* Column Names -----------------------------------------------------------
# rename columns
df <- df_raw |>
  janitor::clean_names() |>
  # remove "mood_" from all variable names
  rename_with(~str_remove(., "mood_")) |>
  rename(
    day = dayno,
    beep = beepno,
    phy_abd = phyabd,
    enthusiastic = enthous,
    irritated = irritat,
    cheerful = cheerf
  )


#* Misc -------------------------------------------------------------------
# split off demographic data
df_demographic <- df |>
  distinct(id, jtvtrauma)

# save demographic data
write_tsv(df_demographic, here("data", "clean", "0029_drukker_static.tsv"))

# remove demographic column from main data
df <- df |>
  select(-jtvtrauma)

# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0029_drukker_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(dataset_id == "0029") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0029") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0029_drukker_metadata.json"))
