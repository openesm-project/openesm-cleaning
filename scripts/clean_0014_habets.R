# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# Read in data
df_raw <- read.csv(here("data", "raw", "0014_habets_ts_raw.csv"))




# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
# Rename columns
df <- df_raw |>
  janitor::clean_names() |>
  rename(
    beep_start = beep_time_start,
    beep_end = beep_time_end,
    well = mood_well,
    down = mood_down,
    frightened = mood_fright,
    tense = mood_tense,
    sleepy = phy_sleepy,
    tired = phy_tired,
    cheerful = mood_cheerf,
    relaxed = mood_relax,
    concentrate = thou_concent,
    hallucinations = pat_hallu,
    parkinson_onoff = sanpar_onoff,
    parkinson_medication = sanpar_medic,
    beep_disturbing = beep_disturb,
    mor_slept_well = mor_sleptwell
  )


#* Misc -------------------------------------------------------------------
# convert two beep columns to PosixCt
df <- df |>
  mutate(
    beep_start = as.POSIXct(beep_start, format = "%Y-%m-%d %H:%M:%S"),
    beep_end = as.POSIXct(beep_end, format = "%Y-%m-%d %H:%M:%S")
  )


# add day variable for each person
df <- df |>
  group_by(id) |>
  mutate(
    day = as.integer(as.Date(beep_start) - min(as.Date(beep_start))) + 1
  ) |>
  ungroup()

# add beep variable
df$beep <- NA


# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0014_habets_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(id == "0014") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0014") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0014_habets_metadata.json"))
