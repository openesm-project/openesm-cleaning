# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(osfr)
library(jsonlite)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# data were downloaded directly from https://doi.org/10.23668/psycharchives.5666
df_raw <- read_delim(here("data", "raw", "0067_schmiedek_ts_raw.csv"),
                   delim = ";", escape_double = FALSE, trim_ws = TRUE,
                   # avoid issues when reading in columns by reading all as
                   # character
                   col_types = cols(.default = "c"))



# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
df <- df_raw |>
  janitor::clean_names()

# renaming other columns later down below for easier handling of
# demographic variables



#* Misc -------------------------------------------------------------------
# remove columns that contain names
df <- df |>
  select(!c(bfq1, bfq_post))

# Split off demographics
df_demographics <- df |>
  select(!c(day1, occasion, weekday,
    starts_with("afs"),
    starts_with("sw"),
    starts_with("zoa"),
    starts_with("zos"),
    starts_with("zea"),
    starts_with("zes"),
    contains("fail"),
    contains("succ"),
    contains("sob"),
    contains("ata"),
    contains("ats"),
    wka1,
    wks1,
    starts_with("cop"),
    fach,
    moti,
    pa,
    na3,
    ac_lern,
    ac_apr,
    ac_av,
    s_dev,
    s_ap,
    s_av,
    relsat,
    reldis,
    lone
  )) |>
  group_by(id) |>
  distinct() |>
  ungroup()

# save demographics
write_tsv(df_demographics, here("data", "clean", "0067_schmiedek_static.tsv"))

# remove from df
df <-  df |>
  select(c(id, day1, occasion, weekday,
    starts_with("afs"),
    starts_with("sw"),
    starts_with("zoa"),
    starts_with("zos"),
    starts_with("zea"),
    starts_with("zes"),
    contains("fail"),
    contains("succ"),
    contains("sob"),
    contains("ata"),
    contains("ats"),
    wka1,
    wks1,
    starts_with("cop"),
    fach,
    moti,
    pa,
    na3,
    ac_lern,
    ac_apr,
    ac_av,
    s_dev,
    s_ap,
    s_av,
    relsat,
    reldis,
    lone
  ))

# remove unclear and aggregate columns
df <- df |>
  select(!contains("ata")) |>
  select(!contains("ats")) |>
  select(!ends_with("_mean"))



#** Renaming daily columns --------------------------------------------------
df <- df |>
 rename(
  beep = occasion,
  # affect
  good_mood = afs1,
  feel_great = afs2,
  satisfied = afs3,
  unhappy = afs4,
  feel_bad = afs5,
  anxious = afs6,
  concentrated = afs7,
  restless = afs8,
  exhausted = afs9,
  relaxed = afs10,
  proud = afs11,
  bored = afs12,
  annoyed = afs13,
  happy = afs14,
  ashamed = afs15,
  hopeless = afs16

  # goal-orientation

)




# Time columns

# Convert to NA

# Beep column




# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "000X_NAME_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(dataset_id == "000X") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("000X") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "000X_NAME_metadata.json"))
