# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
library(readxl)
source(here::here("scripts", "functions_data.R"))

# Data --------------------------------------------------------------------
# .zip data is very large: https://osf.io/hsrfg
# downloaded manually and then extracted the ESM and questionnaire data
# sensor data taken from "EDA/signal_data.csv"
df_raw <- read_xlsx(here::here("data", "raw", "0070_vanhalem_ts_raw.xlsx"))


#- take some cleaning code from their code -------------
# https://osf.io/wh4xg
colnames(df_raw)[2] <- "id"
colnames(df_raw)[35] <- "futloos"
df_raw <- df_raw[
  order( df_raw$id, df_raw$Trigger_date ),
]
# COMMENT: warning messages include: "battery" & "Sensor disconnected". These are to be removed
# Remove type of form: battery or disconnect.

df_raw <- df_raw[!df_raw$Form == "Battery" & !df_raw$Form == "Sensor disconnected"  , ]

#remove type of trigger related to disconnect:
# e.g.:  Immediately (MovisensSensor{sensorName='MOVISENS Sensor 02419', sensorMac='88:6B:0F:64:2C:A1'} was disconnected!)

df_raw <- df_raw[!substr(df_raw$Trigger, 0, 17) == "Immediately (Movi", ]

### 2.2 Removing Pilot participants from the df_raw.

df_raw <- df_raw[df_raw$id > 1010 ,]

#- end of their code ----------------------------------
# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
df <- df_raw |>
  janitor::clean_names() |>
  dplyr::rename(
    counter = trigger_counter,
    enthusiastic = enthousiast,
    relaxed = ontspannen,
    satisfied = tevreden,
    irritable = prikkelbaar,
    energetic = energiek,
    calm = kalm,
    cheerful = vrolijk,
    irritated = geirriteerd,
    bored = verveeld,
    nervous = nerveus,
    sad = verdrietig,
    angry = boos,
    gloomy = somber,
    lifeless = futloos,
    insecure = onzeker,
    fearful = angstig,
    happy = gelukkig,
    worried = bezorgd,
    stressed = gestrest,
    academic = academisch,
    tiring = vermoeiend,
    heartwarming = hartverwarmend,
    standard = standaard,
    productive = productief,
    crazy = gek,
    malicious = kwaadaardig,
    learned = geleerd,
    stressful = stressvol,
    precious = dierbaar,
    ordinary = gewoon,
    useful = nuttig,
    silly = maf,
    repulsive = afstotelijk,
    battery = batterij
  )


#* Misc -------------------------------------------------------------------
# remove irrelevant and unclear column and columns that are always NA
df <- df |>
  dplyr::select(-c(x1)) |>
  dplyr::select(-c(disconnected, battery))

# convert time columns to PosixCt
df <- df |>
  dplyr::mutate(dplyr::across(tidyselect::contains("time") | tidyselect::contains("date"),
                ~ as.POSIXct(.x, format = "%Y-%m-%d %H:%M:%S")))

# check for character NA
df <- df |>
  dplyr::mutate(dplyr::across(where(is.character), ~ dplyr::na_if(.x, "NA"))) |>
  dplyr::mutate(dplyr::across(where(is.character), ~ dplyr::na_if(.x, "")))

# create day variable
df <- df |>
  dplyr::mutate(date = as.Date(trigger_date)) |>
  # add day number
  dplyr::group_by(id) |>
  dplyr::mutate(
    day = as.integer(date - min(date, na.rm = TRUE)) + 1
  ) |>
  dplyr::ungroup() |>
  dplyr::select(!date)

# create beep variable
df <- df |>
  dplyr::group_by(id, day) |>
  dplyr::mutate(beep = dplyr::row_number()) |>
  dplyr::ungroup()

# remove non self-report item
df <- df |>
  select(!c(location, event))

# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
meta_data <- read_sheet(METADATA_URL)
dataset_info <- meta_data |>
  filter(dataset_id == "0070")
variable_data <- read_sheet(pull(dataset_info, "Coding File URL"))

# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, dataset_info, variable_data)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0070_vanhalem_ts.tsv"))
}

# Create metadata ---------------------------------------------------------
write_metadata("0070", meta_data = meta_data, variable_data = variable_data)
