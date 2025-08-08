# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
library(readxl)
source(here("scripts", "functions_data.R"))

# Data --------------------------------------------------------------------
# .zip data is very large: https://osf.io/hsrfg
# downloaded manually and then extracted the ESM and questionnaire data
# sensor data taken from "EDA/signal_data.csv"
df_raw <- read_xlsx(here("data", "raw", "0070_vanhalem_ts_raw.xlsx"))


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
  rename(
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
  select(c(-x1, participant_mov)) |>
  select(-c(disconnected, battery))

# convert time columns to PosixCt
df <- df |>
  mutate(across(contains("time") | contains("date"),
                ~ as.POSIXct(.x, format = "%Y-%m-%d %H:%M:%S")))

# check for character NA
df <- df |>
  mutate(across(where(is.character), ~ na_if(.x, "NA"))) |>
  mutate(across(where(is.character), ~ na_if(.x, "")))

# create day variable
df <- df |>
  mutate(date = as.Date(trigger_date)) |>
  # add day number
  group_by(id) |>
  mutate(
    day = as.integer(date - min(date, na.rm = TRUE)) + 1
  ) |>
  ungroup() |>
  select(!date)

# create beep variable
df <- df |>
  group_by(id, day) |>
  mutate(beep = row_number()) |>
  ungroup()

# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0070_vanhalem_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(dataset_id == "0070") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0070") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0070_vanhalem_metadata.json"))
