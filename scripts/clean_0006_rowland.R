# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(jsonlite)
library(osfr)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
if(!file.exists(here("data", "raw", "0006_rowland_ts_raw.csv"))){
  osf_retrieve_file("https://osf.io/m5fcy") |>
    osf_download(path = here("data", "raw"))


  # rename data to 0001_fried.csv
  file_name <- osf_retrieve_file("https://osf.io/m5fcy") |> pull(name)
  file.rename(here("data", "raw", file_name), here("data", "raw", "0006_rowland_ts_raw.csv"))
}

df <- read.csv(here("data", "raw", "0006_rowland_ts_raw.csv"))



# Cleaning ----------------------------------------------------------------

#* Column Names -----------------------------------------------------------
df <- df |>
  rename(
    id = subjno,
    day = dayno,
    happy = emo1_m,
    excited = emo2_m,
    relaxed = emo3_m,
    satisfied = emo4_m,
    angry = emo5_m,
    anxious = emo6_m,
    depressed = emo7_m,
    sad = emo8_m
  )


#* Misc -------------------------------------------------------------------
# split off group variable
df_demographics <- df |>
  select(id, group) |>
  distinct()

# remove group variable from main data frame
df <- df |>
  select(-group)

# save demographics data frame
write_tsv(df_demographics, here("data", "clean", "0006_rowland_static.tsv"))

# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
# Enter dataset ID here
metadata <- read_metadata("0006")


# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, metadata)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0006_rowland_ts.tsv"))
}
