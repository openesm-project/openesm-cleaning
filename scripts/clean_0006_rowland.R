# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
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


# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0006_rowland_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)

sheet_url <- meta_data |>
  filter(id == "0006") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0006") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0006_rowland_metadata.json"))
