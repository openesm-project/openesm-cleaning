# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(jsonlite)
library(osfr)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
if(!file.exists(here("data", "raw", "0023_dora_ts_raw.csv"))){
  osf_retrieve_file("https://osf.io/chb5m") |>
    osf_download(path = here("data", "raw"))


  # rename data
  file_name <- osf_retrieve_file("https://osf.io/chb5m") |> pull(name)
  file.rename(here("data", "raw", file_name), here("data", "raw", "0023_dora_ts_raw.csv"))
}

df <- read.csv(here("data", "raw", "0023_dora_ts_raw.csv"))



# Cleaning ----------------------------------------------------------------

#* Column Names -----------------------------------------------------------
df <- df |>
  janitor::clean_names() |>
  dplyr::rename(id = pid,
                day = study_day,
                beep = version)


#* Misc -------------------------------------------------------------------
# Remove unnecessary column
df$x <- NULL


# split off demographic data to separate file
df_demographics <- df |>
  group_by(id) |>
  distinct(id, age, gender, ddq_typ_drinks)

# save demographic data
write_tsv(df_demographics, here("data", "clean", "0023_dora_static.tsv"))

# remove demographic columns from main data
df <- df |>
  select(-c(age, gender, ddq_typ_drinks))


# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
metadata <- read_metadata("0023")

# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, metadata)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0023_dora_ts.tsv"))
}
