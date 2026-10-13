# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(jsonlite)
library(osfr)
source(here::here("scripts", "functions_data.R"))


# Data --------------------------------------------------------------------
# donwload data
if(!file.exists(here::here("data", "raw", "0056_ryvkina_ts_raw.csv"))){
  osf_retrieve_file("https://osf.io/9dtfz") |>
    osf_download(path = here::here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/9dtfz") |> pull(name)
  file.rename(here::here("data", "raw", file_name), here::here("data", "raw", "0056_ryvkina_ts_raw.csv"))
}

# read data
df_raw <- read.csv(here::here("data", "raw", "0056_ryvkina_ts_raw.csv"))


# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
df <- df_raw |>
  janitor::clean_names() |>
  dplyr::rename(
    id = id_for_merging
  ) |>
  # reorder some columns more logically
  dplyr::select(id, wave, dataset, created_esm, ended_esm, everything())



#* Misc -------------------------------------------------------------------
# remove irrelevant columns
df <- df |>
  dplyr::select(!c(id_not_for_merging,
         consent))



# split of demographic columns
df_demographics <- df |>
  dplyr::group_by(id) |>
  dplyr::select(c(
    contains("ial"),
    contains("narq"),
    contains("hsns"),
    contains("rses"),
    # remove outlier but not per_report outlier (for ESM data)
    matches("outlier(?!.*per_report)", perl = TRUE),
    ends_with("_t3"),
    ends_with("_t4"),
    matches("created(?!.*esm)", perl = TRUE),
    educational_status,
    occupational_status,
    higher_ed,
    higher_ed_type,
    starts_with("start_"),
    starts_with("end_"),
    email,
    ended_email,
    completion_time_t1,
    completion_time_t2
  ))  |>
  dplyr::distinct()

write_tsv(df_demographics, here("data", "clean", "0056_ryvkina_static.tsv"))


# remove demographic columns
df <- df |>
  dplyr::select(!c(
    contains("ial"),
    contains("narq"),
    contains("hsns"),
    contains("rses"),
    # remove outlier but not per_report outlier (for ESM data)
    matches("outlier(?!.*per_report)", perl = TRUE),
    ends_with("_t3"),
    ends_with("_t4"),
    matches("created(?!.*esm)", perl = TRUE),
    educational_status,
    occupational_status,
    higher_ed,
    higher_ed_type,
    starts_with("start_"),
    starts_with("end_"),
    email,
    ended_email,
    completion_time_t1,
    completion_time_t2
  ))

# remove careless responding indicators
df <- df |>
  dplyr::select(!c(
    contains("irv"),
    contains("outlier"),
    contains("longstring"),
    contains("completion_time")
  ))

# convert time columns to Posixct
df <- df |>
  dplyr::mutate(created_esm = as.POSIXct(created_esm, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
         ended_esm = as.POSIXct(ended_esm, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))


# create day variable
# add day number
df <- df |>
  dplyr::mutate(date = as.Date(created_esm)) |>
  dplyr::group_by(id) |>
  dplyr::mutate(
    day = as.integer(date - min(date)) + 1
  ) |>
  dplyr::ungroup() |>
  dplyr::select(!date)


df$beep <- NA

# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
metadata <- read_metadata("0056")

# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, metadata)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0056_ryvkina_ts.tsv"))
}
