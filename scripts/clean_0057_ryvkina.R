# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(osfr)
library(jsonlite)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# download data
if(!file.exists(here::here("data", "raw", "0057_ryvkina_ts_raw.csv"))){
  osf_retrieve_file("https://osf.io/xzqvt") |>
    osf_download(path = here::here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/xzqvt") |> pull(name)
  file.rename(here::here("data", "raw", file_name), here::here("data", "raw", "0057_ryvkina_ts_raw.csv"))
}

# read data
df_raw <- read.csv(here::here("data", "raw", "0057_ryvkina_ts_raw.csv"))



# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
df <- df_raw |>
  janitor::clean_names() |>
  dplyr::rename(
    id = id_for_merging
  ) |>
  # reorder some columns more logically
  dplyr::select(id, wave, dataset, created_esm, ended_esm, dplyr::everything())


#* Misc -------------------------------------------------------------------
# split off demographic data
# remove irrelevant columns
df <- df |>
  dplyr::select(!c(id_not_for_merging,
            consent))


# split of demographic columns
df_demographics <- df |>
  dplyr::group_by(id) |>
  dplyr::select(c(
    contains("bfi"),
    contains("uls"),
    contains("policy"),
    contains("narqs"),
    contains("rses"),
    starts_with("risk"),
    starts_with("eval"),
    starts_with("change"),
    household,
    sidejob,
    toiletpaper,
    pasta,
    starts_with("aff_"),
    # remove outlier but not per_report outlier (for ESM data)
    matches("outlier(?!.*per_report)", perl = TRUE),
    ends_with("_t2"),
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

write_tsv(df_demographics, here("data", "clean", "0057_ryvkina_static.tsv"))

# remove demographic columns
df <- df |>
  dplyr::select(!c(
    contains("bfi"),
    contains("uls"),
    contains("policy"),
    contains("narqs"),
    contains("rses"),
    starts_with("risk"),
    starts_with("eval"),
    starts_with("change"),
    household,
    sidejob,
    toiletpaper,
    pasta,
    starts_with("aff_"),
    # remove outlier but not per_report outlier (for ESM data)
    matches("outlier(?!.*per_report)", perl = TRUE),
    ends_with("_t2"),
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

# reorder columns
df <- df |>
  dplyr::select(id, day, beep, everything())


# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here::here("data", "clean", "0057_ryvkina_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  dplyr::filter(dataset_id == "0057") |>
  dplyr::pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0057") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here::here("data", "metadata", "0057_ryvkina_metadata.json"))
