# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
library(osfr)
source(here("scripts", "functions_data.R"))


# Data --------------------------------------------------------------------
# donwload data
if(!file.exists(here::here("data", "raw", "0056_rykvina_ts_raw.csv"))){
  osf_retrieve_file("https://osf.io/9dtfz") |>
    osf_download(path = here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/9dtfz") |> pull(name)
  file.rename(here::here("data", "raw", file_name), here::here("data", "raw", "0056_rykvina_ts_raw.csv"))
}

# read data
df_raw <- read.csv(here("data", "raw", "0056_rykvina_ts_raw.csv"))


# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
df <- df_raw |>
  janitor::clean_names() |>
  rename(
    id = id_for_merging
  ) |>
  # reorder some columns more logically
  select(id, wave, dataset, created_esm, ended_esm, everything())



#* Misc -------------------------------------------------------------------
# remove irrelevant columns
df <- df |>
  select(!c(id_not_for_merging,
         consent))



# split of demographic columns
df_demographics <- df |>
  group_by(id) |>
  select(c(
    id,
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
  distinct()

saveRDS(df_demographics, here("data", "clean", "0056_rykvina_static.rds"))


# remove demographic columns
df <- df |>
  select(!c(
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
  select(!c(
    contains("irv"),
    contains("outlier"),
    contains("longstring"),
    contains("completion_time")
  ))

# convert time columns to Posixct
df <- df |>
  mutate(created_esm = as.POSIXct(created_esm, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
         ended_esm = as.POSIXct(ended_esm, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))


# create day variable
# add day number
df <- df |>
  mutate(date = as.Date(created_esm)) |>
  group_by(id) |>
  mutate(
    day = as.integer(date - min(date)) + 1
  ) |>
  ungroup() |>
  select(!date)


df$beep <- NA

# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0056_rykvina_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(id == "000X") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("000X") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "000X_NAME_metadata.json"))
