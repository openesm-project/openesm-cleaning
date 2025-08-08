# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
library(osfr)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# download file if not already downloaded
if(!file.exists(here("data", "raw", "0042_mostajabi_ts_raw.csv"))){
  osf_retrieve_file("https://osf.io/d4me2") |>
    osf_download(path = here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/d4me2") |> pull(name)
  file.rename(here("data", "raw", file_name), here("data", "raw", "0042_mostajabi_ts_raw.csv"))
}

df_raw <- read.csv(here("data", "raw", "0042_mostajabi_ts_raw.csv"))

# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
df <- df_raw |>
  janitor::clean_names() |>
  rename(
    id = participant_id
  ) |>
  rename_with(~str_remove(., "_now"), ends_with("_now"))


#* Misc -------------------------------------------------------------------
# change some columns to PosixCt
df <- df |>
  mutate(across(all_of(c(contains("begin"), contains("finish"))),
         ~ as.POSIXct(.x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")))


# remove irrelevant variables that can be computed from data
df <- df |>
  select(!c(total_ema, first_ema, pa, neg_aff, pa_interaction, neg_aff_interaction))


# remove (re-)centered columns which end on _p or _pc
df <- df |>
  select(!ends_with("_p")) |>
  select(!ends_with("_pc"))


# remove demographic information, already present in static dataset
df <- df |>
  select(!c(gender, gender_other, trans, age, handedness, contains("race"), ethnicity,
            language, language_other, sexuality, sexuality_other, marital, grade, degree, degree_other,
            employment, occupation, military_affil, income, religion, religion_other))



# create day variable
df <- df |>
 group_by(id) |>
  mutate(date = as.Date(begin_day_ema)) |>
  mutate(
    day = as.integer(date - min(date)) + 1
  ) |>
  ungroup() |>
  select(!date)


# create beep variable
df$beep <- NA



# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0042_mostajabi_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(dataset_id == "0042") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0042") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0042_mostajabi_metadata.json"))
