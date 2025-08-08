# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
library(osfr)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# download data
if(!file.exists(here::here("data", "raw", "0049_pronizius_ts_raw.xlsx"))){
  osf_retrieve_file("https://osf.io/rf36u") |>
    osf_download(path = here::here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/rf36u") |> pull(name)
  file.rename(here::here("data", "raw", file_name), here::here("data", "raw", "0049_pronizius_ts_raw.xlsx"))
}

# read data
df_raw <- readxl::read_excel(here("data", "raw", "0049_pronizius_ts_raw.xlsx"))


# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
df <- df_raw |>
  janitor::clean_names() |>
  rename(
    id = participant_e,
    beep = mzp,
    counter = count,
    helping_binary = helping_bin,
    social_binary = soc_bin,
    weekday = wdwe
  )


#* Misc -------------------------------------------------------------------
# split off demographic data
df_demographics <- df |>
  select(id, age, gender, country_name, pss, loneliness, depression, virusconcern, helpers) |>
  group_by(id) |>
  distinct()

write_tsv(df_demographics, here("data", "clean", "0049_pronizius_static.tsv"))

# split off from time series data
df <- df |>
  select(!c(age, gender, country_name, pss, loneliness, depression, virusconcern, helpers))

# nicer column order
df <- df |>
  select(id, day, beep, counter, ema_time, everything())

# recode ema time to actual hour of the day
df <- df |>
  mutate(ema_time = ema_time + 10)

# recode day of the week from numeric to actual days
df <- df |>
  mutate(weekday = case_when(
    weekday == 1 ~ "Monday",
    weekday == 2 ~ "Tuesday",
    weekday == 3 ~ "Wednesday",
    weekday == 4 ~ "Thursday",
    weekday == 5 ~ "Friday",
    weekday == 6 ~ "Saturday",
    weekday == 7 ~ "Sunday"
  ))

df <- df |>
  mutate(day = as.numeric(day))

# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0049_pronizius_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(dataset_id == "0049") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0049") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0049_pronizius_metadata.json"))
