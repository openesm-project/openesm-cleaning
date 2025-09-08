# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
library(osfr)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# download zip file
if(!file.exists(here("data", "raw", "0061_merolla_ts_raw.zip"))){
  osf_retrieve_file("https://osf.io/gfqbd") |>
    osf_download(path = here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/gfqbd") |> pull(name)
  file.rename(here("data", "raw", file_name), here("data", "raw", "0061_merolla_ts_raw.zip"))
}

# unzip data
if(!file.exists(here("data", "raw", "0061_merolla_ts_raw"))) {
  unzip(
    here("data", "raw", "0061_merolla_ts_raw.zip"),
    exdir = here("data", "raw", "0061_merolla_ts_raw")
  )
}

# read in data
df_raw <- read_csv(
  here(
    "data",
    "raw",
    "0061_merolla_ts_raw",
    "data and analyses",
    "data Spring 2020.csv"
  )
)
# save under proper name
write.csv(df_raw, here("data", "raw", "0061_merolla_ts_raw.csv"))


# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
df <- df_raw |>
  janitor::clean_names() |>
  rename(
    counter = survey_number,
    day = day_number,
    relationship_level = relat,
    cared_for = caredfor,
    connected = conn,
    first_prompt = t0,
    time_since_first = t_cen
  )


#* Misc -------------------------------------------------------------------
# convert timestamp to POSIXct
df <- df |>
  mutate(time = as.POSIXct(timestamp, tz = "US/Pacific")) |>
  # remove column that is redundant now, can only use it for
  # double-checking the timezone conversion
  select(!local_date) |>
  select(id, timestamp, counter, day, everything())

# move pre- and post info to separate dataframe
df_demographics <- df |>
  select(id, age, gender, contains("pre"), contains("post"), contains("race"), contains("cond")) |>
  group_by(id) |>
  distinct()

# save demographics
write_tsv(df_demographics, here("data", "clean", "0061_merolla_static.tsv"))

# remove columns from main dataframe
df <- df |>
  select(!c(age, gender, contains("pre"), contains("post"), contains("race"), contains("cond"))) |>
  # also remove sum score
  select(!resp)

# remove redundant time column in hours
df <- df |>
  select(!t_hr)

# convert dummy-coded variable to single categorical variable
df <- df |>
  mutate(
    interaction_type = as.factor(case_when(
      phone == 1 ~ "phone",
      video == 1 ~ "video",
      text == 1 ~ "text",
      interact == 1 ~ "interact",
      TRUE ~ "0" # Default case if none are 1
    ))) |>
  select(!c(phone, video, text, interact))


# create beep for every participant (6x/day)
n_beeps <- 6
n_days <- 10
n_id <- 120
df <- df |>
  mutate(beep = rep(seq(1, n_beeps), n_days * n_id))

# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0061_merolla_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(dataset_id == "0061") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0061") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0061_merolla_metadata.json"))
