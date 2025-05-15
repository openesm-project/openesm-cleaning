# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
library(osfr)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# download file if not already downloaded
if(!file.exists(here("data", "raw", "0044_mostajabi_ts_raw.csv"))){
  osf_retrieve_file("https://osf.io/qmvjd") |>
    osf_download(path = here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/qmvjd") |> pull(name)
  file.rename(here("data", "raw", file_name), here("data", "raw", "0044_mostajabi_ts_raw.csv"))
}

df_raw <- read.csv(here("data", "raw", "0044_mostajabi_ts_raw.csv"))


# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
df <- df_raw |>
  janitor::clean_names() |>
  rename(
    id = participant_id,
    happy = pa1,
    proud = pa2,
    content = pa3,
    excited = pa4,
    relaxed = pa5,
    ashamed = neg_aff1,
    nervous = neg_aff2,
    hostile = neg_aff3,
    sad = neg_aff4,
    angry = neg_aff5
  )




#* Misc -------------------------------------------------------------------
# change some columns to PosixCt
df <- df |>
  mutate(across(all_of(c(contains("begin"), contains("finish"))),
                ~ as.POSIXct(.x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")))


# remove irrelevant variables that can be computed from data
df <- df |>
  select(!c(total_ema, pa, neg_aff))

# remove (re-)centered columns which end on _p or _pc
df <- df |>
  select(!ends_with("_p")) |>
  select(!ends_with("_pc"))


# split off demographic data
df_demographics <- df |>
  select(!c(contains("begin"), contains("finish"),
            happy, proud, content, excited, relaxed,
            ashamed, nervous, hostile, sad, angry, duration_ema, interaction,
            dom_sub_you, warm_cold_you)) |>
  distinct()

# save demographics data
saveRDS(df_demographics, here("data", "clean", "0044_mostajabi_statics.rds"))

# remove demographics data from main data frame
df <- df |>
  select(c(id, contains("begin"), contains("finish"),
           happy, proud, content, excited, relaxed,
           ashamed, nervous, hostile, sad, angry, duration_ema, interaction,
           dom_sub_you, warm_cold_you))


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
  write_tsv(df, here("data", "clean", "0044_mostajabi_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(id == "0044") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0044") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0044_mostajabi_metadata.json"))
