# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
library(haven)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
df_raw <- read_sav(here::here("data", "raw", "0019_soederberg_ts_raw.sav"))


# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
df <- df_raw |>
  janitor::clean_names() |>
  # remove _all in column names
  rename_with(~ str_remove(., "_all$")) |>
  rename(id = id4,
         beep = session,
         stressed = stress,
         enjoy_school = enjoy,
         lecture_enjoy = le_enjoy,
         lecture_difficult = le_diff,
         lecture_interesting = le_intr,
         teacher_strict = te_str,
         teacher_fair = te_fair,
         teacher_encouraging = te_enc,
         peer_relationship = peer_rel,
         sleep_quality = morning_sleep)


#* Misc -------------------------------------------------------------------
# split off demographic/static data
demo_cols <- c("gender", "edu_level", "school_enj", "school_abs")

df_demographics <- df |>
  group_by(id) |>
  distinct(across(c(all_of(demo_cols), contains("pss"), contains("tsr"), contains("sgse"), contains ("fsl"), contains("seqc")))) |>
  ungroup()

# save demographic data
write_tsv(df_demographics, here("data", "clean", "0019_soederberg_static.tsv"))

# remove demographic columns from main data
df <- df |>
  select(-all_of(demo_cols)) |>
  select(-contains("pss"), -contains("tsr"), -contains("sgse"), -contains("fsl"), -contains("seqc"))

# remove irrelevant columns
df <- df |>
  select(-c(startup, esm))

# arrange data set
df <- df |>
  dplyr::arrange(id, notification_time)

# time column to Posixct
df <- df |>
  mutate(notification_time = as.POSIXct(notification_time, format = "%Y-%m-%d %H:%M:%S"))

# binary column to 0 1
df <- df |>
  mutate(morning_breakfast = as.factor(case_when(
    morning_breakfast == 1 ~ 1,
    morning_breakfast == 2 ~ 0,
    TRUE ~ NA_real_
  )))

# check for character NAs
df <- df |>
  mutate(across(where(is.character), ~ na_if(., "NA"))) |>
  mutate(across(where(is.character), ~ na_if(., "")))

# rename hostile to stupid due to swedish translation
df <- df |>
  rename(stupid = hostile)


# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0019_soederberg_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(dataset_id == "0019") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0019") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0019_soederberg_metadata.json"))
