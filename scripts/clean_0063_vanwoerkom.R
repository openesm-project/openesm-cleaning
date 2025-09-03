# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
library(osfr)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# download data if not already present
if(!file.exists(here("data", "raw", "0063_vanwoerkom_ts_raw.csv"))){
  osf_retrieve_file("https://osf.io/hjq4u") |>
    osf_download(path = here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/hjq4u") |> pull(name)
  file.rename(here("data", "raw", file_name), here("data", "raw", "0063_vanwoerkom_ts_raw.csv"))
}

# read in data
df_raw <- read.csv(here("data", "raw", "0063_vanwoerkom_ts_raw.csv"))




# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
df <- df_raw |>
  janitor::clean_names() |>
  rename(
    counter = notice_nr,
    beep = beep_nr,
    weekday = day,
    day = day_nr,
    cheerful = pa_1,
    satisfied = pa_2,
    happy = pa_3,
    insecure = na_1,
    anxious = na_2,
    down = na_3,
    not_tired = ph_1,
    energetic = ph_2,
    inspired = wb_1,
    satisfied_self = wb_2,
    goal_pursuit = wb_3,
    not_feels_like_obligation = at_1,
    chose_doing = at_2,
    good_at_this = cp_1,
    not_doubt = cp_2,
    appreciated = re_1,
    part_of_company = re_2,
    not_misunderstood = re_3
  )


#* Misc -------------------------------------------------------------------
# split off demographics
df_demo <- df |>
  distinct(id, age, gender, prompts_crt)

# save demographics as a separate .tsv file
write_tsv(df_demo, here("data", "clean", "0063_van_woerkom_static.tsv"))

# remove demographics from main data frame
df <- df |>
  select(!c(age, gender, prompts_crt, prompts)) |>
  # also remove some columns that are not needed
  select(!c(missing_count, valid_beeps, should_exclude,
            missing_count_crt, valid_beeps_crt, should_exclude_crt))

# combine date and time and convert to PosixCt
df <- df |>
  mutate(
    datetime = as.POSIXct(paste(date, time), format = "%m/%d/%Y %H:%M:%S", tz = "UTC")) |>
  select(!c(date, time))

# recode "company" to english
df <- df |>
  mutate(
    social_company = case_when(
      company == "niemand ik ben alleen" ~ "no one, I am alone",
      company == "partner" ~ "partner",
      company == "collega's" ~ "colleagues",
      company == "andere familie (uitwonend)" ~ "other family (not living in the same place)",
      company == "ouders" ~ "parents",
      company == "broer/zus" ~ "brother/sister",
      company == "studiegenoten" ~ "fellow students",
      company == "onbekende/anderen" ~ "strangers/others",
      company == "kinderen" ~ "children",
      company == "kennissen" ~ "acquaintances",
      company == "vrienden" ~ "friends",
      is.na(company) ~ NA_character_,
      TRUE ~ "other"
    )
  ) |>
  select(!company) |>
  # select column that only reflects alone above
  select(!alone)

# revert the reverse-transformation of some items
df <- df |>
  mutate(
    tired = 8 - not_tired,
    feels_like_obligation = 8 - not_feels_like_obligation,
    doubt = 8 - not_doubt,
    misunderstood = 8 - not_misunderstood
  ) |>
  select(!c(not_tired, not_feels_like_obligation, not_doubt))



# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0063_van_woerkom_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(dataset_id == "0063") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0063") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0063_vanwoerkom_metadata.json"))
