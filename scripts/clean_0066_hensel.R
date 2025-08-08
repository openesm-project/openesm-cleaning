# Packages ----------------------------------------------------------------
library(tidyverse)
library(haven)
library(here)
library(googlesheets4)
library(jsonlite)
library(osfr)
source(here("scripts", "functions_data.R"))


# Data --------------------------------------------------------------------

## Time series
if (!file.exists(here("data", "raw", "0066_hensel_ts_raw.sav"))) {
  osf_retrieve_file("https://osf.io/mtxfe") |>
    osf_download(path = here("data", "raw"))


  # rename data
  file_name <- osf_retrieve_file("https://osf.io/mtxfe") |> pull(name)
  file.rename(here("data", "raw", file_name),
              here("data", "raw", "0066_hensel_ts_raw.sav"))
}

df <- haven::read_sav(here("data", "raw", "0066_hensel_ts_raw.sav"))


## Static enrollment
if (!file.exists(here("data", "raw", "0066_hensel_static_enrollment_raw.sav"))) {
  osf_retrieve_file("https://osf.io/n7bcr") |>
    osf_download(path = here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/n7bcr") |> pull(name)
  file.rename(
    here("data", "raw", file_name),
    here("data", "raw", "0066_hensel_static_enrollment_raw.sav")
  )
}

df_enrollment <- haven::read_sav(here("data", "raw", "0066_hensel_static_enrollment_raw.sav"))


# static exit
if (!file.exists(here("data", "raw", "0066_hensel_static_exit_raw.sav"))) {
  osf_retrieve_file("https://osf.io/gs4ck") |>
    osf_download(path = here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/gs4ck") |> pull(name)
  file.rename(
    here("data", "raw", file_name),
    here("data", "raw", "0066_hensel_static_exit_raw.sav")
  )
}

df_exit <- haven::read_sav(here("data", "raw", "0066_hensel_static_exit_raw.sav"))

# Join static data
df_static <- full_join(df_enrollment, df_exit) |>
  rename(id = ID)

# save static data
write_tsv(df_static,
          here("data", "raw", "0066_hensel_static_joined_raw.tsv"))



# Cleaning ----------------------------------------------------------------
df <- df |>
  janitor::clean_names() |>
  rename(
    chores = activ_today_1,
    exercise = activ_today_2,
    commute = activ_today_3,
    school_work = activ_today_4,
    medical_appt = activ_today_5,
    alcohol = activ_today_6,
    tobacco = activ_today_7,
    drugs = activ_today_8,
    family_friends = activ_today_9,
    hobby = activ_today_10,
    religious_service = activ_today_11,
    sex = activ_today_12,
    digital_communication = activ_today_13,
    tv_movie = activ_today_14,
    other_activity = activ_today_15,
    other_activity_text = activ_today_15_text,
    nap_rest = activ_today_16,
    ate_meal = activ_today_18,
    activity_prefer_not_answer = activ_today_99
  )

names(df)


#* Get variable names for static and dynamic dataset ---------------------------

# check for ema variables by counts for unique values
counts <- df |>
  group_by(id) |>
  summarise(across(everything(), ~ n_distinct(.))) |>
  # compute mean for every variable across subjects
  summarise(across(where(is.numeric), ~ max(.))) |>
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "n_unique")
counts

# demographic variables: date of birth, is already present in the enrollment data
demographic_vars <- c("dob")

# exclude demographic variables from the main data
df <- df |>
  select(-all_of(demographic_vars))


df <- df |>
  # add beep with 1s for daily data
  dplyr::mutate(beep = 1,
                .after = day)



# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)
check_results

# if it returns "Data are clean.", save the data
# Enter data set ID here
if (check_results == "Data are clean.") {
  write_tsv(df, here("data", "clean", "0066_hensel_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(dataset_id == "0066") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0066") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json,
      here("data", "metadata", "0066_hensel_metadata.json"))

