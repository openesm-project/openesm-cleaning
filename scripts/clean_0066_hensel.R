# Packages ----------------------------------------------------------------
library(tidyverse)
library(haven)
library(here)
library(googlesheets4)
library(jsonlite)
library(osfr)
source(here::here("scripts", "functions_data.R"))


# Data --------------------------------------------------------------------
## Time series
if (!file.exists(here::here("data", "raw", "0066_hensel_ts_raw.sav"))) {
  osf_retrieve_file("https://osf.io/mtxfe") |>
    osf_download(path = here::here("data", "raw"))


  # rename data
  file_name <- osf_retrieve_file("https://osf.io/mtxfe") |> pull(name)
  file.rename(here::here("data", "raw", file_name),
              here::here("data", "raw", "0066_hensel_ts_raw.sav"))
}

df <- haven::read_sav(here::here("data", "raw", "0066_hensel_ts_raw.sav"))


## Static enrollment
if (!file.exists(here::here("data", "raw", "0066_hensel_static_enrollment_raw.sav"))) {
  osf_retrieve_file("https://osf.io/n7bcr") |>
    osf_download(path = here::here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/n7bcr") |> pull(name)
  file.rename(
    here::here("data", "raw", file_name),
    here::here("data", "raw", "0066_hensel_static_enrollment_raw.sav")
  )
}

df_enrollment <- haven::read_sav(here::here("data", "raw", "0066_hensel_static_enrollment_raw.sav"))


# static exit
if (!file.exists(here::here("data", "raw", "0066_hensel_static_exit_raw.sav"))) {
  osf_retrieve_file("https://osf.io/gs4ck") |>
    osf_download(path = here::here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/gs4ck") |> pull(name)
  file.rename(
    here::here("data", "raw", file_name),
    here::here("data", "raw", "0066_hensel_static_exit_raw.sav")
  )
}

df_exit <- haven::read_sav(here::here("data", "raw", "0066_hensel_static_exit_raw.sav"))

# Join static data
df_static <- dplyr::full_join(df_enrollment, df_exit) |>
  dplyr::rename(id = ID)

# save static data
write_tsv(df_static,
          here::here("data", "raw", "0066_hensel_static_joined_raw.tsv"))



# Cleaning ----------------------------------------------------------------
df <- df |>
  janitor::clean_names() |>
  dplyr::rename(
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
    activity_prefer_not_answer = activ_today_99,
    # better names
    amount_stool = amt_stool,
    amount_urine = amt_urine,
    anxious_stool = anx_stool,
    anxious_stool_possibility = anx_stool_poss,
    anxious_urine = anx_urine,
    anxious_urine_possibility = anx_urine_poss,
    prevent_activity_stool = noact_stool,
    prevent_activity_urine = noact_urine,
    stress_stool_possibility = stress_stool_poss,
    stress_urine_possibility = stress_urine_poss
  )

names(df)


#* Get variable names for static and dynamic dataset ---------------------------
# check for ema variables by counts for unique values
counts <- df |>
  dplyr::group_by(id) |>
  dplyr::summarise(across(everything(), ~ n_distinct(.))) |>
  # compute mean for every variable across subjects
  dplyr::summarise(across(where(is.numeric), ~ max(.))) |>
  tidyr::pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "n_unique")


# demographic variables: date of birth, is already present in the enrollment data
demographic_vars <- c("dob")

# exclude demographic variables from the main data
df <- df |>
  dplyr::select(-all_of(demographic_vars))


df <- df |>
  # add beep with 1s for daily data
  dplyr::mutate(beep = 1,
                .after = day)


# remove

# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)
check_results

# if it returns "Data are clean.", save the data
# Enter data set ID here::here
if (check_results == "Data are clean.") {
  write_tsv(df, here::here("data", "clean", "0066_hensel_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here::here
sheet_url <- meta_data |>
  dplyr::filter(dataset_id == "0066") |>
  dplyr::pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0066") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json,
      here::here("data", "metadata", "0066_hensel_metadata.json"))

