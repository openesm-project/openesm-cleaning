# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
library(osfr)
source(here("scripts", "functions_data.R"))


# This is Study2 from Contextual Factors Surrounding Emotion Regulation: Replication Study
# https://osf.io/h7kzb/?view_only=


# Data --------------------------------------------------------------------
if(!file.exists(here("data", "raw", "0065_ladis_ts_raw.csv"))){
  osf_retrieve_file("https://osf.io/f82q3") |>
    osf_download(path = here("data", "raw"))


  # rename data
  file_name <- osf_retrieve_file("https://osf.io/f82q3") |> pull(name)
  file.rename(here("data", "raw", file_name), here("data", "raw", "0065_ladis_ts_raw.csv"))
}

df <- read.csv(here("data", "raw", "0065_ladis_ts_raw.csv"))


# Cleaning ----------------------------------------------------------------

df <- df |>
  janitor::clean_names()

names(df)

#* Get variable names for static and dynamic dataset ---------------------------

# check for ema variables by counts for unique values
counts <- df |>
  group_by(subject) |>
  summarise(across(everything(), ~ n_distinct(.))) |>
  # compute mean for every variable across subjects
  summarise(across(everything(), ~ max(.))) |>
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "n_unique")
counts

# ema variables
ema_vars <- counts |>
  filter(n_unique > 1) |>
  pull(variable)

ema_vars

# static variables
static_vars <- c("subject", names(df)[!names(df) %in% ema_vars])
static_vars


#* Misc -------------------------------------------------------------------

# split off demographic data to separate file
df_demographics <- df |>
  select(all_of(static_vars)) |>
  distinct(pick(all_of(static_vars)))


# save demographic data
write_tsv(df_demographics, here("data", "clean", "0065_ladis_static.tsv"))


#* Column Names -----------------------------------------------------------
df <- df |>
  # remove demographic columns from main data
  select(ema_vars) |>
  select(-ends_with("centered"),
         -total_e_rbins) |>
  # rename columns
  dplyr::rename(
    id = subject,
    day = study_day,
    positive_affect = posaff,
    negative_affect = negaff,
    er_no_try = no_try_rt,
    er_accept = accept_rt,
    er_advice = advice_rt,
    er_reappraisal = reappraisal_rt,
    er_problem_solving = prob_solv_rt,
    er_introspection = introspection_rt,
    er_exp_suppress = exp_suppress_rt,
    er_emo_suppress = emo_suppress_rt,
    er_distraction = distract_rt,
    er_number_strategies = total_er,
    social_context = social
  ) |>
  # add beep with NAs
  mutate(beep = NA, .after = day)


# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)
check_results

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "009_ladis_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(id == "0009") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("009") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "009_ladis_metadata.json"))
