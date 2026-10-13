# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(jsonlite)
library(arrow)
source(here("scripts", "functions_data.R"))


# Data --------------------------------------------------------------------
# read in merged data (https://zenodo.org/records/14190139/files/merged_df.feather)
# data were merged here: https://github.com/DigitalHealthcareLab/24PanicPrediction/blob/master/src/3_data_merge/data_merge.py
df_raw <- arrow::read_feather(here("data", "raw", "0017_jang_ts_raw.feather"))

# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
df <- df_raw |>
  janitor::clean_names() |>
  rename(
    negative_feeling = negative,
    positive_energy = positive_e,
    negative_energy = negative_e,
    anxiety_control = acq,
    agoraphobia = appq_1,
    social_phobia = appq_2,
    interoceptive_fear = appq_3,
    body_shape = bsq,
    fear_negative_evaluation = bfne,
    depression_ces = ces_d,
    generalized_anxiety = gad_7,
    occupational_stress = kosssf,
    depression_phq = phq_9,
    social_avoidance_distress = sads,
    state_anxiety = stai_x1
  )


#* Misc -------------------------------------------------------------------
# find columns which have at most 1 distinct value for all IDs
demographic_vars <- df |>
  group_by(id) |>
  summarise(across(everything(), ~ n_distinct(.))) |>
  pivot_longer(cols = -id, names_to = "variable", values_to = "n_distinct") |>
  # filter out variables who have more than 1 distinct value
  group_by(variable) |>
  summarize(max_distinct = max(n_distinct)) |>
  # find variables with only 1 distinct value
  filter(max_distinct == 1) |>
  pull(variable)

# split off demographic vars
demographic_df <- df |>
  select(c(id), all_of(demographic_vars)) |>
  distinct()

# save demographic vars
write_tsv(demographic_df, here("data", "clean", "0017_jang_static.tsv"))

# remove demographic variables
df <- df |>
  select(-all_of(demographic_vars))

# remove some computed columns which are not needed
df <- df |>
  select(-c(hr_acrophase_difference, hr_acrophase_difference_2d,
            hr_amplitude_difference, hr_amplitude_difference_2d,
            hr_mesor_difference, hr_mesor_difference_2d))

# remove unclear column foot_var, which could be the variance of steps
# but the values are somewhat implausible
df <- df |>
  select(-foot_var)

# Create beep variable
df$beep <- 1

# For each participant, create day variable
df <- df |>
  group_by(id) |>
  mutate(
    day = as.integer(as.Date(date) - min(as.Date(date))) + 1
  ) |>
  ungroup()

# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
metadata <- read_metadata("0017")


# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, metadata)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0017_jang_ts.tsv"))
}
