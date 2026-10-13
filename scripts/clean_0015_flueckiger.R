# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(jsonlite)
library(haven)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
df_raw <- read_sav(here::here("data", "raw", "0015_flueckiger_ts_raw.sav"))

# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
df <- df_raw |>
  janitor::clean_names() |>
  rename(
    sleep_quality = sq,
    physical_activity = phys_act,
    learning_goal_achievement = lga
  )


#* Misc -------------------------------------------------------------------
# add beep column (once per day)
df <- df |>
  mutate(beep = 1)


# recode -99 to NA
df <- df |>
  mutate(across(where(is.numeric), ~na_if(., -99))) |>
  mutate(across(where(is.character), ~na_if(., "-99")))

# arrange data set
df <- df |>
  arrange(id, day)


# split off demograhpic data
cols_demo <- c("bdi", "age", "sex", "sem", "exam", "hsg")

df_demographics <- df |>
  group_by(id) |>
  distinct(across(all_of(cols_demo))) |>
  ungroup()

# save demographic data
write_tsv(df_demographics, here("data", "clean", "0015_flueckiger_static.tsv"))

# remove demographic columns from main data
df <- df |>
  select(-all_of(cols_demo))


# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
metadata <- read_metadata("0015")


# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, metadata)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0015_flueckiger_ts.tsv"))
}
