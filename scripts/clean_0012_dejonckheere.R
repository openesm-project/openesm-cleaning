# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(jsonlite)
library(haven)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
df_raw <- haven::read_sav(here::here("data", "raw", "0012_dejonckheere_ts_raw.sav"))


# Cleaning ----------------------------------------------------------------

#* Column Names -----------------------------------------------------------
# Rename columns
df <- df_raw |>
  janitor::clean_names() |>
  rename(
    id = pid,
    counter = beepnum,
    angry = anger,
    stressed = stress
  )

# For each id, create a new beep column (7x/day) and a day column (14 days)
df <- df |>
  arrange(id) |>
  mutate(
    beep = rep(rep(1:7, times = 14), times = 100),
    day = rep(rep(1:14, each = 7), times = 100)
  )

#* Misc -------------------------------------------------------------------
# split off cross-sectional information
df_demographics <- df |>
  group_by(id) |>
  distinct(ces_dmean, bd_imean, era, rrs_br)

# save the demographics data
write_tsv(df_demographics, here("data", "clean", "0012_dejonckheere_static.tsv"))

# remove cross-sectional information from the main data
df <- df |>
  select(-ces_dmean, -bd_imean, -era, -rrs_br)

# remove aggregate computations from data
df <- df |>
  select(-c(pa, na))



# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
metadata <- read_metadata("0012")


# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, metadata)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0012_dejonckheere_ts.tsv"))
}
