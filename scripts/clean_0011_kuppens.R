# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(jsonlite)
library(readr)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# Read in data
df_raw <- read_delim(here("data", "raw", "0011_kuppens_ts_raw.csv"),
                  delim = ";",
                  col_names = TRUE,
                  trim_ws = TRUE,
                  na = c("9998", "9999"))

# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
# Rename columns to snake_case
df <- df_raw |>
  janitor::clean_names() |>
  rename(
    id = id_1,
    angry = kwaad,
    depressed = depre,
    sad = droev,
    anxious = angst,
    relaxed = ontsp,
    happy = blij
  )


#* Misc -------------------------------------------------------------------
# remove lagged and superfluous columns
df <- df |>
  select(-c(contains("_1")))

# split off neuroticism to static data file
all_ids <- unique(df$id)

# split of the neuroticism column, then reattach it to the correct id
df_neuroticism <- df |>
  select(id, neuroticism_score) |>
  slice(1:95) |>
  # attach the correct id
  mutate(id = all_ids)


# remove neuroticism from df
df <- df |>
  select(-neuroticism_score)

# save static data
write_tsv(df_neuroticism, here("data", "clean", "0011_kuppens_static.tsv"))

# add empty day and beep variable
df <- df |>
  mutate(
    day = NA,
    beep = NA
  )


# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
metadata <- read_metadata("0011")

# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, metadata)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0011_kuppens_ts.tsv"))
}
