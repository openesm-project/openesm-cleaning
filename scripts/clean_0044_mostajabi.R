# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
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
saveRDS(df_demographics, here("data", "clean", "0044_mostajabi_static.csv"))

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

# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
metadata <- read_metadata("0044")

# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, metadata)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0044_mostajabi_ts.tsv"))
}
