# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(jsonlite)
library(osfr)
source(here::here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# download data
if(!file.exists(here("data", "raw", "0039_kuczynski_ts_raw.csv"))){
  osf_retrieve_file("https://osf.io/huz67") |>
    osf_download(path = here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/huz67") |> pull(name)
  file.rename(here("data", "raw", file_name), here("data", "raw", "0039_kuczynski_ts_raw.csv"))
}
# read data
df_raw <- read_csv(here("data", "raw", "0039_kuczynski_ts_raw.csv"))




# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
# rename columns
df <- df_raw |>
  janitor::clean_names()

df <- df |>
  rename(
    id = pid,
    depressed = depressedmood,
    left_out = leftout,
    social_interaction = socialintgross,
    perceived_responsiveness = ppr,
    covid_anxiety = anxietycovid
  )

#* Misc -------------------------------------------------------------------
# arrange data
df <- df |>
  arrange(id, date)

# create day column for each id
df <- df |>
  group_by(id) |>
  mutate(
    day = as.integer(date - min(date)) + 1
  ) |>
  ungroup()

# create beep = 1
df$beep <- 1

# remove weekend column
df <- df |>
  select(-weekend)


# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
metadata <- read_metadata("0039")

# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, metadata)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0039_kuczynski_ts.tsv"))
}
