# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(jsonlite)
library(osfr)
source(here("scripts", "functions_data.R"))


# Data --------------------------------------------------------------------
# download data
if(!file.exists(here::here("data", "raw", "0047_barkalifa_ts_raw.csv"))){
  osf_retrieve_file("https://osf.io/yjhvr") |>
    osf_download(path = here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/yjhvr") |> pull(name)
  file.rename(here::here("data", "raw", file_name), here::here("data", "raw", "0047_barkalifa_ts_raw.csv"))
}

# read in data
df_raw <- read.csv(here("data", "raw", "0047_barkalifa_ts_raw.csv"))


# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
df <- df_raw |>
  janitor::clean_names() |>
  rename(
    day = diaryday,
    men_anxiety = m_anx,
    men_sadness = m_sad,
    men_vigor = m_vig,
    men_contentment = m_con,
    women_anxiety = w_anx,
    women_sadness = w_sad,
    women_vigor = w_vig,
    women_contentment = w_con
  )


#* Misc -------------------------------------------------------------------
# add beep variable (daily-diary study)
df$beep <- 1

# check if there are any character NAs
df <- df |>
  mutate(across(where(is.character), ~na_if(., "NA"))) |>
  mutate(across(where(is.character), ~na_if(., "")))


# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
metadata <- read_metadata("0047")


# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, metadata)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0047_barkalifa_ts.tsv"))
}
