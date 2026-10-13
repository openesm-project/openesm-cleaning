# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(jsonlite)
library(osfr)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# download data
if(!file.exists(here("data", "raw", "0026_fernandez_ts_raw.RData"))){
  osf_retrieve_file("https://osf.io/jvms7") |>
    osf_download(path = here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/jvms7") |> pull(name)
  file.rename(here("data", "raw", file_name), here("data", "raw", "0026_fernandez_ts_raw.RData"))
}

# load data
load(here("data", "raw", "0026_fernandez_ts_raw.RData"))
df_raw <- dat1
rm(dat1)


# Cleaning ----------------------------------------------------------------

#* Column Names -----------------------------------------------------------
# rename columns
df <- df_raw |>
  janitor::clean_names() |>
  rename(
    gender = woman,
    all_smartphone_pre = all_pre,
    communication_pre = com_pre,
    social_media_pre = sm_pre,
    other_pre = oth_pre,
    all_smartphone_post = all_post,
    communication_post = com_post,
    social_media_post = sm_post,
    other_post = oth_post
  )

# add empty beep and day columns
df$beep <- NA
df$day <- NA

#* Misc -------------------------------------------------------------------
# remove aggregate columns
df <- df |>
  select(-c(contains("_pmc"), contains("_pm")))

# remove redundant columns
df <- df |>
  select(-c(nr, dataset, timescale_before_esm))


# split off gender column to demographics
df_demographic <- df |>
  distinct(id, gender)

# save demographic data
write_tsv(df_demographic, here("data", "clean", "0026_fernandez_static.tsv"))

# remove demographic columns from main data
df <- df |>
  select(!gender)


# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
metadata <- read_metadata("0026")

# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, metadata)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0026_fernandez_ts.tsv"))
}
