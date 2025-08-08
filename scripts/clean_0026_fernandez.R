# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
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


# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0026_fernandez_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(dataset_id == "0026") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0026") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0026_fernandez_metadata.json"))
