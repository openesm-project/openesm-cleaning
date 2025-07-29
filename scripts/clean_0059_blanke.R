# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
library(osfr)
library(haven)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# download file if not already downloaded
if(!file.exists(here("data", "raw", "0059_blanke_ts_raw.sav"))){
  osf_retrieve_file("https://osf.io/y7qsn") |>
    osf_download(path = here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/y7qsn") |> pull(name)
  file.rename(here("data", "raw", file_name), here("data", "raw", "0059_blanke_ts_raw.sav"))
}

# read in data
df_raw <- read_sav(here("data", "raw", "0059_blanke_ts_raw.sav"))


# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
# rename columns
df <- df_raw |>
  janitor::clean_names() |>
  rename(
    id = id_anonym,
    counter = a_ftl,
    miss_status = a_miss,
    time = a_time,
    attention_present = m_att1,
    opened_up = m_att2,
    concentrated_present = m_att3,
    nonjudgmental_feelings = m_noju1,
    nonjudgmental_thoughts = m_noju2,
    nonjudgmental_behavior = m_noju3,
    happy = m_aff1,
    content = m_zufr,
    relaxed = m_entsp,
    downhearted = m_nieder,
    distressed = m_bekuem,
    nervous = m_nerv,
    ruminate_feelings = m_rum1,
    ruminate_things = m_rum2,
    relaxed_thoughts_feelings = m_refl1,
    relaxed_thoughts_things = m_refl2
  )




#* Misc -------------------------------------------------------------------
# convert time column to POSIXct
df <- df |>
  mutate(
    time = as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  )

# remove irrelevant observations which, apparently, were no longer part of EMA
df <- df |>
  filter(counter <= 54)


# arrange per individual based on time
df <- df |>
  arrange(id, time)

# create day and beep column, with 6 beeps for 9 days
df <- df |>
  group_by(id) |>
  mutate(
    day = rep(seq(1, 9), each = 6),
    beep = rep(seq(1, 6), times = 9)
  ) |>
  ungroup()

# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0059_blanke_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(id == "0059") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0059") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0059_blanke_metadata.json"))
