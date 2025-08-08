# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
library(osfr)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
if(!file.exists(here::here("data", "raw", "0048_barkalifa_ts_raw.csv"))){
  osf_retrieve_file("https://osf.io/2gc69") |>
    osf_download(path = here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/2gc69") |> pull(name)
  file.rename(here::here("data", "raw", file_name), here::here("data", "raw", "0048_barkalifa_ts_raw.csv"))
}

# read in data
df_raw <- read.csv(here("data", "raw", "0048_barkalifa_ts_raw.csv"))


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

# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0048_barkalifa_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(dataset_id == "0048") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0048") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0048_barkalifa_metadata.json"))
