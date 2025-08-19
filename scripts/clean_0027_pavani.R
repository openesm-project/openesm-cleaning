# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
library(osfr)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# download data
# we download "DF_2" here because it contains more info than "DF_1"
if(!file.exists(here("data", "raw", "0027_pavani_ts_raw.xlsx"))){
  osf_retrieve_file("https://osf.io/traf4") |>
    osf_download(path = here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/traf4") |> pull(name)
  file.rename(here("data", "raw", file_name), here("data", "raw", "0027_pavani_ts_raw.xlsx"))
}

# load data
df_raw <- read_xlsx(here("data", "raw", "0027_pavani_ts_raw.xlsx"))



# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
# rename columns
df <- df_raw |>
  janitor::clean_names() |>
  rename(
    id = num,
    counter = nobs1
  )


# remove lag-2 variables
df <- df |>
  select(-contains("2"))

# remove the "1" from all variable names
df <- df |>
  rename_with(~str_remove(., "1"), -id, -counter)

df <- df |>
  rename(
    reappraisal = pr,
    distraction = dis,
    suppression = sup,
    appreciation = app,
    rumination = rum,
    problem_coping = pfc,
    sharing_affect = ssa
  )


#* Misc -------------------------------------------------------------------
# shift all data forward by one, because the columns represent lagged effects
df <- df |>
  mutate(counter = counter + 1)


# add implicit missings to obtain beep and day column
n_ids <- unique(df$id) |>
  length()

df <- df |>
  tidyr::complete(id, counter = 1:70) |>
  # 5 beeps per day for 14 days for each participant
  dplyr::mutate(beep = rep(1:5, times = 14 * n_ids)) |>
  # repeat day id 5 times
  dplyr::mutate(day = rep(1:14, each = 5, times = n_ids))




# remove aggregate computations from data
df_demographics <- df |>
  filter(!is.na(e)) |>
  filter(!is.na(n)) |>
  distinct(id, e, n) |>
  rename(
    extraversion = e,
    neuroticism = n
  )

# save the demographics data
write_tsv(df_demographics, here("data", "clean", "0027_pavani_static.tsv"))

# remove demographic columns from main data
df <- df |>
  select(-c(e, n))




# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0027_pavani_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(dataset_id == "0027") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0027") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0027_pavani_metadata.json"))
