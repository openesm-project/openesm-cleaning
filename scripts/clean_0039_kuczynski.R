# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
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


# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0039_kuczynski_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(dataset_id == "0039") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0039") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0039_kuczynski_metadata.json"))
