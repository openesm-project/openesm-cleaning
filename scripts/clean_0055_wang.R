# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(osfr)
library(jsonlite)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# download EMA data http://osf.io/fvsjh
if(!file.exists(here::here("data", "raw", "0055_wang_ts_raw.csv"))){
  osf_retrieve_file("http://osf.io/fvsjh") |>
    osf_download(path = here::here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("http://osf.io/fvsjh") |> pull(name)
  file.rename(here::here("data", "raw", file_name), here::here("data", "raw", "0055_wang_ts_raw.csv"))
}

# read data
df_raw <- read.csv(here("data", "raw", "0055_wang_ts_raw.csv"), header = FALSE)

# we then also download the end-of-day survey data
if(!file.exists(here::here("data", "raw", "0055_wang_daily_raw.csv"))){
  osf_retrieve_file("https://osf.io/c2km5") |>
    osf_download(path = here::here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/c2km5") |> pull(name)
  file.rename(here::here("data", "raw", file_name), here::here("data", "raw", "0055_wang_daily_raw.csv"))
}

df_daily <- read.csv(here("data", "raw", "0055_wang_daily_raw.csv"))

# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
# column names of the EMA data are in the last row
col_names <- df_raw[nrow(df_raw), ]

# remove the last row
df <- df_raw[-nrow(df_raw), ]
# set the column names
colnames(df) <- col_names


##* Merging --------------------------------------------------------------
# Now merge EMA with daily data by first harmonizing columns
df_daily <- df_daily |>
  # remove _E in column name
  rename_with(~ str_remove(., "_E")) |>
  rename(sad_daily = Sad) |>
  # add beep etc. information
  mutate(DailyResponse = 7) |>
  rename(Day = Evening) |>
  # count should go from 7 to 145 in steps of seven for each ID
  group_by(ID) |>
  mutate(count = Day * 7) |>
  ungroup()

# remove irrelevant columns and rows such that both data can be merged
df <- df |>
  select(-c(X, Count)) |>
  filter(DailyResponse != 7) |>
  mutate(across(!Time, as.numeric))

df <- bind_rows(df, df_daily) |>
  arrange(ID, Day, DailyResponse)


# then we give proper column names
df <- df |>
  janitor::clean_names() |>
  rename(
    beep = daily_response,
    counter = count,
    anxious = anxiety,
    angry = anger,
    anxiety_daily = anxious,
    anger_daily = angry,
    family_support_daily = fam_coh1,
    family_togetherness_daily = fam_coh2,
    family_backup_daily = fam_coh3,
    family_critcism_daily = fam_con1,
    family_fight_daily = fam_con2
  )

#* Misc -------------------------------------------------------------------
# check character NAs
df <- df |>
  mutate(across(where(is.character), ~ na_if(., "NA"))) |>
  mutate(across(where(is.character), ~ na_if(., "")))

# nicer column order
df <- df |>
  select(id, day, beep, time, counter, order, everything())



# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0055_wang_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(id == "0055") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0055") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0055_wang_metadata.json"))
