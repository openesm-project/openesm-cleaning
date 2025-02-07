# Packages ----------------------------------------------------------------
library(tidyverse)
library(osfr)
library(here)
library(googlesheets4)
library(jsonlite)
source(here("scripts", "functions_data.R"))


# Data --------------------------------------------------------------------
if(!file.exists(here("data", "raw", "0002_nestler_ts_raw.txt"))){
  osf_retrieve_file("https://osf.io/gmz7e") |>
    osf_download(path = here("data", "raw"))


  # rename data to 0001_fried.csv
  file_name <- osf_retrieve_file("https://osf.io/gmz7e") |> pull(name)
  file.rename(here("data", "raw", file_name), here("data", "raw", "0002_nestler_ts_raw.txt"))
}

df <- read.table(here("data", "raw", "0002_nestler_ts_raw.txt"), header = TRUE)



# Cleaning ----------------------------------------------------------------
#* Column names ------------------------------------------------------------
df <- df |>
  janitor::clean_names()

#* Misc --------------------------------------------------------------------
# add beep column (although irrelevant here)
df$beep <- 1

# remove person-specific means and modeling columns
df <- df |>
  select(-starts_with("m_")) |>
  select(-c(train, last))

# split off demographic data to separate file
df_demographics <- df |>
  group_by(id) |>
  distinct(no_meas, age, sex, pa, na, swls, pow, ach, aff, int, fear)

# save demographic data
write_tsv(df_demographics, here("data", "clean", "0002_nestler_static.tsv"))

# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0002_nestler_ts.tsv"))
}



# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)

sheet_url <- meta_data |>
  filter(id == "0002") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)


meta_json <- create_metadata_json("0002") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0002_nestler_metadata.json"))
