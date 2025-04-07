# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
library(osfr)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# download data
if(!file.exists(here("data", "raw", "0033_fisher_ts_raw.zip"))){
  osf_retrieve_file("https://osf.io/mgdp6") |>
    osf_download(path = here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/mgdp6") |> pull(name)
  file.rename(here("data", "raw", file_name), here("data", "raw", "0033_fisher_ts_raw.zip"))
}

# unzip the data and load it
unzip(here("data", "raw", "0033_fisher_ts_raw.zip"), exdir = here("data", "raw", "0033_fisher"))

# for each .RData file in the folder, load it, save the "data" in a list, then delete the rest
files <- list.files(here("data", "raw", "0033_fisher", "R Data"), pattern = "\\.RData$", full.names = TRUE)
df_list <- list()
for (file in files) {
  load(file)
  df_list[[file]] <- data
}

# use the file name without the extension as the key
df_list <- df_list |>
  set_names(gsub("\\.RData$", "", basename(files)))

# combine into one data frame with id column based on the key
df_raw <- df_list |>
  bind_rows(.id = "id") |>
  mutate(id = gsub("_final", "", id))



# Cleaning ----------------------------------------------------------------

#* Column Names -----------------------------------------------------------
df <- df_raw |>
  janitor::clean_names()


#* Misc -------------------------------------------------------------------
# convert to date columns
df <- df |>
  mutate(across(c(start, finish), ~as.POSIXct(., format = "%m/%d/%Y %H:%M")))

# remove irrelevant columns
df <- df |>
  select(-c(lag, tdif, cumsum_t, x29, x30, x31))

# for each person, create a numerical day indicator from the first to the last day of the study
df <- df |>
  mutate(date = as.Date(start)) |>
  # add day number
  group_by(id) |>
  mutate(day = dense_rank(date)) |>
  ungroup() |>
  select(!date)

# create empty beep column as we cannot create it
# from the data
df$beep <- NA



# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0033_fisher_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(id == "0033") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0033") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0033_fisher_metadata.json"))
