# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
library(osfr)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# download file if not already downloaded
if(!file.exists(here("data", "raw", "0046_ringwald_ts_raw.csv"))){
  osf_retrieve_file("https://osf.io/s4jaq") |>
    osf_download(path = here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/s4jaq") |> pull(name)
  file.rename(here("data", "raw", file_name), here("data", "raw", "0046_ringwald_ts_raw.csv"))
}

# read in data
df_raw <- read.csv(here("data", "raw", "0046_ringwald_ts_raw.csv"))



# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
df <- df_raw |>
  janitor::clean_names()


#* Misc -------------------------------------------------------------------
# remove superfluous columns
# grand-mean columns, squared values
# then rename other columns
df <- df |>
  select(!c(ends_with("_p"),
            ends_with("_p2"),
            ends_with("_grand"),
            ends_with("_g2"),
            contains("selfother"))) |>
  # remove non-centered columns for consistency
  select(!c("emp_tot", "emp_cog", "emp_aff")) |>
  # remove _g at the end of variable
  rename_with(~ str_remove(., "_g$")) |>
  rename_with(~ str_replace(., "othr", "other")) |>
  rename_with(~ str_replace(., "emp", "empathy")) |>
  rename(empathy_global = empathy_tot)


# add missing day and beep variables
df$day <- NA
df$beep <- NA


# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0046_ringwald_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(dataset_id == "0046") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0046") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0046_ringwald_metadata.json"))
