# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
library(osfr)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# download directly from OSF
if(!file.exists(here("data", "raw", "0041_wright_ts_raw.zip"))){
  osf_retrieve_file("https://osf.io/5x8rv") |>
    osf_download(path = here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/5x8rv") |> pull(name)
  file.rename(here("data", "raw", file_name), here("data", "raw", "0041_wright_ts_raw.zip"))
}

# unzip and read into one df
unzip(here("data", "raw", "0041_wright_ts_raw.zip"),
      exdir = here("data", "raw", "0041_wright_ts_raw"))

df_raw <- list.files(here("data", "raw", "0041_wright_ts_raw", "Individual Level Data"),
                     pattern = "*.csv",
                     full.names = TRUE) |>
  # read as dataframe,
  map_dfr(~read_csv(.x, col_names = TRUE) |>
            mutate(id = str_remove(basename(.x), "func7_"))) |>
  mutate(id = gsub(".csv", "", id))


# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
df <- df_raw |>
  janitor::clean_names() |>
  dplyr::rename(
    stressed = stress,
    pa = pos_aff,
    na = neg_aff
  )



#* Misc -------------------------------------------------------------------



# day and beep column
df$day <- NA
df$beep <- 1

# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0041_wright_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(dataset_id == "0041") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0041") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0041_wright_metadata.json"))
