# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
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

# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
metadata <- read_metadata("0041")

# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, metadata)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0041_wright_ts.tsv"))
}
