# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(jsonlite)
library(osfr)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
if(!file.exists(here("data", "raw", "0031_koval_ts_raw.csv"))){
  osf_retrieve_file("https://osf.io/7sa9k") |>
    osf_download(path = here("data", "raw"))


  # rename data
  file_name <- osf_retrieve_file("https://osf.io/7sa9k") |> pull(name)
  file.rename(here("data", "raw", file_name), here("data", "raw", "0031_koval_ts_raw.csv"))
}

df <- read_delim(here("data", "raw", "0031_koval_ts_raw.csv"),
                 delim = ";", escape_double = FALSE, na = "empty",
                 trim_ws = TRUE)



# Cleaning ----------------------------------------------------------------

#* Column Names -----------------------------------------------------------
df <- df |>
  janitor::clean_names() |>
  dplyr::rename(id = pid,
                day = unit,
                beep = occasion,
                stressed = stress)

#* Misc -------------------------------------------------------------------
# remove aggregate scores
df <- df |>
  dplyr::select(!c(pa, na))


# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
metadata <- read_metadata("0031")

# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, metadata)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0031_koval_ts.tsv"))
}
