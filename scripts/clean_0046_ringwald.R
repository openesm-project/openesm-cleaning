# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
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


# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
metadata <- read_metadata("0046")

# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, metadata)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0046_ringwald_ts.tsv"))
}
