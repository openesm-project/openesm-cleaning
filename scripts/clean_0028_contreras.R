# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
library(osfr)
library(haven)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# download data
if(!file.exists(here("data", "raw", "0028_contreras_ts_raw.sav"))){
  osf_retrieve_file("https://osf.io/8ewyn") |>
    osf_download(path = here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/8ewyn") |> pull(name)
  file.rename(here("data", "raw", file_name), here("data", "raw", "0028_contreras_ts_raw.sav"))
}

df_raw <- read_sav(here("data", "raw", "0028_contreras_ts_raw.sav"))


# Cleaning ----------------------------------------------------------------

#* Column Names -----------------------------------------------------------
# rename columns
df <- df_raw |>
  janitor::clean_names() |>
  rename(id = subject,
         useless = se1,
         manage_well = se2,
         no_trust = par1,
         harm = par2,
         criticism = par3,
         avoid = ea)


#* Misc -------------------------------------------------------------------
# remove average columns
df <- df |>
  select(-contains("aver"))





# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0028_contreras_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(id == "0028") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0028") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0028_contreras_metadata.json"))
