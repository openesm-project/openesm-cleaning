# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
library(osfr)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# download data
if(!file.exists(here("data", "raw", "0025_hasselhorn_ts_raw.RDa"))){
  osf_retrieve_file("https://osf.io/esa8y") |>
    osf_download(path = here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/esa8y") |> pull(name)
  file.rename(here("data", "raw", file_name), here("data", "raw", "0025_hasselhorn_ts_raw.RDa"))
}

# load data
load(here("data", "raw", "0025_hasselhorn_ts_raw.RDa"))

df_raw <- AA
rm(AA)


# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
df <- df_raw |>
  janitor::clean_names() |>
  rename(
    id = serial,
    well = st02_01,
    awake = st02_02,
    good = st04_03_r,
    calm = st04_04_r,
    rested = st04_06_r,
    relaxed = st02_05,
    pleased = st02_07,
    happy = st04_08_r,
    not_bashful = sp01_01_r,
    bold = sp01_02,
    energetic = sp01_03,
    extraverted = sp01_04,
    not_quiet = sp01_05_r,
    not_shy = sp01_06_r,
    talkative = sp01_07,
    not_withdrawn = sp01_08_r,
    not_careless = sp01_09_r,
    not_disorganized = sp01_10_r,
    # efficient = sp01_11,
    not_inefficient = sp01_12_r,
    # not_organized = sp01_13_r,
    # practical = sp01_14,
    not_sloppy = sp01_15_r,
    # systematic = sp01_16,
    # creative = sp01_17,
    envious = sp01_18_r,
    not_unsympathetic = sp01_19_r,
    # deep = sp01_20,
    # not_fretful = sp01_22_r,
    not_harsh = sp01_23_r,
    not_relaxed = sp01_30_r,
    not_rude = sp01_31_r,
    not_uncreative = sp01_35_r,
    not_unintellectual = sp01_36_r,
    not_cold = sp01_38_r,
    study_burden = sb01_01,
    study_interfere = sb01_02,
    study_annoy = sb01_03
  )


#* Misc -------------------------------------------------------------------
# remove variables already available in static data set
df <- df |>
  select(!c(treatment, treatment_short0_long1, bel))

# change all NAN to NA
df[df == "NaN"] <- NA

# remove variables which were included as reverse-coded or are otherwise not relevant
df <- df |>
  select(!c(st04_03, st04_04, st04_06, st04_08, sp01_01, sp01_05, sp01_06, sp01_08,
            # centered and aggregate columns
            ends_with("_cwc"), gs, ext, gew))

# no beep information available
df$beep <- NA



# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0025_hasselhorn_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(dataset_id == "0025") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0025") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0025_hasselhorn_metadata.json"))
