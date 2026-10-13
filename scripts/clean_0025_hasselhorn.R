# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
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

# recode all reverse-coded variables
df <- df |>
  mutate(
    quiet = 6 - not_quiet,
    bashful = 6 - not_bashful,
    withdrawn = 6 - not_withdrawn,
    careless = 6 - not_careless,
    disorganized = 6 - not_disorganized,
    inefficient = 6 - not_inefficient,
    sloppy = 6 - not_sloppy,
    unsympathetic = 6 - not_unsympathetic,
    harsh = 6 - not_harsh,
    relaxed_personality = 6 - not_relaxed,
    rude = 6 - not_rude,
    shy = 6 - not_shy,
    uncreative = 6 - not_uncreative,
    unintellectual = 6 - not_unintellectual,
    cold = 6 - not_cold
  ) |>
  # remove original reverse-coded variables
  select(
    !c(
      not_quiet,
      not_bashful,
      not_withdrawn,
      not_careless,
      not_disorganized,
      not_inefficient,
      not_sloppy,
      not_unsympathetic,
      not_harsh,
      not_relaxed,
      not_rude,
      not_uncreative,
      not_unintellectual,
      not_cold,
      not_shy
    )
  )

# change all NAN to NA
df[df == "NaN"] <- NA

# remove variables which were included as reverse-coded or are otherwise not relevant
df <- df |>
  select(!c(st04_03, st04_04, st04_06, st04_08, sp01_01, sp01_05, sp01_06, sp01_08,
            # centered and aggregate columns
            ends_with("_cwc"), gs, ext, gew))

# no beep information available
df$beep <- NA



# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
metadata <- read_metadata("0025")

# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, metadata)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0025_hasselhorn_ts.tsv"))
}
