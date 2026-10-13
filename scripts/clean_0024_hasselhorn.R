# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(jsonlite)
library(osfr)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# download data
if(!file.exists(here("data", "raw", "0024_hasselhorn_ts_raw.RDa"))){
  osf_retrieve_file("https://osf.io/4n3ug") |>
    osf_download(path = here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/4n3ug") |> pull(name)
  file.rename(here("data", "raw", file_name), here("data", "raw", "0024_hasselhorn_ts_raw.RDa"))
}

# load data
load(here("data", "raw", "0024_hasselhorn_ts_raw.RDa"))

df_raw <- AA.m
rm(AA.m)

# Cleaning ----------------------------------------------------------------

#* Column Names -----------------------------------------------------------
# rename columns
# they use the _corr suffix in the analyses, so I will keep that
df <- df_raw |>
  janitor::clean_names() |>
  rename(
    id = serial,
    well = mdbf1,
    awake = mdbf2,
    relaxed = mdbf3,
    good = mdbf4_r,
    rested = mdbf5_r,
    pleased = mdbf6,
    calm = mdbf7_r,
    happy = mdbf8_r,
    report_easy = kla1,
    report_sure = kla2_r,
    assertive = pers1_corr,
    organized = pers2_corr,
    talkative = pers3_r_corr,
    persistent = pers4_corr,
    outgoing = pers5_r_corr,
    study_burden = bel1,
    study_interfere = bel2,
    study_annoy = bel3
    )




# Due to some implicit missingness, no beep variable can be provided
df$beep <- NA




#* Misc -------------------------------------------------------------------
# remove demograhpic/treatment data (already present in pre-post)
df <- df |>
  select(-c(time))

# remove nonused/differently coded columns
df <- df |>
  select(-c(mdbf4, mdbf5, mdbf7, mdbf8,
            kla2, pers1, pers2, pers3, pers3_corr,
            pers4, pers5, pers5_corr))

# remove seemingly redundant columns
df <- df |>
  select(
    -contains("_na"),  # indicates if a value was NA
    -c(ext, gew, gs, wm, ru, kla, bel)       # aggregate columns
  )

# put reaction time columns to the end
df <- df |>
  select(
    -starts_with("rt"),
    everything(),
    starts_with("rt")
  )


# change empty "missing" column to NA
df <- df |>
  mutate(missing = na_if(missing, ""))



# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
metadata <- read_metadata("0024")

# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, metadata)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0024_hasselhorn_ts.tsv"))
}
