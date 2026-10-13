# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(jsonlite)
library(osfr)
source(here::here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
if(!file.exists(here("data", "raw", "0032_grommisch_ts_raw.rda"))){
  osf_retrieve_file("https://osf.io/r7jw6/files/osfstorage/5da03fbc26eb50000b7c0da6") |>
    osf_download(path = here("data", "raw"))


  # rename data
  file_name <- osf_retrieve_file("https://osf.io/r7jw6/files/osfstorage/5da03fbc26eb50000b7c0da6") |> pull(name)
  file.rename(here("data", "raw", file_name), here("data", "raw", "0032_grommisch_ts_raw.rda"))
}

load(here("data", "raw", "0032_grommisch_ts_raw.rda"))
df <- data



# Cleaning ----------------------------------------------------------------

#* Column Names -----------------------------------------------------------
df <- df |>
  janitor::clean_names() |>
  dplyr::rename(id = sema_id,
                day = day_nr,
                counter = row_nr,
                happy = hap,
                relaxed = rlx,
                confident = conf,
                sad = sad,
                stressed = str,
                angry = ang,
                situation_selection = sitsel,
                situation_modification = sitmod,
                reappraisal = reap,
                acceptance = acpt,
                rumination = rum,
                social_sharing = socshr,
                ignoring = ignr,
                suppression = supr
                ) |>
  mutate(beep = NA, .after = day)

#* Misc -------------------------------------------------------------------
# split off demographic data to separate file
df_demographics <- df |>
  group_by(id) |>
  distinct(
    id,
    age_yrs,
    gender,
    dass_d_agg,
    dass_a_agg,
    dass_s_agg,
    swls_agg,
    pos_a_agg,
    neg_a_agg
  )

# save demographic data
write_tsv(df_demographics, here("data", "clean", "0032_grommisch_static.tsv"))

# remove demographic columns from main data
df <- df |>
  select(
    -c(
      age_yrs,
      gender,
      dass_d_agg,
      dass_a_agg,
      dass_s_agg,
      swls_agg,
      pos_a_agg,
      neg_a_agg
    )
  )

# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
metadata <- read_metadata("0032")

# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, metadata)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0032_grommisch_ts.tsv"))
}
