# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
library(osfr)
source(here("scripts", "functions_data.R"))



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
                occasion = row_nr,
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

# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)
check_results

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0032_grommisch_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(id == "0032") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0032") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0032_grommisch_metadata.json"))
