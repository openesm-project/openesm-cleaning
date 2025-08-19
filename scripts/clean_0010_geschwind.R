# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
library(osfr)
# install.packages("RCurl")
library(RCurl)
source(here("scripts", "functions_data.R"))





# Data --------------------------------------------------------------------
if (!file.exists(here("data", "raw", "0010_geschwind_ts_raw.rda"))) {
  df <- read.csv(textConnection(
    getURL(
      "https://storage.googleapis.com/plos-corpus-prod/10.1371/journal.pone.0060188/1/pone.0060188.s004.txt?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=wombat-sa%40plos-prod.iam.gserviceaccount.com%2F20250819%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20250819T115748Z&X-Goog-Expires=86400&X-Goog-SignedHeaders=host&X-Goog-Signature=1fae82c216975b4e043dbb1eef29990717316a2a0f9515313077c6ba762203eb5ad353e905b4feb619d867006bac80aa1c8432aeb9704cc7299030d95bb3122d8f68dc2a73331a6b2d67269ef6d5fae38dd322d1d19cabb51c00a1624b7f2aa938b1c3f739f4e0eca33c86258f8ebf2c30b92543ddab50d9493ceb2e46666ccd3e8fce49321649d228b9593a575470068797f777cb369a44236b92ba212e9ee6259bcb3b4ca1a4e779ab5218ff7babed11d8fb4b3d7d6b2375abc24c93e19022569ef25e6862ce39785b217990b5c4b9088265e4ab82c2ac8e0a5968686a46acfa9a3cd87e9150be64a3c689e7d431232c9b576b4ddc53df4f8f13a3f08111a0"
    )
  ))
  write.csv2(df,
             here("data", "raw", "0010_geschwind_ts_raw.csv"),
             row.names = FALSE)
}


df <- read.csv2(here("data", "raw", "0010_geschwind_ts_raw.csv"))



# Cleaning ----------------------------------------------------------------

#* Column Names -----------------------------------------------------------
df <- df |>
  janitor::clean_names() |>
  dplyr::rename(
    id = subjno,
    day = dayno,
    beep = beepno,
    therapy = informat04,
    study_period = st_period,
    # affect variables
    cheerful = opgewkt,
    pleasantness = onplplez,
    worried = pieker,
    fearful = angstig,
    sad = somber,
    relaxed = ontspann,
    neuroticism = neur
  )

#* Misc -------------------------------------------------------------------
# split off demographic data to separate file
df_demographics <- df |>
  group_by(id) |>
  distinct(id, therapy, neuroticism)

# save demographic data
write_tsv(df_demographics,
          here("data", "clean", "0010_geschwind_static.tsv"))

# remove demographic columns from main data
df <- df |>
  select(-c(therapy, neuroticism))

# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)
check_results

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0010_geschwind_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(dataset_id == "0010") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0010") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0010_geschwind_metadata.json"))
