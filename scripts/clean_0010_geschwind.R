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
      "https://storage.googleapis.com/plos-corpus-prod/10.1371/journal.pone.0060188/1/pone.0060188.s004.txt?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=wombat-sa%40plos-prod.iam.gserviceaccount.com%2F20250722%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20250722T074654Z&X-Goog-Expires=86400&X-Goog-SignedHeaders=host&X-Goog-Signature=a12cb1738aa1871e104338be40f38cd6d2972cc4cf86c7d529ee2f43791c63412cbc25761dcd4922447afb029b1025c74e1b2ef7b36cce7cf6d6110f0e57deb9878f811dd506d020dcfb03cda287015bcbddce216d5305bb5c544dda6d7117a75f88e28752b80a1170a93664fbf134fe856db6e5327c77e2f31549889daf6b2cb710a883356a433d426deb0ee6ae2624e563e3cf5a3beef84dae0ac0b8a504c2819365e3961954f077586976f225e0af3808a44295b14456e450bbabb80692e40312a2f66785fdd0f09640e9cac6ff5de5c1e62a2b11d9ff069ac9ceac1b27b9a1fc569945a7c0cba732c1e58b7a4517dd22794f7230ce9c2cdd22d64031a642"
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
