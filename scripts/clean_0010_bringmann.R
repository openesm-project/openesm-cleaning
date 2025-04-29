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
if (!file.exists(here("data", "raw", "0010_bringmann_ts_raw.rda"))) {
  df <- read.csv(textConnection(
    getURL(
      "https://storage.googleapis.com/plos-corpus-prod/10.1371/journal.pone.0060188/1/pone.0060188.s004.txt?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=wombat-sa%40plos-prod.iam.gserviceaccount.com%2F20250429%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20250429T141620Z&X-Goog-Expires=86400&X-Goog-SignedHeaders=host&X-Goog-Signature=1f3ea0660fbb6ee1fcf2c38bbd85357f2a4bcff4d5378711d82499ec246a27bc54fbd3924b5052655e5129dd661c8748dbe72b63a71928e74308ef91d3cb0f70b57a01d381795d2283134b3394fb4ab7ffe0b44b295147556e3c6c3956b831d28328ae8bdf2247323bad7e51ce4c8187c7aaf99508256cbe48cfe4c35eba192dfb9fec184ed87bc489a4579b37ab4de11be8ae1138eff7b72bae334af1de7594e843b96e9d204cb85aadb72d7169e1b73085b5b07bb0420cb939f8a168815206ddde18ae4b22cdb22ed932e39dd93cc785592b9b323b897371a2f0605b0292d2d38911b129cae874ad14857ff724dbcd53a1089d2f847367af6518f77cea60ec"
    )
  ))
  write.csv2(df,
             here("data", "raw", "0010_bringmann_ts_raw.csv"),
             row.names = FALSE)
}


df <- read.csv2(here("data", "raw", "0010_bringmann_ts_raw.csv"))



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
    worry = pieker,
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
          here("data", "clean", "0010_bringmann_static.tsv"))

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
  write_tsv(df, here("data", "clean", "0010_bringmann_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(id == "0010") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0010") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0010_bringmann_metadata.json"))
