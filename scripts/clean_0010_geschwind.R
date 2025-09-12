# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
library(osfr)
# install.packages("RCurl")
library(RCurl)
source(here::here("scripts", "functions_data.R"))





# Data --------------------------------------------------------------------
# downloaded from "https://doi.org/10.1371/journal.pone.0060188.s004"
df <- read.csv(here::here("data", "raw", "0010_geschwind_ts_raw.csv"))



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
  dplyr::group_by(id) |>
  dplyr::distinct(id, therapy, neuroticism)

# save demographic data
write_tsv(df_demographics,
          here::here("data", "clean", "0010_geschwind_static.tsv"))

# remove demographic columns from main data
df <- df |>
  dplyr::select(-c(therapy, neuroticism))

# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)
check_results

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here::here("data", "clean", "0010_geschwind_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  dplyr::filter(dataset_id == "0010") |>
  dplyr::pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0010") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here::here("data", "metadata", "0010_geschwind_metadata.json"))
