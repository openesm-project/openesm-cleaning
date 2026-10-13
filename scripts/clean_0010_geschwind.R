# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
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

# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
metadata <- read_metadata("0010")

# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, metadata)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0010_geschwind_ts.tsv"))
}
