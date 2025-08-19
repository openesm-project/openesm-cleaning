# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
library(readr)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# Read in data
df_raw <- read_delim(here("data", "raw", "0011_bringmann_ts_raw.csv"),
                  delim = ";",
                  col_names = TRUE,
                  trim_ws = TRUE,
                  na = c("9998", "9999"))





# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
# Rename columns to snake_case
df <- df_raw |>
  janitor::clean_names() |>
  rename(
    id = id_1,
    angry = kwaad,
    depressed = depre,
    sad = droev,
    anxious = angst,
    relaxed = ontsp,
    happy = blij
  )


#* Misc -------------------------------------------------------------------
# remove lagged and superfluous columns
df <- df |>
  select(-c(contains("_1")))

# split off neuroticism to static data file
all_ids <- unique(df$id)

# split of the neuroticism column, then reattach it to the correct id
df_neuroticism <- df |>
  select(id, neuroticism_score) |>
  slice(1:95) |>
  # attach the correct id
  mutate(id = all_ids)


# remove neuroticism from df
df <- df |>
  select(-neuroticism_score)

# save static data
write_tsv(df_neuroticism, here("data", "clean", "0011_bringmann_static.tsv"))

# add empty day and beep variable
df <- df |>
  mutate(
    day = NA,
    beep = NA
  )


# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0011_bringmann_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(dataset_id == "0011") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0011") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0011_bringmann_metadata.json"))
