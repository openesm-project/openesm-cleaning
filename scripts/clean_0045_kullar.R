# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# download directly from GitHub
if(!file.exists(here("data", "raw", "0045_kullar_ts_raw.csv"))){
  download.file("https://raw.githubusercontent.com/mkullar/DataDrivenEmotionDynamics/refs/heads/main/esmdata.csv",
                destfile = here("data", "raw", "0045_kullar_ts_raw.csv"))
}
df_raw <- read.csv(here("data", "raw", "0045_kullar_ts_raw.csv"))

# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
# rename columns
df <- df_raw |>
  janitor::clean_names() |>
  rename(
    id = moniker,
    happy = happy_e,
    mind_wandering = m_woccur
  )


#* Misc -------------------------------------------------------------------
# split time into day and beep number at the "."
df <- df |>
  separate_wider_delim(time,
                       delim = ".",
                       names = c("day", "beep"))


# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0045_kullar_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(id == "0045") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0045") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0045_kullar_metadata.json"))
