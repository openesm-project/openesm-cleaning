# Packages ----------------------------------------------------------------
library(tidyverse)
library(osfr)
library(here)
library(googlesheets4)
library(jsonlite)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
if(!file.exists(here("data", "raw", "0001_fried_ts_raw.csv"))){
  osf_retrieve_file("https://osf.io/t7g4f") |>
    osf_download(path = here("data", "raw"))


  # rename data to 0001_fried.csv
  file_name <- osf_retrieve_file("https://osf.io/t7g4f") |> pull(name)
  file.rename(here("data", "raw", file_name), here("data", "raw", "0001_fried_ts_raw.csv"))
}

df <- read.csv(here("data", "raw", "0001_fried_ts_raw.csv"))

# Cleaning ----------------------------------------------------------------
#* Column names ------------------------------------------------------------
# Perform some of the cleaning steps outlined here: https://osf.io/5p3zg
new_names <- c(
  Q1 = "Relax",
  Q2 = "Irritable",
  Q3 = "Worry",
  Q4 = "Nervous",
  Q5 = "Future",
  Q6 = "Anhedonia",
  Q7 = "Tired",
  Q8 = "Hungry",
  Q9 = "Alone",
  Q10 = "Angry",
  Q11 = "Social-offline",
  Q12 = "Social-online",
  Q13 = "Music",
  Q14 = "Procrastinate",
  Q15 = "Outdoors",
  Q16 = "C19-occupied",
  Q17 = "C19-worry",
  Q18 = "Home"
)

# rename
colnames(df) <- ifelse(colnames(df) %in% names(new_names), new_names[colnames(df)], colnames(df))

# clean names
df <- df |>
  janitor::clean_names()



#* Misc --------------------------------------------------------------------
# change ID to subsequent numbers, makes looping easier
un <- unique(df$id)
id_new <- rep(NA, length(df$id))
for(i in 1:length(un)) id_new[df$id==un[i]] <- i
df$id <- id_new


# save time of response as date
df$date <- as.POSIXct(df$response, format = "%Y-%m-%d %H:%M:%S")

# create numeric day variable
df$day <- as.Date(df$day)
df$day <- as.numeric(df$day)
un2 <- unique(df$day)
day_new <- rep(NA, length(df$day))
for(i in 1:length(un2)) day_new[df$day==un2[i]] <- i
df$day <- day_new

df <- df |>
  rename(beep = beepvar)


# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0001_fried_ts.tsv"))
}



# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)

sheet_url <- meta_data |>
  filter(id == "0001") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)


create_metadata_json <- function(dataset_id) {

  # Extract dataset-level info
  dataset_info <- meta_data |>
    dplyr::filter(id == dataset_id)

  # Extract feature-level info
  dataset_features <- variable_data |>
    dplyr::filter(id == dataset_id) |>
    dplyr::mutate_all(~ ifelse(is.na(.), "", .)) |>
    dplyr::select(!id)

  # Construct JSON structure
  list(
    first_author = dataset_info$Author,
    dataset = dataset_info$id,
    year = dataset_info$Year,
    paper_doi = dataset_info$`Paper DOI`,
    link_to_data = dataset_info$`Link to data`,
    link_to_code = dataset_info$`Optional: Link to Code`,
    n_participants = dataset_info$`N Participants`,
    n_time_points = dataset_info$`N Time Points`,
    n_beeps_per_day = dataset_info$`N Beeps/Day`,
    passive_data_available = dataset_info$`Passive data available?`,
    topics = dataset_info$Topics,
    implicit_missingness = dataset_info$`Implicit Missingness?`,
    raw_time_stamp = dataset_info$`Raw Time Stamp available?`,
    sampling_scheme = dataset_info$`Sampling Scheme`,
    participants = dataset_info$Participants,
    coding_file = dataset_info$`Coding File`,
    additional_comments = dataset_info$`additional comments`,
    coder_data = dataset_info$`Coder Data`,
    coder_metadata = dataset_info$`Coder Metadata`,
    features = dataset_features
  )
}

test <- create_metadata_json("0001") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)
test
