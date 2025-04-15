# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
library(osfr)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# download data
# https://osf.io/6b7fw
if(!file.exists(here("data", "raw", "0037_fisher_ts_raw.zip"))){
  osf_retrieve_file("https://osf.io/6b7fw") |>
    osf_download(path = here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/6b7fw") |> pull(name)
  file.rename(here("data", "raw", file_name), here("data", "raw", "0037_fisher_ts_raw.zip"))
}
# unzip data
if(!file.exists(here("data", "raw", "0037_fisher_ts_raw"))){
  unzip(here("data", "raw", "0037_fisher_ts_raw.zip"), exdir = here("data", "raw", "0037_fisher_ts_raw"))
}

# read in RData files
file_names <- list.files(
  here("data", "raw", "0037_fisher_ts_raw"),
  pattern = ".RData",
  full.names = TRUE,
  recursive = TRUE
)

# read in data into list
file_list <- list()
for(file in file_names){
  load(file)
  file_list[[file]] <- datx
}

# remove everything but "file_list"
rm(list = ls()[!ls() %in% c("file_list")])

# for first entry, only keep columns that are present for second entry
# the first person seems to have somewhat different items
file_list[[1]] <- file_list[[1]] |>
  select(any_of(colnames(file_list[[2]])))

# in all dataframes, remove the duplicated column name
file_list <- lapply(file_list, function(x) {
  x <- x[, !duplicated(colnames(x))]
  return(x)
})


# combine into dataframe with id
df_raw <- bind_rows(file_list, .id = "id") |>
  mutate(id = gsub(".RData", "", id)) |>
  # only keep the last the three characters of id
  mutate(id = str_sub(id, -3, -1))

# remove rownames
df_raw <- df_raw |>
  rownames_to_column("row") |>
  select(-row)

# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
# rename columns
df <- df_raw |>
  janitor::clean_names() |>
  rename(delay_gratification = delaygr)

df <- df |>
  rename(
    start_time = start,
    finish_time = finish,
    sleep_hours = hours,
    sleep_difficulty = difficulty
  )

#* Misc -------------------------------------------------------------------
# remove aggregate/time conversion or unclear columns
irrelevant_cols <- c(
  "lag",
  "tdif",
  "cumsum_t",
  "morning",
  "midday",
  "eve",
  "night",
  "mon",
  "tues",
  "wed",
  "thur",
  "fri",
  "sat",
  "sun",
  "linear",
  "quad",
  "cub",
  "restless_1",
  "cigbin",
  "cig_h",
  "filter"
)

df <- df |>
  select(-c(
    any_of(irrelevant_cols),
    contains("sin"),
    contains("cos"),
    contains("fw")

  ))

# convert date columns to posixct
df <- df |>
  mutate(across(c(start_time, finish_time), ~as.POSIXct(., format = "%m/%d/%Y %H:%M")))

# obtain beep and day column
df <- df |>
  mutate(
    beep = ping + 1
  ) |>
  select(-ping)

# arrange by id and date
df <- df |>
  arrange(id, start_time)

# give each day for each individual an id, indicating left out days
# do this based on "start_time"
df <- df |>
  group_by(id) |>
  mutate(
    day = as.numeric(as.factor(as.Date(start_time))),
  ) |>
  ungroup()



# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0037_fisher_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(id == "0037") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0037") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0037_fisher_metadata.json"))
