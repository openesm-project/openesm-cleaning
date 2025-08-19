# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
library(osfr)
source(here::here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# download data
if(!file.exists(here("data", "raw", "0036_bosley_ts_raw.zip"))){
  osf_retrieve_file("https://osf.io/8t46j") |>
    osf_download(path = here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/8t46j") |> pull(name)
  file.rename(here("data", "raw", file_name), here("data", "raw", "0036_bosley_ts_raw.zip"))
}

# unzip files
unzip(here("data", "raw", "0036_bosley_ts_raw.zip"), exdir = here("data", "raw", "0036_bosley_ts_raw"))

# load data
file_names <- list.files(
  here("data", "raw", "0036_bosley_ts_raw", "EJPA OSF", "Idiographic Models"),
  pattern = "Week 1\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

# read files into a list
file_list <- lapply(file_names, function(x) {
  read_csv(x)
})

# rename wrongly worded columns
file_list[[95]] <- file_list[[95]] |>
  rename(
    "Felt that worry is interfering with your life" = "Felt that worry is interfering in your life",
    "When you feel happy; how often do you remind yourself that these feelings won&#39;t last" =
      "When you felt happy; how often did you remind yourself these feelings won&#39;t last"
  ) |>
  # replace "did" with "do" and "felt" with "feel" in column names
  rename_with(~gsub("did", "do", .x)) |>
  rename_with(~gsub("felt", "feel", .x))


# combine into df
df_raw <- bind_rows(file_list, .id = "id") |>
  mutate(id = gsub("_Week_1.csv$", "", id)) |>
  mutate(id = gsub("EJPA OSF/Idiographic Models/", "", id))


# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
# rename columns
df <- df_raw |>
  janitor::clean_names()

df <- df |>
  rename(
      creation_time = survey_creation_date,
      completion_time = survey_completion_date,
      bragging_thought = quot_when_you_feel_happy_how_often_do_you_think_quot_quot_people_will_think_i_number_39_m_bragging_quot_quot_quot,
      too_good_to_be_true = quot_when_you_feel_happy_how_often_do_you_think_quot_quot_this_is_too_good_to_be_true_quot_quot_quot,
      ruminate_negatives = when_you_feel_happy_how_often_do_you_think_about_things_that_have_not_gone_well_for_you,
      feelings_transient = when_you_feel_happy_how_often_do_you_remind_yourself_that_these_feelings_won_number_39_t_last,
      hard_to_concentrate = when_you_feel_happy_how_often_do_you_think_about_how_hard_it_is_to_concentrate,
      think_of_risks = when_you_feel_happy_how_often_do_you_think_about_things_that_could_go_wrong,
      undeserving = quot_when_you_feel_happy_how_often_do_you_think_quot_quot_i_don_number_39_t_deserve_this_quot_quot_quot,
      luck_will_end = quot_when_you_feel_happy_how_often_do_you_think_quot_quot_my_streak_of_luck_is_going_to_end_soon_quot_quot_quot,
      happy = felt_happy,
      content = felt_content,
      excited = felt_excited,
      sad = felt_sad,
      depressed = felt_depressed,
      nervous = felt_nervous,
      determined = felt_determined,
      attentive = felt_attentive,
      alert = felt_alert,
      inspired = felt_inspired,
      active = felt_active,
      afraid = felt_afraid,
      upset = felt_upset,
      ashamed = felt_ashamed,
      hostile = felt_hostile,
      worry_frequent = felt_worried_frequently,
      worry_hard_to_control = felt_that_worry_is_difficult_to_stop_or_control,
      worry_interferes = felt_that_worry_is_interfering_with_your_life
    )

#* Misc -------------------------------------------------------------------
# convert date columns to posixct
df <- df |>
  mutate(across(c(creation_time, completion_time), ~as.POSIXct(., format = "%m/%d/%Y %H:%M")))

# order dataframe
df <- df |>
  arrange(id, creation_time)

# day and beep columns
# day and beep column
df <- df |>
  mutate(date = as.Date(creation_time)) |>
  # add day number
  group_by(id) |>
  mutate(
    day = as.integer(date - min(date)) + 1
  ) |>
  ungroup() |>
  select(!date)

df$beep <- NA


# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0036_bosley_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(dataset_id == "0036") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0036") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0036_bosley_metadata.json"))
