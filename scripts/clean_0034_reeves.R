# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
library(osfr)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# downloaded zip from https://osf.io/6vxhf/
# unzip data
unzip(here("data", "raw", "0034_reeves_ts_raw.zip"),
      exdir = here("data", "raw", "0034_reeves_ts_raw"))

# load all csv files in the folder and combine
# into one data frame
df_raw <- list.files(here("data", "raw", "0034_reeves_ts_raw"),
                      pattern = "*.csv",
                      full.names = TRUE) |>
   # read as dataframe, give file name without "-Phone Surveys" as id
   map_dfr(~read_csv(.x, col_names = TRUE) |>
             mutate(id = str_remove(basename(.x), "-Phone Surveys"))) |>
  mutate(id = gsub(".csv", "", id)) |>
  mutate(id = gsub("-Phone Surveys", "", id))


# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
# rename columns
df <- df_raw |>
  janitor::clean_names()

df <- df |>
  rename(
    creation_date = survey_creation_date,
    completion_date = survey_completion_date,
    hypervigilant = quot_felt_extremely_alert_watchful_or_quot_quot_on_guard_quot_quot_quot,
    aggressive = felt_irritable_had_angry_outbursts_or_acted_aggressively,
    afraid = felt_afraid,
    neg_thoughts_self = had_negative_thoughts_about_yourself,
    avoidance = avoided_thoughts_and_or_feelings_related_to_the_event,
    reliving = felt_like_you_were_reliving_a_traumatic_event,
    unpleasant_dreams = had_unpleasant_dreams_about_a_traumatic_event,
    intrusions = had_stressful_unwanted_memories_of_a_traumatic_event,
    sleep_duration = how_many_hours_did_you_sleep_last_night,
    restless_sleep = experienced_restless_or_unsatisfying_sleep,
    sleep_troubles = had_trouble_falling_or_staying_asleep,
    horrified = felt_horrified,
    angry = felt_angry,
    guilty = felt_guilty,
    ashamed = felt_ashamed,
    neg_thoughts_others = had_negative_thoughts_about_others,
    neg_thoughts_world = had_negative_thoughts_about_the_world,
    self_blame = blamed_yourself_for_a_traumatic_event_or_its_consequences,
    others_blame = blamed_others_for_a_traumatic_event_or_its_consequences,
    avoid_places = avoided_people_places_or_situations_related_to_a_traumatic_event,
    upset = felt_upset_when_reminded_of_a_traumatic_event,
    physical_reactions = had_physical_reactions_when_reminded_of_a_traumatic_event,
    amnesia = had_difficulty_remembering_the_details_of_a_traumatic_event,
    anhedonia = felt_less_interested_or_participated_less_in_your_usual_activities,
    distant = felt_distant_or_cut_off_from_others,
    difficulty_positive = had_difficulty_feeling_positive,
    reckless = acted_reckless_or_self_destructive,
    startled = felt_jumpy_or_easily_startled,
    difficulty_concent = had_difficulty_concentrating,
    sleepy = felt_sleepy,
    happy = felt_happy,
    positive = felt_positive,
    content = felt_content,
    calm = felt_calm,
    fatigued = felt_fatigued,
    muscle_tension = experienced_muscle_tension
  )


#* Misc -------------------------------------------------------------------

# convert date columns to posixct
df <- df |>
  mutate(across(c(creation_date, completion_date), ~as.POSIXct(., format = "%m/%d/%Y %H:%M")))

# for each person, create a numerical day indicator from the first to the last day of the study
df <- df |>
  mutate(date = as.Date(creation_date)) |>
  # add day number
  group_by(id) |>
  mutate(
    day = as.integer(date - min(date)) + 1
  ) |>
  ungroup() |>
  select(!date)

# create empty beep column
df$beep <- NA



# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0034_reeves_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(dataset_id == "0034") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0034") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0034_reeves_metadata.json"))
