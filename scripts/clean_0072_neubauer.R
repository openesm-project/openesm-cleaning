# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
library(osfr)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# daily data
if(!file.exists(here("data", "raw", "0072_neubauer_ts_raw.csv"))){
  osf_retrieve_file("https://osf.io/unkfc") |>
    osf_download(path = here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/unkfc") |> pull(name)
  file.rename(here("data", "raw", file_name), here("data", "raw", "0072_neubauer_ts_raw.csv"))
}
df_raw <- read_csv(here("data", "raw", "0072_neubauer_ts_raw.csv"))

# download static data
if(!file.exists(here("data", "raw", "0072_neubauer_static_raw.csv"))){
  osf_retrieve_file("https://osf.io/39jgh") |>
    osf_download(path = here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/39jgh") |> pull(name)
  file.rename(here("data", "raw", file_name), here("data", "raw", "0072_neubauer_static_raw.csv"))
}

# download followup data
if(!file.exists(here("data", "raw", "0072_neubauer_followup_raw.csv"))){
  osf_retrieve_file("https://osf.io/m7qyr") |>
    osf_download(path = here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/m7qyr") |> pull(name)
  file.rename(here("data", "raw", file_name), here("data", "raw", "0072_neubauer_followup_raw.csv"))
}

# download post data
if(!file.exists(here("data", "raw", "0072_neubauer_post_raw.csv"))){
  osf_retrieve_file("https://osf.io/wsfmz") |>
    osf_download(path = here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/wsfmz") |> pull(name)
  file.rename(here("data", "raw", file_name), here("data", "raw", "0072_neubauer_post_raw.csv"))
}

# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
df <- df_raw |>
  janitor::clean_names() |>
  rename(
    # basic stuff
    id = serial,
    questionnaire_type = questnnr,
    start_time = started_d,
    last_update = lastdata_d,
    sleep_quality = sl02_01,
    # school work
    school_new_material = ds05,
    school_work_today = ds01,
    difficulty_school_work = ds02_01,
    volume_school_work = ds02_02,
    school_work_hours = ds03_01,
    school_work_minutes = ds03_02,
    school_work_comprehensible = ds04_01,
    school_work_independent = ds04_02,
    school_work_needed_support = ds04_03,
    school_work_joy = ds04_04,
    school_work_effort = ds04_05,
    # parenting
    time_with_child_hours = pd01_01,
    time_with_child_minutes = pd01_02,
    parenting_fun = pd02_01,
    parenting_talked_worries = pd02_02,
    parenting_assert_difficult = pd02_03,
    parenting_drained_energy = pd02_04,
    parenting_disagreements = pd02_05,
    parenting_child_decide = pd03_01,
    parenting_child_liked = pd03_02,
    parenting_told_what_to_do = pd03_03,
    parenting_explained_why = pd03_04,
    parenting_explained_why_not = pd03_05,
    # child affect
    child_happy = ac01_01,
    child_afraid = ac01_02,
    child_sad = ac01_03,
    child_balanced = ac01_04,
    child_exhausted = ac01_05,
    child_cheerful = ac01_06,
    child_worried = ac01_07,
    child_lively = ac01_08,
    child_angry = ac01_09,
    child_relaxed = ac01_10,
    # daily activities
    activities_total = ah01,
    activity_work = ah01_01,
    activity_work_hours = ah07_01,
    activity_household = ah01_02,
    activity_household_hours = ah07_02,
    activity_learning_with_child = ah01_03,
    activity_learning_hours = ah07_03,
    activity_playing_with_child = ah01_04,
    activity_playing_hours = ah07_04,
    activity_communicating_friends = ah01_05,
    activity_communicating_hours = ah07_05,
    activity_walking = ah01_06,
    activity_walking_hours = ah07_06,
    activity_sports = ah01_07,
    activity_sports_hours = ah07_07,
    activity_shopping = ah01_08,
    activity_shopping_hours = ah07_08,
    activity_gardening = ah01_09,
    activity_gardening_hours = ah07_09,
    activity_praying_meditating = ah01_10,
    activity_praying_hours = ah07_10,
    activity_media = ah01_11,
    activity_media_hours = ah07_11,
    activity_reading_me_time = ah01_12,
    activity_reading_hours = ah07_12,
    activity_other = ah01_13,
    activity_other_hours = ah07_13,
    time_outside_hours = ah06_01,
    time_outside_minutes = ah06_02,
    # stressors
    stressor_argument = de01_01,
    stressor_friend_relative = de01_03,
    stressor_health = de01_04,
    stressor_work = de01_05,
    stressor_household = de01_06,
    stressor_leisure = de01_07,
    stressor_financial = de01_08,
    stressor_other = de01_09,
    # psychological need fulfillment
    contact_with_caring_people = dn01_01,
    excluded_ostracized = dn01_02,
    failure = dn01_03,
    things_own_way = dn01_04,
    completed_difficult_project = dn01_05,
    true_self = dn01_06,
    close_connected = dn01_07,
    mastered_hard_challenges = dn01_08,
    strong_sense_intimacy = dn01_09,
    pressure = dn02_01,
    people_telling_me = dn02_02,
    unappreciated = dn02_03,
    disagreements_conflicts = dn02_04,
    even_hard_things = dn02_05,
    things_against_will = dn02_06,
    did_something_stupid = dn02_07,
    struggled_doing_something = dn02_08,
    doing_what_interests_me = dn02_09,
    # need fulfillment composites
    # autonomy_satisfaction_daily = aut_sat_d,
    # competence_satisfaction_daily = com_sat_d,
    # relatedness_satisfaction_daily = rel_sat_d,
    # autonomy_frustration_daily = aut_dis_d,
    # competence_frustration_daily = com_dis_d,
    # relatedness_frustration_daily = rel_dis_d,
    # parent daily affect
    parent_happy = af03_01,
    parent_afraid = af03_02,
    parent_sad = af03_03,
    parent_balanced = af03_04,
    parent_exhausted = af03_05,
    parent_cheerful = af03_06,
    parent_worried = af03_07,
    parent_lively = af03_08,
    parent_angry = af03_09,
    parent_relaxed = af03_10,
    # worries
    worry_many_things = dw01_01,
    worry_bothered = dw01_02,
    worry_might_happen = dw01_05,
    worry_cant_get_out_head = dw01_06,
    worry_wrapped_up = dw01_07,
    # mindfulness
    mindfulness_open_to_happening = dm01_01,
    mindfulness_inappropriate = dm01_02,
    mindfulness_present_moment = dm01_03,
    mindfulness_could_act_better = dm01_04,
    # Corona specific
    corona_info_update = dc01_01,
    corona_talk_friends_family = dc01_02,
    corona_social_media_check = dc01_03,
    corona_social_media_create = dc01_04,
    corona_thinking_about = dc02_01,
    corona_worries = dc02_02,
    corona_acceptance = dc02_03
  )


#* Misc -------------------------------------------------------------------
# remove composite columns
df <- df |>
  select(-aut_sat_d, -com_sat_d, -rel_sat_d, -aut_dis_d, -com_dis_d, -rel_dis_d)

# remove redundant column
df <- df |>
  select(-questionnaire_type)

# convert time columns to POSIXct
df <- df |>
  mutate(
    start_time = as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    last_update = as.POSIXct(last_update, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  )

# check for character NA
df <- df |>
  mutate(across(where(is.character), ~ na_if(., "NA")))

# recode stressor columns from 2, 1 to 1, 0
df <- df |>
  mutate(across(starts_with("stressor_"), ~ ifelse(!is.na(.), . - 1, .)))

# add beep
df$beep <- 1

# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0072_neubauer_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(dataset_id == "0072") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0072") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0072_neubauer_metadata.json"))
