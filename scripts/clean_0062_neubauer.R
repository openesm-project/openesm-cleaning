# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(osfr)
library(jsonlite)
source(here("scripts", "functions_data.R"))


# Data --------------------------------------------------------------------
# download data
# this is the ESM data
if(!file.exists(here::here("data", "raw", "0062_neubauer_ts_raw.csv"))){
  osf_retrieve_file("https://osf.io/gnxw7") |>
    osf_download(path = here::here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/gnxw7") |> pull(name)
  file.rename(here::here("data", "raw", file_name), here::here("data", "raw", "0062_neubauer_ts_raw.csv"))
}

# read data
df_raw <- read.csv(here("data", "raw", "0062_neubauer_ts_raw.csv"))

# we then also download the end-of-day survey data
if(!file.exists(here::here("data", "raw", "0062_neubauer_daily_raw.csv"))){
  osf_retrieve_file("https://osf.io/5a4yf") |>
    osf_download(path = here::here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/5a4yf") |> pull(name)
  file.rename(here::here("data", "raw", file_name), here::here("data", "raw", "0062_neubauer_daily_raw.csv"))
}

df_daily <- read.csv(here("data", "raw", "0062_neubauer_daily_raw.csv"))

# merge datasets
df_raw_merge <- df_raw |>
  mutate(source = "esm")
df_daily_merge <- df_daily |>
  mutate(source = "daily",
         beep = 6)

df <- bind_rows(df_raw_merge, df_daily_merge)

# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
df <- df |>
  janitor::clean_names() |>
  rename(
    start_time = started,
    end_time = lastdata,
    question_nr = questnnr,
    day = day0,
    timestamp = iv01_01,
    last_page = lastpage,
    max_page = maxpage,
    too_late = toolate,
    noncompliance_daily = noncompliance_eod,

    # renaming esm variables
    # location/context
    location = um01,
    number_of_company = um02,
    no_company = um02_01,
    with_partner = um02_02,
    with_parents = um02_03,
    with_relatives = um02_04,
    with_friends = um02_05,
    with_fellow_students = um02_06,
    with_strangers = um02_07,
    # affect
    happy = af05_01,
    afraid = af05_02,
    sad = af05_03,
    balanced = af05_04,
    exhausted = af05_05,
    cheerful = af05_06,
    worried = af05_07,
    lively = af05_08,
    angry = af05_09,
    relaxed = af05_10,
    # study-related
    studied = ds01,
    number_study_activities = ds02,
    attended_lecture_seminar = ds02_01,
    held_presentation = ds02_02,
    group_work = ds02_03,
    assignments = ds02_04,
    study_reading = ds02_05,
    preparation_follow_up = ds02_06,
    other_study_activities = ds02_07,
    time_spent_studying = ds03,
    # other activities
    number_other_activities = ds04,
    part_time_job = ds04_01,
    care_taking = ds04_02,
    time_with_friends = ds04_03,
    on_social_media = ds04_04,
    tv_video_games = ds04_05,
    listened_music = ds04_06,
    sports = ds04_07,
    walking = ds04_08,
    prayer_meditation = ds04_09,
    reading = ds04_10,
    other_activities = ds04_11,
    supposed_to_study = ds05,
    # study motivation
    study_motivation_others_disappointed = sm04_01,
    study_motivation_compulsory = sm04_02,
    study_motivation_felt_bad = sm04_03,
    study_motivation_proving = sm04_04,
    study_motivation_important = sm04_05,
    study_motivation_understanding = sm04_06,
    study_motivation_interesting = sm04_07,
    study_motivation_enjoyment = sm04_08,
    # activity motivation
    activity_motivation_others_disappointed = ds15_01,
    activity_motivation_compulsory = ds15_02,
    activity_motivation_felt_bad = ds15_03,
    activity_motivation_proving = ds15_04,
    activity_motivation_important = ds15_05,
    activity_motivation_understanding = ds15_06,
    activity_motivation_interesting = ds15_07,
    activity_motivation_enjoyment = ds15_08,
    # emotion regulation
    see_good_in_bad = er02_01,
    focus_on_good = er02_03,
    suppression = er02_04,
    changed_feeling = er02_06,
    rumination = er02_08,

    # daily
    # need fulfillment
    contact_with_people = pn05_01,
    excluded = pn05_02,
    failure = pn05_03,
    own_way = pn05_04,
    completed_difficult_project = pn05_05,
    true_self = pn05_06,
    connected = pn05_07,
    mastered_challenges = pn05_08,
    intimacy = pn05_09,
    pressure = pn06_01,
    tell_what_to_do = pn06_02,
    unappreciated = pn06_03,
    disagreements = pn06_04,
    even_hard_things = pn06_05,
    against_own_will = pn06_06,
    did_stupid = pn06_07,
    struggled = pn06_08,
    did_interesting = pn06_09,
    # daily affect
    happy_daily = af02_01,
    afraid_daily = af02_02,
    sad_daily = af02_03,
    balanced_daily = af02_04,
    exhausted_daily = af02_05,
    cheerful_daily = af02_06,
    worried_daily = af02_07,
    lively_daily = af02_08,
    angry_daily = af02_09,
    relaxed_daily = af02_10,
    # studying
    number_study_formats = st53,
    study_in_person = st53_01,
    study_virtual = st53_02,
    study_hybrid = st53_03,
    no_class = st53_04,
    hours_studying = ds06_01,
    minutes_studying = ds06_02,
    # study satisfaction
    study_enjoy = st54_01,
    study_wearing_down = st54_04,
    study_satisfied = st54_02,
    study_difficult_reconcile = st54_05,
    study_interesting = st54_03,
    study_exhausted = st54_06,
    study_only_necessary = st54_07,
    study_energy = st54_08,
    study_identification = st54_09,
    study_expectations = st54_10,
    study_consider_quitting = st54_11,
    # parents
    time_mother_hours = pa21_01,
    time_mother_minutes = pa21_02,
    time_father_hours = pa22_01,
    time_father_minutes = pa22_02,
    # perceived parenting
    mother_autonomy = pa23_01,
    mother_own_decisions = pa23_03,
    mother_asked_opinion = pa23_02,
    mother_interrupted = pa23_05,
    mother_point_of_view = pa23_04,
    mother_guilty = pa23_07,
    mother_tried_change = pa23_06,
    mother_disapproved = pa23_08,
    father_autonomy = pa24_01,
    father_own_decisions = pa24_03,
    father_asked_opinion = pa24_02,
    father_interrupted = pa24_05,
    father_point_of_view = pa24_04,
    father_guilty = pa24_07,
    father_tried_change = pa24_06,
    father_disapproved = pa24_08
  )




#* Misc -------------------------------------------------------------------
# remove columns that are not needed
df <- df |>
  select(!c(sample))

# # recode -9 to NA
df <- df |>
  mutate(across(where(is.numeric), ~ na_if(.x, -9))) |>
  mutate(across(where(is.character), ~ na_if(.x, "-9")))

# convert time columns to PosixCt
df <- df |>
mutate(
  start_time = as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%S"),
  end_time = as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%S"),
  date = as.Date(date)
)


# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0062_neubauer_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(dataset_id == "0062") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0062") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0062_neubauer_metadata.json"))
