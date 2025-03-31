# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
if(!file.exists(here("data", "raw", "0013_hoebeke_ts_raw.csv"))){
  osf_retrieve_file("https://osf.io/3z5jh") |>
    osf_download(path = here("data", "raw"))


  # rename data
  file_name <- osf_retrieve_file("https://osf.io/3z5jh") |> pull(name)
  file.rename(here("data", "raw", file_name), here("data", "raw", "0013_hoebeke_ts_raw.csv"))
}

df_raw <- read_csv(here("data", "raw", "0013_hoebeke_ts_raw.csv"))


# Cleaning ----------------------------------------------------------------

#* Column Names -----------------------------------------------------------
df <- df_raw |>
  janitor::clean_names()



#* Misc -------------------------------------------------------------------
# Column types
df <- df |>
  mutate(across(c(created, modified, ended, expired), ~as.POSIXct(., format = "%Y-%m-%d %H:%M:%S"))) |>
  mutate_if(is.character, list(~na_if(., "")))








#** Demographics data combination -----------------------------------------
# They are in separate files
hoebeke_demographics <- read_csv(here::here("data", "raw", "0013_hoebeke_demographics_raw.csv"))
hoebeke_endsurvey <- read_csv(here::here("data", "raw", "0013_hoebeke_endsurvey_raw.csv"))
hoebeke_endusers <- read_csv(here::here("data", "raw", "0013_hoebeke_endusers_raw.csv"))

# combine demographics data by session
hoebeke_all_demo <- hoebeke_demographics |>
  # deal with duplicated column names explicitly in the left_join
  left_join(hoebeke_endsurvey, by = "session", suffix = c("_demo", "qualtrics")) |>
  left_join(hoebeke_endusers, by = "session", suffix = c("_demo", "_endusers"))

# save
write_tsv(hoebeke_all_demo, here("data", "clean", "0013_hoebeke_static.tsv"))

#** Erroneous surveys -----------------------------------------------------
# as explained in the original code, there are some erroneous surveys
# https://osf.io/myjdu
# These need to be removed
param_timepoints = c("09:00", "13:00", "17:00","21:00")
param_t_zero = "00:05"
param_t_end = "23:30"
param_days_duration_study = 14
param_surveys_by_day = 4

#--- the following code is mostly copied from the original code

df <- df |>
  mutate(
    time_interval = case_when(
      format(created,"%H:%M") >= param_timepoints[1] & format(created,"%H:%M") < param_timepoints[2]  ~ param_timepoints[1],
      format(created,"%H:%M") >= param_timepoints[2] & format(created,"%H:%M") < param_timepoints[3]  ~ param_timepoints[2],
      format(created,"%H:%M") >= param_timepoints[3] & format(created,"%H:%M") < param_timepoints[4]  ~ param_timepoints[3],
      format(created,"%H:%M") >= param_timepoints[4] & format(created,"%H:%M") < param_t_end          ~ param_timepoints[4],
    )
  )

# to determine obs_n you need to get the n_day
# to get n_day you need to know when the participants started
df_start_date <- hoebeke_demographics |>
  dplyr::select(session, created) |>
  mutate(start_date_study = as_date(created)) |>
  dplyr::select(session, start_date_study)

df = df |>
  full_join(df_start_date, by = "session")

# add column for n days (this works even if a participant did not answer at all one day)
df = df |>
  mutate(n_day = as.numeric(difftime(as_date(created),
                                     start_date_study,
                                     units = "days"), units="days"),
         time_interval = as.factor(time_interval))

# now we can add the n_obs column
df = df |> mutate(
  n_obs = as.numeric(case_when(
    time_interval == param_timepoints[1]    ~ (n_day - 1)*param_surveys_by_day + 1,
    time_interval == param_timepoints[2]    ~ (n_day - 1)*param_surveys_by_day + 2,
    time_interval == param_timepoints[3]    ~ (n_day - 1)*param_surveys_by_day + 3,
    time_interval == param_timepoints[4]    ~ (n_day - 1)*param_surveys_by_day + 4
  ))
)



n_duplicates <- df |>
  arrange(session, created) |>
  group_by(session, n_obs, time_interval) |>
  mutate(num_dups = n(),
         dup_id = row_number()) |>
  ungroup() |>
  subset(num_dups > 1) |> count() |> as.numeric()


# first find the duplicates according to n_obs and time_interval
df <- df |>
  arrange(session, created) |>
  group_by(session, n_obs, time_interval) |>
  mutate(num_dups = n(),
         dup_id = row_number()) |>
  ungroup() |>
  # then remove first the duplicates if one of the survey's submission hasn't been submitted
  subset(num_dups == 1 | (num_dups > 1 & !is.na(ended)) ) |>
  # we recompute which rows are duplicate
  group_by(session, n_obs, time_interval) |>
  arrange(session, created) |>
  mutate(num_dups = n(),
         dup_id = row_number()) |>
  ungroup() |>
  # and if the participant had submitted two surveys for the same time, we only kept the first submission (as it is ordered by creation date)
  subset(dup_id == 1) |>
  dplyr::select(-num_dups, -dup_id)


#--- end original code

# Now change these variables to our naming scheme
df <- df |>
  rename(
    id = session,
    day = n_day,
    counter = n_obs
  ) |>
  mutate(beep = case_when(
    time_interval == param_timepoints[1] ~ 1,
    time_interval == param_timepoints[2] ~ 2,
    time_interval == param_timepoints[3] ~ 3,
    time_interval == param_timepoints[4] ~ 4
  )) |>
  select(-time_interval)




# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0013_hoebeke_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(id == "0013") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0013") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0013_hoebeke_metadata.json"))
