# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# download from GitHub
if(!file.exists(here::here("data", "raw", "0073_sicorello_ts_raw.csv"))){
  download.file(
    url = "https://raw.githubusercontent.com/MaurizioSicorello/SDERSvalid_Analysis/refs/heads/main/data/SDERSvalid_DailyLife_data_preprocessed.csv",
    destfile = here::here("data", "raw", "0073_sicorello_ts_raw.csv")
  )
}

df_raw <- read.csv(here::here("data", "raw", "0073_sicorello_ts_raw.csv"))

# also download cross-sectional data
if(!file.exists(here::here("data", "raw", "0073_sicorello_static_raw.csv"))){
  download.file(
    url = "https://raw.githubusercontent.com/MaurizioSicorello/SDERSvalid_Analysis/refs/heads/main/data/SDERSvalid_crossSec_data_preprocessed.csv",
    destfile = here::here("data", "raw", "0073_sicorello_static_raw.csv")
  )
}


# Cleaning ----------------------------------------------------------------

#* Column Names -----------------------------------------------------------
df <- df_raw |>
  janitor::clean_names()


#* Misc -------------------------------------------------------------------
# remove all aggregate columns
df <- df |>
  dplyr::select(!c(
    ends_with("_asum"),
    ends_with("_sum"),
    ends_with("_amean"),
    ends_with("_mean")
  ))

# rename columns
df <- df |>
  rename( # general columns
         id = participant_id,
         day = day_ind,
         beep = occ_ind,
         participant_timezone = participant_tz,
         export_timezone = export_tz,
         created = created_ts,
         scheduled = scheduled_ts,
         started = started_ts,
         completed = completed_ts,
         expired = expired_ts,
         uploaded = uploaded_ts,

         # ders columns
         guilty_emotion = s_ders1_esm,
         emotion_awareness = s_ders2_esm_r,
         out_of_control = s_ders3_esm,
         embarrassed_emotion = s_ders4_esm,
         negative_self_thoughts = s_ders5_esm,
         acknowledge_emotions = s_ders6_esm_r,
         emotion_confusion = s_ders7_esm,
         ashamed_emotion = s_ders8_esm,
         task_difficulty = s_ders9_esm,
         prolonged_emotion_expectation = s_ders10_esm,
         care_about_emotions = s_ders11_esm_r,
         annoyed_at_self = s_ders12_esm,
         behavior_control_difficulty = s_ders13_esm,
         confused_emotions = s_ders14_esm,
         down_expectation = s_ders15_esm,
         emotion_exploration = s_ders16_esm_r,
         emotions_out_of_control = s_ders17_esm,
         annoyed_at_self_emotion = s_ders18_esm,
         emotions_valid = s_ders19_esm_r,
         weak_emotion = s_ders20_esm,
         overwhelming_emotions = s_ders21_esm,

         # other items
         affect_grid_x = affect_esm_x,
         affect_grid_y = affect_esm_y,
         seek_contact_affect = spa,
         others_perspective_giving = pu,
         received_comfort = b_t,
         received_coping_examples = am,
         negative_event = ne_event,
         positive_event = po_event,
         impulse_contact_emotion_regulation = impuls_ier,
         impulse_self_regulation = impuls_er
  )

# remove lagged variables and non-reverse-coded
df <- df |>
  dplyr::select(-c("completed_ts_lag", "time_diff_to_previous")) |>
  dplyr::select(-c("s_ders2_esm", "s_ders6_esm", "s_ders11_esm", "s_ders16_esm", "s_ders19_esm"))

# remove demographic variables
df <- df |>
  dplyr::select(-c("age", "sex", "occupation", "acad_degree",
                   "fam_status", "ment_age", "ment_medicine",
                   "ment_treatment", "affect_pre_x", "affect_pre_y",
                   "affect_post_x", "affect_post_y", "ders_total_mean_unweighted",
                   "ierq_total_mean_unweighted"))


# better ordering of variables
df <- df |>
  dplyr::select(id, day, beep, date, everything())




# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0073_sicorello_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(dataset_id == "0073") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0073") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0073_sicorello_metadata.json"))
