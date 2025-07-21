# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# Download data from GitHub link
# https://github.com/zwihawks/PredictingMomentaryCog/blob/main/cleanData.rds
df_list <- readRDS(here("data", "raw", "0003_hawks_ts_raw.rds"))

# check if the four dataframes include the same columns
# extract column names
col_names_list <- map(df_list$data, colnames)
unique_cols <- unique(unlist(col_names_list))

# check if columns are equal across the four dataframes
col_presence <- sapply(col_names_list, function(cols) unique_cols %in% cols)
rownames(col_presence) <- unique_cols
print(col_presence)


# rename value columns for each dataframe
df_list$data_new <- map2(df_list$key, df_list$data, function(k, d) {
  d |>
    rename(!!k := value)  # Rename value column based on key
})

# attach them to the longest dataframe
df <- df_list$data_new[[4]] |>
  full_join(df_list$data_new[[3]], by = c("user_id", "study_days", "study_time"), suffix=c("",".y")) |>
  select(-ends_with(".y")) |>
  full_join(df_list$data_new[[2]], by = c("user_id", "study_days", "study_time"), suffix=c("",".y")) |>
  select(-ends_with(".y")) |>
  full_join(df_list$data_new[[1]], by = c("user_id", "study_days", "study_time"), suffix=c("",".y")) |>
  select(-ends_with(".y"))




# Cleaning ----------------------------------------------------------------

#* Column Names -----------------------------------------------------------
df <- janitor::clean_names(df)

# rename columns
df <- df |>
  rename(id = user_id) |>
  rename(day = study_days) |>
  rename(interruptions = interruptions_interruptions) |>
  rename(sleep_duration = sleep_dur,
         social_functioning_composite = socialfunc_comp)

# remove long column prefixes
prefixes_to_remove <- c("na_emotions_",
                        "pa_emotions_",
                        "anx_dep_emotions_",
                        "stress_",
                        "context_",
                        "attention_")

df <- df |>
  rename_all(~ str_remove_all(., str_c("^(", str_c(prefixes_to_remove, collapse = "|"), ")")))


#* Misc -------------------------------------------------------------------
# convert study time to hours
df <- df |>
  mutate(study_time = study_time * 24)

# create a beep column
df <- df |>
  arrange(id, day, study_time) |>
  group_by(id, day) |>
  mutate(beep = row_number()) |>
  ungroup()


# split off demographic data
cols_demo <- c("age", "education", "gender", "english_primary",
               "wake_up_typical", "wake_up_earliest", "wake_up_latest",
               "go_to_sleep_typical", "go_to_sleep_earliest", "go_to_sleep_latest",
               "screen_w", "screen_h")

df_demographics <- df |>
  group_by(id) |>
  distinct(across(all_of(cols_demo))) |>
  ungroup()

# save demographic data
write_tsv(df_demographics, here("data", "clean", "0003_hawks_static.tsv"))

# remove demographic data from main df
df <- df |>
  select(-c(all_of(cols_demo)))


# nicer order
df <- df |>
  select(id, day, beep, sitting_id, everything())


# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0003_hawks_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)

sheet_url <- meta_data |>
  filter(id == "0003") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0003") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0003_hawks_metadata.json"))
