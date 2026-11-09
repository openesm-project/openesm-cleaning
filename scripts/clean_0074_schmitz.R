# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(jsonlite)
library(osfr)
source(here::here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# the original study consisted of 4 datasets
# distrust
if(!file.exists(here::here("data", "raw", "0074_schmitz_ts_raw_distrust.csv"))){
  osf_retrieve_file("https://osf.io/9hwtg") |>
    osf_download(path = here::here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/9hwtg") |> pull(name)
  file.rename(here::here("data", "raw", file_name), here::here("data", "raw", "0074_schmitz_ts_raw_distrust.csv"))

}

# emotion
if(!file.exists(here::here("data", "raw", "0074_schmitz_ts_raw_emotion.csv"))){
  osf_retrieve_file("https://osf.io/r26pv") |>
    osf_download(path = here::here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/r26pv") |> pull(name)
  file.rename(here::here("data", "raw", file_name), here::here("data", "raw", "0074_schmitz_ts_raw_emotion.csv"))

}

# positive emotion
if(!file.exists(here::here("data", "raw", "0074_schmitz_ts_raw_pos_emotion.csv"))){
  osf_retrieve_file("https://osf.io/hsg4f") |>
    osf_download(path = here::here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/hsg4f") |> pull(name)
  file.rename(here::here("data", "raw", file_name), here::here("data", "raw", "0074_schmitz_ts_raw_pos_emotion.csv"))

}

# negative emotion
if(!file.exists(here::here("data", "raw", "0074_schmitz_ts_raw_neg_emotion.csv"))){
  osf_retrieve_file("https://osf.io/4873h") |>
    osf_download(path = here::here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/4873h") |> pull(name)
  file.rename(here::here("data", "raw", file_name), here::here("data", "raw", "0074_schmitz_ts_raw_neg_emotion.csv"))

}

# read in these files
data_distrust <- read_csv(here::here("data", "raw", "0074_schmitz_ts_raw_distrust.csv"))
data_emotion <- read_csv(here::here("data", "raw", "0074_schmitz_ts_raw_emotion.csv"))
data_pos_emotion <- read_csv(here::here("data", "raw", "0074_schmitz_ts_raw_pos_emotion.csv"))
data_neg_emotion <- read_csv(here::here("data", "raw", "0074_schmitz_ts_raw_neg_emotion.csv"))


# collect the four datasets in a named list
datasets <- list(
  dataset1 = data_distrust,
  dataset2 = data_emotion,
  dataset3 = data_pos_emotion,
  dataset4 = data_neg_emotion
)

# identify all unique column names
all_columns <- datasets |>
  purrr::map(names) |>
  unlist() |>
  unique()

# check which datasets contain each column
column_overlap <- tibble(
  column = all_columns
) |>
  mutate(
    across_datasets = purrr::map(
      column,
      \(col) names(datasets)[
        purrr::map_lgl(datasets, \(df) col %in% names(df))
      ]
    ),
    n_datasets = purrr::map_int(
      column,
      \(col) sum(
        purrr::map_lgl(datasets, \(df) col %in% names(df))
      )
    )
  ) |>
  arrange(desc(n_datasets), column)

# dataset 2 contains all trials (+14 with NA trial), dataset 3 & 4
# are split up version of dataset 2 with additional information

# we therefore first merge datasets 3 and 4
key_vars <- c("vp", "studyday", "prompt_wd", "trial")

datasets$dataset34 <- bind_rows(
  datasets$dataset3,
  datasets$dataset4
)

# we then check whether the join keys are unique -> they are
datasets$dataset34 |>
  count(across(all_of(key_vars))) |>
  filter(n > 1)

datasets$dataset2 |>
  count(across(all_of(key_vars))) |>
  filter(n > 1)

# we then also merge dataset 2
datasets$dataset234 <- full_join(
  datasets$dataset34,
  datasets$dataset2,
  by = key_vars,
  suffix = c("_d34", "_d2")
)

# check whether we can merge them safely
cols_d34 <- names(datasets$dataset234) |>
  grep(pattern = "_d34$", value = TRUE)

base_names <- sub("_d34$", "", cols_d34)

comparison <- tibble(
  variable = base_names,
  col_d34 = paste0(base_names, "_d34"),
  col_d2 = paste0(base_names, "_d2")
) |>
  filter(col_d2 %in% names(datasets$dataset234)) |>
  rowwise() |>
  mutate(
    both_present = sum(
      !is.na(datasets$dataset234[[col_d34]]) &
        !is.na(datasets$dataset234[[col_d2]])
    ),
    different = sum(
      datasets$dataset234[[col_d34]] !=
        datasets$dataset234[[col_d2]] &
        !is.na(datasets$dataset234[[col_d34]]) &
        !is.na(datasets$dataset234[[col_d2]]),
      na.rm = TRUE
    ),
    only_d34 = sum(
      !is.na(datasets$dataset234[[col_d34]]) &
        is.na(datasets$dataset234[[col_d2]])
    ),
    only_d2 = sum(
      is.na(datasets$dataset234[[col_d34]]) &
        !is.na(datasets$dataset234[[col_d2]])
    )
  ) |>
  ungroup() |>
  select(variable, both_present, different, only_d34, only_d2)


# we can do that, so we fully merge:
for (v in comparison$variable) {
  datasets$dataset234[[v]] <- coalesce(
    datasets$dataset234[[paste0(v, "_d34")]],
    datasets$dataset234[[paste0(v, "_d2")]]
  )
}

cols_to_remove <- c(
  paste0(comparison$variable, "_d34"),
  paste0(comparison$variable, "_d2")
)

datasets$dataset234 <- datasets$dataset234 |>
  select(-any_of(cols_to_remove))

# check if it worked
names(datasets$dataset234) |>
  grep(pattern = "_d34$|_d2$", value = TRUE)


# we then also merge dataset 1
datasets$final <- full_join(
  datasets$dataset234,
  datasets$dataset1,
  by = c("vp", "studyday", "prompt_wd", "trial")
)

# consolidate the 10 matching pairs
# consolidated duplicate variables by preferring dataset34 values
#  and filling missing values from dataset2/dataset1
# retained time_of_day_c.x because the two centred versions used
# different centring constants
cols_x <- names(datasets$final) |>
  grep(pattern = "\\.x$", value = TRUE)

base_names <- sub("\\.x$", "", cols_x)

comparison_xy <- tibble(
  variable = base_names,
  col_x = paste0(base_names, ".x"),
  col_y = paste0(base_names, ".y")
) |>
  filter(col_y %in% names(datasets$final)) |>
  rowwise() |>
  mutate(
    both_present = sum(
      !is.na(datasets$final[[col_x]]) &
        !is.na(datasets$final[[col_y]])
    ),
    different = sum(
      datasets$final[[col_x]] != datasets$final[[col_y]] &
        !is.na(datasets$final[[col_x]]) &
        !is.na(datasets$final[[col_y]]),
      na.rm = TRUE
    ),
    only_x = sum(
      !is.na(datasets$final[[col_x]]) &
        is.na(datasets$final[[col_y]])
    ),
    only_y = sum(
      is.na(datasets$final[[col_x]]) &
        !is.na(datasets$final[[col_y]])
    )
  ) |>
  ungroup()


safe_vars <- comparison_xy |>
  filter(different == 0, variable != "time_of_day_c") |>
  pull(variable)

for (v in safe_vars) {
  datasets$final[[v]] <- coalesce(
    datasets$final[[paste0(v, ".x")]],
    datasets$final[[paste0(v, ".y")]]
  )
}

# keep the dataset34 version of time_of_day_c
datasets$final <- datasets$final |>
  select(
    -any_of(c(
      paste0(safe_vars, ".x"),
      paste0(safe_vars, ".y"),
      "time_of_day_c.y"
    ))
  )

# check that no .x/.y columns remain
names(datasets$final) |>
  grep(pattern = "\\.[xy]$", value = TRUE)

# check row count and duplicate keys
nrow(datasets$final)

datasets$final |>
  count(across(all_of(key_vars))) |>
  filter(n > 1)

# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
df <- datasets$final |>
  janitor::clean_names() |>
  rename(
    id = vp,
    day = studyday,
    beep = prompt_wd,
    counter = trigger_counter,
    time_of_day_c = time_of_day_c_x,
    money_rating = rating_mon,
    mood = mdbf_posrelaxed,
    mood_c = mdbf_posrelaxed_c,
    mood_day = mdbf_posrelaxed_day,
    mood_day_c = mdbf_posrelaxed_day_c,
    mood_person_c = mdbf_posrelaxed_pers_c,
    mood_person = mdbf_posrelaxed_pers,
    emotion_stimulus = emotion,
    time_since_sampling = testi2,
    bipolar_intensity_stimulus = intensity_bip,
    bipolar_intensity_stimulus_rescaled = intensity_bip_new,
    bipolar_intensity_stimulus_rescaled_c = intensity_bip_new_c,
    duration = durat_randomp,
    duration_sec = durat_randomp_sec
  )


#* Misc -------------------------------------------------------------------
# convert string sentinels ("NA", "", ...) and NaN to real NA
df <- recode_missing(df)


# Create metadata ---------------------------------------------------------
# one-time: writes a skeleton JSON from the cleaned columns if none exists yet;
# afterwards the JSON is edited directly
if (length(list.files(here("data", "metadata"), pattern = "^0074_")) == 0) {
  new_metadata_json("0074", "Schmitz", df)
}

# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
# Enter dataset ID here
metadata <- read_metadata("0074")


# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, metadata)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0074_schmitz_ts.tsv"))
}
