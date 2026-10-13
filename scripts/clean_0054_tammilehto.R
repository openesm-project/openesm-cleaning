# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(jsonlite)
library(osfr)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# Read in data
if(!file.exists(here::here("data", "raw", "0054_tammilehto_ts_raw.dat"))){
  osf_retrieve_file("https://osf.io/r5jkc") |>
    osf_download(path = here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/r5jkc") |> pull(name)
  file.rename(here::here("data", "raw", file_name), here::here("data", "raw", "0054_tammilehto_ts_raw.dat"))
}

# read data
df_raw <- read.table(here("data", "raw", "0054_tammilehto_ts_raw.dat"), header = FALSE)


# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
# need to name them manually based on the order of the codebook
colnames(df_raw) <- c(
  "id",  # originally kandi_id
  "discrete_time", # originally aika
  "time_elapsed",
  "reappraisal",
  "rumination",
  "suppression",
  "feel_loved", # originally security1
  "rely_on", # originally security2
  "security",
  "need_love", # originally anxiety1
  "share_feelings", # originally anxiety2
  "anxiety",
  "keep_distance", # originally avoidance1
  "closeness_nervous", # originally avoidance2
  "avoidance",
  "trait_avoidance",
  "trait_anxiety",
  "trait_neuroticism",
  "ses",
  "age",
  "lag_security",
  "lag_anxiety",
  "lag_avoidance"
)

#* Misc -------------------------------------------------------------------
# convert single "." to NA
df_raw[df_raw == "."] <- NA

# remove unnecessary columns
df <- df_raw |>
  select(!c(security, anxiety, avoidance,
            lag_security, lag_anxiety, lag_avoidance))

# split off demographics
df_demographics <- df |>
  select(id, ses, age, contains("trait")) |>
  # filter out rows where everything except id is NA
  filter(!if_all(-id, is.na)) |>
  group_by(id) |>
  distinct()

write_tsv(df_demographics, here("data", "clean", "0054_tammilehto_static.tsv"))

# remove demographics from main data frame
df <- df |>
  select(!c(ses, age, contains("trait")))

# We initially checked which counters were all NA, but then decided to keep them all
# the code is still here:

# # we then noticed a strange pattern: it seems like every other 7 rows are empty
# # removing them would better align with the maximum number of observations per individual
# # Check the pattern for each participant
# df_raw |>
#   group_by(id) |>
#   select(!counter) |>
#   mutate(
#     row_num = row_number(),
#     should_be_na = row_num %in% c(8:14, 22:28, 36:42, 50:56, 64:70, 78:84, 92),
#     is_na = rowSums(is.na(across(everything()))) == (ncol(df_raw) - 2)
#   ) |>
#   filter(should_be_na) |>
#   summarise(
#     total_na_rows = n(),
#     actually_na = sum(is_na),
#     pattern_correct = all(is_na)
#   )
#
#
# # check which counters are NA for everyone
# n_participants <- df |>
#   distinct(id) |>
#   nrow()
#
# all_na <- df |>
#   select(!id) |>
#   group_by(counter) |>
#   summarise(
#     # number of missings in row
#     all_na = rowSums(is.na(across(everything()))),
#   ) |>
#   filter(all_na == ncol(df) - 2) |>
#   ungroup() |>
#   count(counter) |>
#   filter(n == n_participants) |>
#   pull(counter)
#
# # remove these from the data
# df <- df |>
#   filter(!counter %in% all_na)

# day and beep are not fully clear
df$day <- NA
df$beep <- NA



# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
metadata <- read_metadata("0054")

# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, metadata)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0054_tammilehto_ts.tsv"))
}
