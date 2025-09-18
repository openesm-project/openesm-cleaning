# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(googlesheets4)
library(jsonlite)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# downloaded data from GitHub
# https://github.com/emoriebeck/behavior-prediction/blob/main/04-data/01-raw-data/esm_cleaned_combined_2021-04-07.RData

load(here("data", "raw", "0060_beck_ts_raw.RData"))

#* prepare data for merging -----------------------------------------------
# remove empty column from one dataframe
bfi <- bfi |>
  select(-answer)

df_all <- bfi |>
  mutate(df = "bfi") |>
  bind_rows(ds8 |>
              mutate(df = "ds8")) |>
  bind_rows(emo |>
              mutate(df = "emo")) |>
  bind_rows(sit |>
              mutate(df = "sit"))

# if orig_itemname is empty, enter "itemname"
df_all <- df_all |>
  mutate(orig_itemname = ifelse(orig_itemname == "", itemname, orig_itemname)) |>
  mutate(orig_itemname = ifelse(is.na(orig_itemname), itemname, orig_itemname))


# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
df <- df_all |>
  janitor::clean_names()

# when trying to pivot wider, we realized that one individual consistently had duplicate
# answers. However, these duplicates were identical, as we can see in the following
df |>
  select(-c(facet, trait, type)) |>
  group_by(
    trial_index,
    category,
    reverse_code,
    df,
    sid,
    date,
    hour,
    minute,
    start_date,
    day,
    hour_block_1,
    hour_block,
    orig_itemname
  ) |>
  summarise(n = n(), n_distinct_responses = n_distinct(responses2)) |>
  ungroup() |>
  filter(n > 1L) |>
  filter(n_distinct_responses > 1L)

# we then pivot wider to have one row per beep and remove duplicates
df_wide <- df |>
  select(-c(facet, trait, type)) |>
  pivot_wider(names_from = orig_itemname, values_from = responses2,
              id_cols = c(sid, date, hour, minute, start_date, day, hour_block_1, hour_block),
              values_fn = max)

# better column names
df_wide <- df_wide |>
  rename(
    id = sid,
    beep = hour_block,
    first_beep = hour_block_1,
    # Extraversion - Sociability
    outgoing = E1,
    talkative = E2,
    quiet = E3,
    shy = E4,
    # Extraversion - Assertiveness
    assertive = E5,
    dominant = E6,
    influence_hard = E7,
    others_charge = E8,
    # Extraversion - Energy Level
    energetic = E9,
    enthusiastic = E10,
    rarely_excited = E11,
    less_active = E12,
    # Agreeableness - Compassion
    compassionate = A1,
    helpful = A2,
    little_sympathy = A3,
    cold = A4,
    # Agreeableness - Respectfulness
    respectful = A5,
    polite = A6,
    starts_arguments = A7,
    rude = A8,
    # Agreeableness - Trust
    forgiving = A9,
    assumes_best = A10,
    finds_fault = A11,
    suspicious = A12,
    # Conscientiousness - Organization
    systematic = C1,
    neat_tidy = C2,
    disorganized = C3,
    messy = C4,
    # Conscientiousness - Productiveness
    efficient = C5,
    persistent = C6,
    lazy = C7,
    difficulty_starting = C8,
    # Conscientiousness - Responsibility
    dependable = C9,
    reliable = C10,
    careless = C11,
    irresponsible = C12,
    # Negative Emotionality - Anxiety
    tense = N1,
    worries = N2,
    relaxed = N3,
    rarely_anxious = N4,
    # Negative Emotionality - Depression
    sad = N5,
    depressed = N6,
    optimistic_setback = N7,
    secure_comfortable = N8,
    # Negative Emotionality - Emotional Volatility
    moody = N9,
    temperamental = N10,
    emotionally_stable = N11,
    controls_emotions = N12,
    # Open-Mindedness - Intellectual Curiosity
    curious = O1,
    deep_thinker = O2,
    avoids_intellectual = O3,
    little_abstract = O4,
    # Open-Mindedness - Aesthetic Sensitivity
    fascinated_art = O5,
    values_beauty = O6,
    few_artistic = O7,
    poetry_boring = O8,
    # Open-Mindedness - Creative Imagination
    inventive = O9,
    original = O10,
    little_creativity = O11,
    difficulty_imagining = O12,
    # Additional D items
    duty = D1,
    intellect = D2,
    adversity = D3,
    mating = D4,
    positivity = D5,
    negativity = D6,
    deception = D7,
    sociality = D8,
    # Situational items
    studying = sit_01,
    argument_friend = sit_02,
    argument_family = sit_03,
    interacted_friend = sit_04,
    interacted_family = sit_05,
    lost_something = sit_06,
    late = sit_07,
    forgot_something = sit_08,
    bored_schoolwork = sit_09,
    excited_schoolwork = sit_10,
    anxious_schoolwork = sit_11,
    tired = sit_12,
    sick = sit_13,
    sleeping = sit_15,
    in_class = sit_16,
    music = sit_17,
    internet = sit_18,
    tv = sit_19,
    procrastinating = sit_20,
    lonely = sit_21,
    goal_directed = goaldir)


# we double-check the reverse coding of item O7
cor(as.numeric(df_wide$quiet), as.numeric(df_wide$outgoing), use = "pairwise.complete.obs")

#* Misc -------------------------------------------------------------------
# remove weird text from "minute" field
df_wide <- df_wide |>
  mutate(minute = str_remove(minute, ".csv"))

# check for character NA
df_wide <- df_wide |>
  mutate(across(where(is.character), ~ na_if(., "NA")))

# better order of columns
df_wide <- df_wide |>
  select(id, day, beep, everything())

df <- df_wide

# Check requirements ------------------------------------------------------
# if check_data runs without messages, the data are clean
# and should be saved as a .tsv file
check_results <- check_data(df)

# if it returns "Data are clean.", save the data
# Enter data set ID here
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0060_beck_ts.tsv"))
}


# Create metadata ---------------------------------------------------------
metadata_url <- "https://docs.google.com/spreadsheets/d/1ALGCq_jN6I4dcjWYQ_LQe9o52DGJItwdu9fCkwOh6fg/edit?pli=1&gid=0#gid=0"
meta_data <- read_sheet(metadata_url)


# Enter dataset ID here
sheet_url <- meta_data |>
  filter(dataset_id == "0060") |>
  pull("Coding File URL")

variable_data <- read_sheet(sheet_url)

meta_json <- create_metadata_json("0060") |>
  toJSON(pretty = TRUE, auto_unbox = TRUE)

write(meta_json, here("data", "metadata", "0060_beck_metadata.json"))
