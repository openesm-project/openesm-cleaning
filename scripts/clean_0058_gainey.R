# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(jsonlite)
library(osfr)
source(here("scripts", "functions_data.R"))



# Data --------------------------------------------------------------------
# download data
if(!file.exists(here("data", "raw", "0058_gainey_ts_raw.sav"))){
  osf_retrieve_file("https://osf.io/svj6w") |>
    osf_download(path = here("data", "raw"))


  # rename data
  file_name <- osf_retrieve_file("https://osf.io/svj6w") |> pull(name)
  file.rename(here("data", "raw", file_name), here("data", "raw", "0058_gainey_ts_raw.sav"))
}

df_raw <- haven::read_spss(here("data", "raw", "0058_gainey_ts_raw.sav"))

# split demographics off already
df <- df_raw[, c(1, 183:222)]

df_demographics <- df_raw[, c(1 : 182, 223:226)]

# save demographics
write_tsv(df_demographics, here("data", "clean", "0058_gainey_static.tsv"))

# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
df <- df |>
  janitor::clean_names() |>
  rename(
    beep = sig,
    counter = signal,
    counter_all = sig_cum,
    time_since_start = run_time,
    afraid_anxious = afraid_anx,
    observe_thoughts_feelings = decenter1,
    struggled_thoughts_feelings = decenter2,
    detached_thoughts_feelings = decenter3,
    caught_up_thoughts = decenter4,
    symptom_experience = exp_sx,
    symptom_interfere = sx_interfere,
    depressed = dys1,
    anhedonia = dys2,
    inadequate = dys3,
    discouraged = dys4,
    reappraisal = reapprais,
    brooding = brood,
    savoring = savor,
    resist_temptation = resist,
    getting_done = produce,
    mental_exhaust = exhaust
  )


#* Misc -------------------------------------------------------------------
# recode all -999 and -999.0 and "-999" to NA
df[df == -999 | df == -999.0 | df == "-999"] <- NA

# remove sum-scores and reverse-scored item
df <- df |>
  select(!c(pa, na, self_ctrl, dysphoria, wellbeing,
            r_exhaust))




# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
metadata <- read_metadata("0058")

# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, metadata)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here("data", "clean", "0058_gainey_ts.tsv"))
}
