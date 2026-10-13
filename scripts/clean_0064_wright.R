# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(jsonlite)
library(osfr)
source(here::here("scripts", "functions_data.R"))

# Data --------------------------------------------------------------------
# Load data
if(!file.exists(here::here("data", "raw", "0064_wright_ts_raw.csv"))){
  osf_retrieve_file("https://osf.io/xvcpy") |>
    osf_download(path = here("data", "raw"))

  # rename data
  file_name <- osf_retrieve_file("https://osf.io/xvcpy") |> pull(name)
  file.rename(here::here("data", "raw", file_name), here::here("data", "raw", "0064_wright_ts_raw.csv"))
}

# read in data
dat <- read_delim(here::here("data", "raw", "0064_wright_ts_raw.csv"),
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)


# taking code from Wendt at al. (2020), order data by original timestamp
# https://osf.io/zj9uc
library(summarytools)
library(psych)
library(plyr)
library(data.table)
library(Hmisc)
library(dplyr)

dummy2 <- data.frame(ddply( dat, .(UsrID), function(x) length(x$UsrID) ))

# remove NA beeps
which.na <- which(dat$tmstmp1 == 2)
dat1 <- dat[-which.na,]
dat1 <- unique(dat1)
dat1$na.sum <- rowSums(is.na(dat1))

which.na.sum <- which(dat1$na.sum > 4)
dat1 <- dat1[-which.na.sum ,]

dat1 <- dat1[
  with(dat1, order(UsrID, Date, tmstmp1)),
]


#--- added by us afterwards

dat1$trial <- NA

#---

trial <- 0
id <- 0
i <- 1
date <- 0
#  if (dat1$date[i]==id) {

for (i in 1:nrow(dat1)){
  if (dat1$UsrID[i]==id) {
    trial <- trial+1
  } else {
    trial <- 0
  }
  id <- dat1$UsrID[i]
  dat1$trial[i] <- trial+1
}

date <- 1160
id <- 80000
trial <- 1

for (i in 1:nrow(dat1)){

  if (dat1$UsrID[i]==id) {

    if (dat1$Date[i]==date) {trial <- trial+1} else {trial <- trial+2}

    dat1$trial[i] <- trial
    date <- dat1$Date[i]

  } else

  {
    trial <- 1
    date <- dat1$Date[i]
    id <- dat1$UsrID[i]
  }

}

min.occ = 20 #minimum of occasions per subject (=/= subsequent occasions)
max.occ = 600 #maximum of occasions per subject (here = 30)

#############################
#############################
####
####    Datenaufbereitung
####
#############################


# Daten ordnen nach vp_id und tbday
dat1 <- dat1[with(dat1, order(UsrID, trial)),]


dat <- dat1

colnames(dat) <- c("tmstmp1", "UsrID", "PTNUM", "Dyad", "Date", "Afraid", "Ashamed",
                   "Distressed", "Guilty", "Hostile", "Irritable", "Jittery",
                   "Nervous", "Scared", "Upset", "Frightened", "Shaky", "Angry",
                   "Scornful", "Disgusted", "Loathing", "Sad", "Blue", "Downhearted",
                   "Alone", "Lonely", "Active", "Alert", "Attentive", "Determined",
                   "Enthusiastic", "Excited", "Inspired", "Interested", "Proud",
                   "Strong","na.sum" , "trial")
dat <- dat[c(38,1:36)]
#view(dfSummary(dat))

daily_trial <- 0
Date <- 0
i <- 1

for (i in 1:nrow(dat)){
  if (dat$Date[i]==Date) {
    daily_trial <- daily_trial+1
  } else {
    daily_trial <- 0
  }
  Date <- dat$Date[i]
  dat$daily_trial[i] <- daily_trial+1
}


day <- 1
Date <- 0
i <- 1
UsrID <- 0

for (i in 1:nrow(dat)){

  if (dat$UsrID[i]==UsrID) {
  } else {
    day <- 0
  }

  if (dat$Date[i]==Date) {
  } else {
    day <- day+1
  }
  Date <- dat$Date[i]
  UsrID <- dat$UsrID[i]
  dat$day[i] <- day
}

dat <- dat[c(39,38,1:37)]
colnames(dat) <- c("day", "beep", "trial", "tmstmp1", "id", "PTNUM", "Dyad","Date","Afraid","Ashamed", "Distressed",
                   "Guilty" ,"Hostile","Irritable","Jittery","Nervous","Scared",
                   "Upset","Frightened","Shaky","Angry","Scornful", "Disgusted","Loathing","Sad","Blue",
                   "Downhearted", "Alone", "Lonely", "Active", "Alert","Attentive" ,
                   "Determined", "Enthusiastic", "Excited","Inspired","Interested",
                   "Proud", "Strong")

df <- dat

unloadNamespace("summarytools")
unloadNamespace("rapportools")
unloadNamespace("reshape2")
unloadNamespace("plyr")
unloadNamespace("Hmisc")

# Cleaning ----------------------------------------------------------------
#* Column Names -----------------------------------------------------------
df <- dat |>
  janitor::clean_names() |>
  dplyr::rename(
    pt_num = ptnum,
    counter = trial,
    timestamp = tmstmp1,
  ) |>
  # some reordering
  dplyr::select(id, pt_num, day, beep, counter, timestamp, date, dplyr::everything())


#* Misc -------------------------------------------------------------------
# double check if there is a character NA somewhere
sapply(df, function(col) any(col == "NA", na.rm = TRUE))


# Read metadata -----------------------------------------------------------
# loaded before checking so check_data() can cross-check data against metadata
metadata <- read_metadata("0064")

# Check requirements ------------------------------------------------------
# errors abort; warnings flag likely problems but still allow saving
check_results <- check_data(df, metadata)

# if it returns "Data are clean.", save the data
if(check_results == "Data are clean."){
  write_tsv(df, here::here("data", "clean", "0064_wright_ts.tsv"))
}
