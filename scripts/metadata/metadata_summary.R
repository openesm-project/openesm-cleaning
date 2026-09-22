# summarize metadata for all datasets
library(tidyverse)
library(jsonlite)
library(here)
library(writexl)
library(googledrive)

source(here::here("scripts", "functions_data.R"))

# List combinations of variables and constructs
variables_constructs <- list_variable_construct_pairs(here::here("data", "metadata"))

# save as xlsx and RDS
write_xlsx(variables_constructs, here::here("data", "variables_constructs.xlsx"))
write_rds(variables_constructs, here::here("data", "variables_constructs.rds"))

# upload to googledrive
drive_auth()
drive_upload(
  media = here::here("data", "variables_constructs.xlsx"),
  path = as_id("1loG0MQpHJsMfSATwzjpQVxy-DHOhcVPu"))
drive_upload(
  media = here::here("data", "variables_constructs.rds"),
  path = as_id("1loG0MQpHJsMfSATwzjpQVxy-DHOhcVPu"))


# Simply list all variables
all_variables <- list_variable_construct_pairs(here::here("data", "metadata"),
                                               include_all = TRUE)

all_variables_vec <- all_variables |>
  distinct(variable_name) |>
  pull(variable_name)


# write them to a .txt file
all_variables |>
  select(dataset_id, variable_name) |>
  write_delim(
    file = here::here("all_variables.txt"),
    delim = "\t",
    col_names = FALSE
  )





