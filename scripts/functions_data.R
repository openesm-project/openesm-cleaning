check_data <- function(data){

  # Check if the data is a dataframe
  if(!is.data.frame(data)){
    stop("Data is not a dataframe")
  }

  # Check if the data has an "id" column
  if(!"id" %in% colnames(data)){
    stop("Data does not have an 'id' column")
  }

  # Check if the data has a "day" column
  if(!"day" %in% colnames(data)){
    stop("Data does not have a 'day' column")
  }

  # Check if the data has a "beep" column
  if(!"beep" %in% colnames(data)){
    stop("Data does not have a 'beep' column")
  }

  # Check if the column names are in snake case
  if(!all(colnames(janitor::clean_names(data)) == colnames(data))){
    stop("Column names are not in snake case. Use janitor::clean_names().")
  }

  return("Data are clean.")

}

# create metadata json file
create_metadata_json <- function(dataset_id) {

  # Extract dataset-level info
  dataset_info <- meta_data |>
    dplyr::filter(id == dataset_id)

  # Extract feature-level info
  dataset_features <- variable_data |>
    dplyr::filter(id == dataset_id) |>
    dplyr::mutate_all(~ ifelse(is.na(.), "", .)) |>
    dplyr::select(!id)

  # Construct JSON structure
  list(
    first_author = dataset_info$Author,
    dataset = dataset_info$id,
    year = dataset_info$Year,
    reference_a = dataset_info$`Reference A`,
    reference_b = dataset_info$`Reference B`,
    paper_doi = dataset_info$`Paper DOI`,
    link_to_zenodo = dataset_info$`Link to Zenodo`,
    link_to_data = dataset_info$`Link to data`,
    link_to_codebook = dataset_info$`Optional: Link to Codebook`,
    link_to_code = dataset_info$`Optional: Link to Code`,
    n_participants = dataset_info$`N Participants`,
    n_time_points = dataset_info$`N Time Points`,
    n_beeps_per_day = dataset_info$`N Beeps/Day`,
    passive_data_available = dataset_info$`Passive data available?`,
    cross_sectional_available = dataset_info$`Cross sectional available?`,
    topics = dataset_info$Topics,
    implicit_missingness = dataset_info$`Implicit Missingness?`,
    raw_time_stamp = dataset_info$`Raw Time Stamp available?`,
    sampling_scheme = dataset_info$`Sampling Scheme`,
    participants = dataset_info$Participants,
    coding_file = dataset_info$`Coding File`,
    additional_comments = dataset_info$`additional comments`,
    coder_data = dataset_info$`Coder Data`,
    coder_metadata = dataset_info$`Coder Metadata`,
    features = dataset_features
  )
}


# Metadata analysis -------------------------------------------------------
# List all variable-construct pairs to understand construct annotation
list_variable_construct_pairs <- function(folder_path) {
  # list all metadata json files
  json_files <- list.files(folder_path, pattern = "\\.json$", full.names = TRUE)

  if (length(json_files) == 0) {
    stop("No JSON files found in the specified folder.")
  }

  # initialize storage tibble
  variable_construct_pairs <- dplyr::tibble(
    dataset_id = character(),
    variable_name = character(),
    construct_name = character()
  )

  for (file_path in json_files) {
    metadata <- jsonlite::fromJSON(file_path, simplifyVector = FALSE)
    dataset_id <- metadata$dataset

    # iterate features
    if (!is.null(metadata$features) && length(metadata$features) > 0) {
      for (feature in metadata$features) {
        if (!is.null(feature$construct) && feature$construct != "") {
          variable_name <- feature$name
          construct_name <- feature$construct

          # add to the tibble
          variable_construct_pairs <- variable_construct_pairs |>
            dplyr::add_row(
              dataset_id = dataset_id,
              variable_name = variable_name,
              construct_name = construct_name
            )
        }
      }
    }
  }

  return(variable_construct_pairs)
}

