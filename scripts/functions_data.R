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
    paper_doi = dataset_info$`Paper DOI`,
    link_to_data = dataset_info$`Link to data`,
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
