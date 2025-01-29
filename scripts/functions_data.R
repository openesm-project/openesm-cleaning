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
