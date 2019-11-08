#' List available datasets with description
#'
#' This function will list all available datasets from the book. It also includes a description and origin, if applicable.
#'
#' @param be_silent Be silent?
#'
#' @return A dataframe with the data information
#' @export
#'
#' @examples
#' df_data <- afedR_list_available_data()
afedR_list_available_data <- function(be_silent = FALSE) {

  desc_file <- system.file('description_data.csv', package = 'afedR')

  df_desc <- readr::read_csv(desc_file, col_types = readr::cols())

  if (!be_silent) print(df_desc)

  return(invisible(df_desc))
}

#' Get path to data file
#'
#' This is a helper function of book "Analyzing Financial and Economic Data with R" by Marcelo S. Perlin.
#' With this function you'll be able to read the tables used in the book. Every table is located by its names.
#' In order to fin available tables, use function \link{afedR_list_available_data}.
#'
#' @param name_dataset Name of the dataset filename (see \link{afedR_list_available_data} for more details)
#'
#' @return A dataframe with the data
#' @export
#'
#' @examples
#' df <- afedR_get_data_file('grunfeld.csv')
afedR_get_data_file <- function(name_dataset) {

  df_available <- afedR_list_available_data(be_silent = TRUE)

  if (!(name_dataset %in% df_available$file_name)) {
    stop('Cant find name ', name_dataset, ' in list of available tables.')
  }

  path_out <- system.file(paste0('extdata/', name_dataset),
                          package = 'afedR')

  return(path_out)
}

