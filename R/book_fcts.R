#' Gets official links from book
#'
#' @return A list with links
#' @export
#'
#' @examples
#'
#' print(afedR_get_links_book())
afedR_get_links_book <- function() {

  my_l <- list(book_site = 'https://www.msperlin.com/blog/publication/2017_book-pafdr-en/',
               book_site_zip = 'https://www.msperlin.com/blog/files/afedR/afedR_files.zip',
               blog_site = 'https://www.msperlin.com/blog')

  return(my_l)

}

#' Donwload and unzip book file
#'
#' @param path_to_unzip Path to unzip the book files
#'
#' @return TRUE, if sucessful
#' @export
#'
#' @examples
#' \dontrun{
#' flag <- afedR_unzip_book_file()
#' }
afedR_unzip_book_file <- function(path_to_unzip = '~') {

  zip_file <- system.file('extdata/afedR_files.zip', package = 'afedR')

  message('Unzipping files to ', path_to_unzip)

  utils::unzip(zip_file, exdir = path_to_unzip)

  files <- list.files(path_to_unzip,
                      full.names = TRUE,
                      recursive = TRUE)

  name_in_zip <- 'afedR files'
  message(paste0('Unzipped ', length(files), ' files at ',
                 file.path(path_to_unzip, name_in_zip)))
  message(paste0('\tR code available at ', file.path(path_to_unzip, name_in_zip, 'r-code')))
  message(paste0('\tData files available at ', file.path(path_to_unzip, name_in_zip, 'data')))
  message(paste0('\tSlides available at ', file.path(path_to_unzip, name_in_zip, 'slides')))
  message(paste0('\tSolution to exercises at ', file.path(path_to_unzip, name_in_zip, 'exercises')))

  return(invisible())
}
