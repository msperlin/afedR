
afedR_build_exam <- function(students_names,
                             students_ids = paste0('Exam ', 1:length(students_names)),
                             class_name = 'My Class',
                             exercise_name = paste0('Sample Exercise'),
                             links_in_html = dplyr::tibble(text = 'Analyzing Financial and Economic Data with R',
                                                           url = 'https://www.msperlin.com/blog/publication/2020_book-afedr-en/'),
                             chapters_to_include = 1:16,
                             dir_out = 'html exams',
                             language = 'en') {

  path_exercises <- system.file('extdata/exam_files/exercise_files', package = 'afedR')
  available_exercises <- list.files(path_exercises, full.names = TRUE,
                                    recursive = TRUE)

  if (!dir.exists(dir_out)) dir.create(dir_out)

  chapter_names <- paste0('Chapter_', sprintf('%02d', chapters_to_include))
  idx <- stringr::str_sub(basename(available_exercises), 1, 10) %in%
    chapter_names

  exercises_to_compile <- available_exercises[idx]

  n_ver <- length(students_names)
  lan <<- language # global for language

  # set template
  template_html_file <- system.file('extdata/exam_files/templates/Exams_Template.html',
                                    package = 'afedR')

  my_temp_dir <- file.path(tempdir(), paste0('exams files ',
                                             basename(tempfile())) )

  my_exam <- exams::exams2html(file = exercises_to_compile,
                               n = length(students_names),
                               name = paste0(class_name, ' - ',
                                             exercise_name,
                                             ' - Version '),
                               encoding = 'UTF-8',
                               dir = my_temp_dir,
                               mathjax = TRUE,
                               template = template_html_file,
                               question = '',
                               verbose = TRUE, solution = F)

  df_answer_key <- dplyr::tibble()
  for (i_ver in seq(n_ver)){


    exam_now <- my_exam[[i_ver]]

    n_q <- length(exam_now)

    for (i_q in seq(n_q)){

      sol_now <- letters[which(exam_now[[i_q]]$metainfo$solution)]

      temp <- dplyr::tibble(i_name = students_names[i_ver],
                            i_ver = i_ver,
                            i_q = i_q,
                            solution = sol_now)

      df_answer_key <- dplyr::bind_rows(df_answer_key, temp)
    }

  }

  my_old_files <- list.files(my_temp_dir,
                             full.names = TRUE)[1:n_ver]

  # replace content of html and save new files
  l_args <- list(f_in = stringr::str_sort(my_old_files[1:n_ver],
                                          numeric = TRUE),
                 dir_out = dir_out,
                 student_name = students_names[1:n_ver],
                 student_version = 1:n_ver,
                 n_q = length(exercises_to_compile),
                 class_name = class_name,
                 exam_links = rep(list(links_in_html), n_ver),
                 exercise_name = exercise_name)

  l_out <- purrr::pwalk(.l = l_args, .f = afedR_add_html_content)

  df_answer_key_wide <- tidyr::spread(df_answer_key,
                                      key = i_q,
                                      value = solution)

  info_activity <- list()
  info_activity$answer_key <- df_answer_key_wide
  info_activity$answer_key_log <- gsub(Sys.time(),
                                       pattern = ':', replacement = '', fixed=TRUE)

  # save answer key
  message('')
  message('Writing answer key to csv file')
  f_out <- paste0('Answerkey-', class_name, '-',
                  exercise_name, '-',
                  gsub(Sys.time(),
                  pattern = ':', replacement = '', fixed=TRUE), '.csv')
  readr::write_csv(x = df_answer_key_wide, path = f_out)
  message('Done. File available at ', f_out)
  message('')
  message('All exam files are available at folder "', dir_out, '".')



  return(info_activity)

}

afedR_add_html_content <- function(f_in,
                                   dir_out,
                                   student_name,
                                   student_version,
                                   n_q,
                                   class_name,
                                   exercise_name,
                                   exam_links) {


  if (is.na(student_name)) {
    std.name <- 'ZZ-NO NAME'
  }

  message('Adding content to html: ', student_name)

  html_content <- paste0(readr::read_lines(f_in), collapse = '\n')

  # replace links for html

  base_str <- '<p><a href="%s"> %s </a>.</p>'
  text_itself <- sapply(exam_links, names)

  html_links <- paste0(sprintf(base_str, exam_links$url, exam_links$text),
                       collapse = '\n')

  # make replacements
  replace_vec <- list('EXAM_LINKS' = html_links,
                   'EXAM_NAME' = exercise_name,
                   'TAB_TITLE' = paste0(exercise_name, '-', student_name),
                   'STD_NAME' = student_name,
                   'STD_VERSION' = student_version,
                   'N_QUESTIONS' = n_q,
                   'CLASS_NAME' = class_name,
                   'DATE_COMPILE' = paste0(Sys.time(), ' at ', Sys.info()['nodename']))

  for (i_vec in seq(length(replace_vec))) {
    html_content <- stringr::str_replace_all(html_content,
                                             pattern = stringr::fixed(names(replace_vec[i_vec])),
                                             replacement = replace_vec[[i_vec]])
  }

  my_new_name <- file.path(dir_out,
                           paste0(exercise_name, '_',
                                  student_name, '_',
                                  stringr::str_c('Ver ', sprintf("%02d",student_version)),
                                  '.html') )

  cat(html_content, file = my_new_name, append = FALSE)

}


afedR_grade_exam <- function(l_in) {

}


#' Title
#'
#' @return
#' @export
#'
#' @examples
afedR_gen_rnd_vec <- function(){
  rnd.vec.1 <- c(1, seq(runif(1,0.1,0.2), runif(1,0.7,0.8), length.out = 4))
  rnd.vec.2 <- c(1, seq(runif(1,1.1,1.2), runif(1,1.7, 1.8), length.out = 4))
  rnd.vec.3 <- c(1, seq(runif(1,0.25,0.5),runif(1,0.6,0.8), length.out = 2),
                 seq(runif(1,1.2,2), length.out = 2))

  rnd.l <- list(rnd.vec.1, rnd.vec.2, rnd.vec.3)
  rnd.vec <- sample(rnd.l,1)[[1]]
  return(rnd.vec)
}
