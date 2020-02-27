# Repository for R package afedR 

Includes several functions related to book "Analyzing Financial and Economic Data with R", available in [Amazon](https://www.amazon.com/dp/B084LSNXMN).

## Installation

```
# only in github (will not pass cran checks)
devtools::install_github('msperlin/afedR')
```

## Example of usage

### Listing available datasets

```
afedR::afedR_list_available_data()
```

### Fetching data from book repository

```
file_name <- 'SP500.csv'
path_to_file <- afedR::afedR_get_data_file(file_name)

df <- readr::read_csv(path_to_file)
```

### Copying all book files to local directory

```
flag <- afedR::afedR_get_book_files(path_to_copy = '~')
```

### Building html exams (testing)

```
afedR::afedR_build_exam(students_names = c('George', 'Michael'), 
                        chapters_to_include = 2,
                        dir_out = tempdir())
```
