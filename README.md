# Repository for R package afedR 

Includes several functions related to book "Analyzing Financial and Economic Data with R", available in the following formats:

- [Amazon](https://www.amazon.com/dp/B084LSNXMN) - full length book
- [Online version](https://www.msperlin.com/afedR/) - first seven chapters


## Installation

```
# only in github (will not pass cran checks)
devtools::install_github('msperlin/afedR')
```

## Example of usage

### Listing available datasets

```
afedR::list_available_data()
```

### Fetching data from book repository

```
file_name <- 'SP500.csv'
path_to_file <- afedR::get_data_file(file_name)

df <- readr::read_csv(path_to_file)
```

### Copying all book files to local directory

```
flag <- afedR::get_book_files(path_to_copy = '~')
```

### Exporting exercises

See this [blog post](https://www.msperlin.com/blog/post/2021-02-28-dynamic-exercises-afedr/) for details.
