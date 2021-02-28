#' # Importing Data from Local Files {#importing}
#' 

#' 
#' Surely, the very first step of script is gettin
#' 
#' Here we will draw a comprehensive list of file 
#' 
#' - Text data with comma-separated values (_csv_)
#' - Microsoft Excel (_xls_, _xlsx_);
#' - R native files (_RData_, _rds_);
#' - `fst` format;
#' - SQLite;
#' - Unstructured text data.
#' 
#' The first lesson in importing data from local f
#' 
## -------------------------------------------------------------------------------------------------------------------
my_file <- 'C:/My Research/data/SP500_Data.csv'

#' 
#' Note the use of forwarding slashes (`/`) to des
#' 
## -------------------------------------------------------------------------------------------------------------------
my_file <- 'data/SP500_Data.csv'

#' 
#' Here, it is assumed that in the current working
#' 
## ---- eval=FALSE----------------------------------------------------------------------------------------------------
## setwd('C:/My Research')
## my_file <- 'data/SP500_Data.csv'

#' 
## Here, again, I reinforce the use of _tab_ and the _autocomplete_ tool of RStudio. It is much more **easier and practical** to find files on your computer's hard disk using _tab_ navigation than to copy and paste the address from your file explorer. To use it, open double or quotes in RStudio, place the _mouse_ cursor in between the quotes and press _tab_.

#' 
#' Another very important point here is that **the
#' 
#' Each column in the `dataframe` will have its ow
#' 
#' 
#' ## _csv_ files 
#' 

#' 
#' Consider a data file called `r basename(my_f)`,
#' 
## ---- eval=FALSE----------------------------------------------------------------------------------------------------
## # install devtools dependency
## install.packages('devtools')
## 
## # install book package
## devtools::install_github('msperlin/afedR')

#' 
#' Once you installed package `afedR`, file `r bas
#' 
#' Let's copy `r basename(my_f)` to your "My Docum
#' 
## ---- eval=FALSE----------------------------------------------------------------------------------------------------
## my_f <- afedR::get_data_file('SP500.csv')
## file.copy(from = my_f, to = '~' )

#' 
#' Now, if it is your first time working with _.cs
#' 

#' 
#' The content of `r basename(my_f)` is standard a
#' 
#' 1) Check the existence of text before the actua
#' 
#' 2) Verify the existence of names for all column
#'    
#' 3) Check the symbol for column separation. Norm
#'    
#' 4) For the numerical data, verify the decimal s
#' 
#' 5) Check the encoding of the text file. Normall
#' 
## Whenever you find an unexpected text structure in a _.csv_ file, use the arguments of the _csv_ reading function to import the information correctly. As a rule of thumb, **never modify raw .csv data manually**. Its far more efficient to use the R code to deal with different structures of _.csv_ files. It takes a bit of work, but such a policy will save you a lot of time in the future as, in a couple of months, you are unlikely to remember how you manually cleaned that _.csv_ file for your R script.

#' 
#' 
#' ### Importing Data
#' 
#' The `base` package of R includes a native funct
#' 
#' This is the first package from the `tidyverse` 
#' 
## ---- eval=FALSE----------------------------------------------------------------------------------------------------
## install.packages('tidyverse')

#' 
#' After running the previous code, all `tidyverse
#' 
## -------------------------------------------------------------------------------------------------------------------
# load library
library(tidyverse)

#' 
#' Back to importing data from _.csv_ files, to lo
#' 
## ---- message=TRUE--------------------------------------------------------------------------------------------------
# set file to read
my_f <- afedR::get_data_file('SP500.csv')

# read file
my_df_sp500 <- read_csv(my_f)

# print it
print(head(my_df_sp500))

#' 
#' The contents of the imported file becomes a `da
#' 
## -------------------------------------------------------------------------------------------------------------------
# Check the content of dataframe
glimpse(my_df_sp500)

#' 
#' Note that the column of dates -- `ref.date` -- 
#' 
#' Notice how the previous code presented a messag
#' 
## -------------------------------------------------------------------------------------------------------------------
# set cols from import message
my_cols <- cols(ref.date = col_date(),
                price.close = col_character() ) 

# read file with readr::read_csv
my_df_sp500 <- read_csv(my_f, col_types = my_cols)

#' 
#' As an exercise, Let's import the same data, but
#' 
## -------------------------------------------------------------------------------------------------------------------
# set cols from import message
my_cols <- cols(ref.date = col_character(),
                price.close = col_character() ) 

# read file with readr::read_csv
my_df_sp500 <- read_csv(my_f, col_types = my_cols)

# glimpse the dataframe
glimpse(my_df_sp500)

#' 
#' As expected, both columns are of class `charact
#' 
#' There is also a simpler way of using the classe
#' 
## -------------------------------------------------------------------------------------------------------------------
# read file with readr::read_csv
my_df_sp500 <- read_csv(my_f, 
                        col_types = cols())

# glimpse the dataframe
glimpse(my_df_sp500)

#' 
#' Going further, `read_csv` has several other inp
#' 
#' - change the format of the import data, includi
#' - change column names (argument `col_names`);
#' - skip _n_ lines before importation (`skip` opt
#' - custom definition for NA values (`na` option)
#' 
#' Now, let's study a more abnormal case of _.csv_
#' 
#' - the header has textual information;
#' - the file will use the comma as a decimal;
#' - the file text will contain Latin characters.
#' 
#' The first 5 lines of the files contain the foll
#' 
## ---- echo=FALSE----------------------------------------------------------------------------------------------------
my_f <- afedR::get_data_file('funky_csv_file.csv')

message(paste0(read_lines(my_f, 
                          n_max = 5), 
               collapse = '\n'))

#' 
#' He ave a header text up to line number 7 and th
#' 
#' 
## -------------------------------------------------------------------------------------------------------------------
my_f <- adfeR::get_data_file('funky_csv_file.csv')

df_funky <- read_csv(my_f, 
                     col_types = cols())

glimpse(df_funky)

#' 
#' Clearly something went wrong, with the issue of
#' 
## -------------------------------------------------------------------------------------------------------------------
df_not_funky <- read_delim(
        file = my_f, 
        skip = 7, # how many lines do skip
        delim = ';', # column separator
        col_types = cols(), # column types
        locale = locale(decimal_mark = ',')# locale
)

glimpse(df_not_funky)

#' 
#' Note that the data has now been correctly impor
#' 
#' 
#' ### Exporting Data
#' 
#' To write a _.csv_ file, use the `readr::write_c
#' 
## -------------------------------------------------------------------------------------------------------------------
# set the number of rows
N <- 100

# set dataframe
my_df <- data.frame(y = runif(N), 
                    z = rep('a',N))

# print it
print(head(my_df))

#' 
#' And now we use `write_csv` to save it in a new 
#' 
## -------------------------------------------------------------------------------------------------------------------
# set file out
f_out <- 'data/temp.csv'

# write to files
write_csv(x = my_df,  
          file = f_out)

#' 
#' In the previous example, we save the object `my
#' 
## -------------------------------------------------------------------------------------------------------------------
# read it
my_df_imported <- read_csv(f_out)

# print first five rows
print(head(my_df_imported))

#' 
#' As we can see, the data imported from the file 
#' 
#' 
#' ## _Excel_ Files (_xls_ and _xlsx_) 
#' 
#' Although it is not an efficient or portable dat
#' 
#' The downside of using Excel files for storing d
#' 
#' 
#' ### Importing Data
#' 
#' R does not have a native function for importing
#' 
#' Despite their similar goals, each package has i
#' 
#' In this section, we will give priority to packa
#' 
## -------------------------------------------------------------------------------------------------------------------
library(readxl)

# set excel file
my_f <- afedR::get_data_file('SP500_Excel.xlsx')

# read excel file 
my_df <- read_excel(my_f, sheet = 'Sheet1')

# print classes
print(sapply(my_df, class))

# print with head (first five rows)
print(head(my_df))

#' 
#' As we can see, one benefit of using Excel files
#' 
#' 
#' ### Exporting Data 
#' 
#' Exporting a `dataframe` to an Excel file is als
#' 
#' An example of `xlsx` usage is given next: \inde
#' 
## -------------------------------------------------------------------------------------------------------------------
library(xlsx)

# create dataframe
N <- 50
my_df <- data.frame(y = seq(1,N), z = rep('a',N))

# set excel file
f_out <- 'data/temp.xlsx'

# write to excel
write.xlsx(x = my_df, file = f_out, sheetName = "my df")

#' 
#' If you want to save several `dataframes` into s
#' 
## ---- tidy=FALSE----------------------------------------------------------------------------------------------------
# create two dataframes
N <- 25
my_df_A <- data.frame(y = seq(1, N), 
                      z = rep('a', N))

my_df_B <- data.frame(z = rep('b', N))

# set file out
f_out <- 'data/temp.xlsx'

# write in different sheets
write.xlsx(x = my_df_A, 
           file = f_out, 
           sheetName = "my df A")

write.xlsx(x = my_df_B, 
           file = f_out, 
           sheetName = "my df B", 
           append = TRUE )

#' 
#' After executing the code, we can open the excel
#' 
#' As for package `writexl`, its innovation is tha
#' 
## ---- eval=FALSE----------------------------------------------------------------------------------------------------
## library(writexl)
## # set number of rows
## N <- 25
## 
## # create random dfs
## my_df_A <- data.frame(y = seq(1, N),
##                       z = rep('a', N))
## 
## write_xlsx(x = my_df_A,
##            path = f_out)

#' 
#' In order to compare writing performance, let's 
#' 
## -------------------------------------------------------------------------------------------------------------------
library(writexl)
library(readxl)
library(xlsx)

# set number of rows
N <- 2500

# create random dfs
my_df_A <- data.frame(y = seq(1,N),
                      z = rep('a',N))

# set files
my_file_1 <- 'data/temp_writexl.xlsx'
my_file_2 <- 'data/temp_xlsx.xlsx'

# test export
time_write_writexl <- system.time(write_xlsx(x = my_df_A,
                                             path = my_file_1))

time_write_xlsx <- system.time(write.xlsx(x = my_df_A,
                                          file = my_file_2))

# test read
time_read_readxl <- system.time(read_xlsx(path = my_file_1 ))
time_read_xlsx <- system.time(read.xlsx(file = my_file_2,
                                        sheetIndex = 1 ))

#' 
#' And now we show the results:
#' 
## ---- results='hold'------------------------------------------------------------------------------------------------
# results
my_formats <- c('xlsx', 'readxl')
results_read <- c(time_read_xlsx[3], time_read_readxl[3])
results_write<- c(time_write_xlsx[3], time_write_writexl[3])

# print text
my_text <- paste0('\nTime to WRITE dataframe with ',
                  my_formats, ': ',
                  format(results_write, digits = 4),
                  ' seconds', collapse = '')
message(my_text)

my_text <- paste0('\nTime to READ dataframe with ',
                  my_formats, ': ',
                  format(results_read, digits = 4),
                  ' seconds', collapse = '')
message(my_text)

#' 
#' As we can see, even for low-volume data, a data
#' 
#' 
#' ## _RData_ and _rds_ Files
#' 
#' R offers native formats to write objects to a l
#' 
#' The difference between _RData_ and _rds_ is tha
#' 
#' 
#' ### Importing Data
#' 
#' To create a new _.RData_ file, use the `save` f
#' 
## -------------------------------------------------------------------------------------------------------------------
# set a object
my_x <- 1:100

# set name of RData file
my_file <- 'data/temp.RData'

# save it
save(list = c('my_x'), file = my_file)

#' 
#' We can verify the existence of the file with th
#' 
## -------------------------------------------------------------------------------------------------------------------
# check if file exists
file.exists(my_file)

#' 
#' As expected, file `r basename(my_file)` is avai
#' 
#' Importing data from `.rds` files is very simila
#' 
## -------------------------------------------------------------------------------------------------------------------
# set file path
my_file <- 'data/temp.rds'

# load content into workspace
my_y <- read_rds(file = my_file)

#' 
#' Comparing the code between using `.RData` and `
#' 
## As a suggestion, give preference to the _.rds_ format, which is more practical, resulting in cleaner code. The difference in speed between one and the other is minimal. The benefit of importing multiple objects into the same `RData` format file becomes irrelevant when using `list` objects, which can incorporate other objects into its content.

#' 
#' 
#' ### Exporting Data
#' 
#' We can create a new _RData_ file with command `
#' 
## -------------------------------------------------------------------------------------------------------------------
# set vars
my_x <- 1:100
my_y <- 1:100

# write to RData
my_file <- 'data/temp.RData'
save(list = c('my_x', 'my_y'),
     file = my_file)

#' 
#' We can check if the file exists with function `
#' 
## -------------------------------------------------------------------------------------------------------------------
file.exists(my_file)

#' 
#' The result is `TRUE` as expected. 
#' 
#' As for _.rds_ files, we save it with function `
#' 
## -------------------------------------------------------------------------------------------------------------------
# set data and file
my_x <- 1:100
my_file <- 'data/temp.rds'

# save as .rds
write_rds(x = my_x,
          file = my_file)

# read it
my_x2 <- read_rds(file = my_file)

# test equality
print(identical(my_x, my_x2))

#' 
#' Command `identical` tests if both objects are e
#' 
#' 
#' ## _fst_ files 
#' 
#' The [_fst_ format](http://www.fstpackage.org/)^
#' 
#' 
#' ### Importing Data
#' 
#' Using _fst_ file format is similar to the previ
#' 
## -------------------------------------------------------------------------------------------------------------------
library(fst)

# set file location
my_file <- afedR::get_data_file('temp.fst')

# read fst file
my_df <- read_fst(my_file)

# check contents
glimpse(my_df)

#' 
#' As with the other cases, the data from file `r 
#' 
#' 
#' ### Exporting Data
#' 
#' We use function `fst::write_fst` to save datafr
#' 
## -------------------------------------------------------------------------------------------------------------------
library(fst)

# create dataframe
N <- 1000
my_file <- 'data/temp.fst'
my_df <- data.frame(x = runif(N))

# write to fst
write_fst(x = my_df, path = my_file)

#' 
#' 
#' ### Timing the _fst_ format
#' 
#' As a test of the potential of the `fst` format,
#' 
## -------------------------------------------------------------------------------------------------------------------
library(fst)

# set number of rows
N <- 5000000

# create random dfs
my_df <- data.frame(y = seq(1,N),
                    z = rep('a',N))

# set files
my_file_1 <- 'data/temp_rds.rds'
my_file_2 <- 'data/temp_fst.fst'

# test write
time_write_rds <- system.time(write_rds(my_df, my_file_1 ))
time_write_fst <- system.time(write_fst(my_df, my_file_2 ))

# test read
time_read_rds <- system.time(readRDS(my_file_1))
time_read_fst <- system.time(read_fst(my_file_2))

# test file size (MB)
file_size_rds <- file.size(my_file_1)/1000000
file_size_fst <- file.size(my_file_2)/1000000

#' 
#' And now we check the results:
#' 
## ---- results='hold'------------------------------------------------------------------------------------------------
# results
my_formats <- c('.rds', '.fst')
results_read <- c(time_read_rds[3], time_read_fst[3])
results_write<- c(time_write_rds[3], time_write_fst[3])
results_file_size <- c(file_size_rds , file_size_fst)

# print text
my_text <- paste0('\nTime to WRITE dataframe with ',
                  my_formats, ': ',
                  results_write, ' seconds', collapse = '')
message(my_text)

my_text <- paste0('\nTime to READ dataframe with ',
                  my_formats, ': ',
                  results_read, ' seconds', collapse = '')
message(my_text)

my_text <- paste0('\nResulting FILE SIZE for ',
                  my_formats, ': ',
                  results_file_size, ' MBs', collapse = '')
message(my_text)


#' 
#' The difference is very impressive! The `fst` no
#' 
## Due to the use of all the computer's cores, the _fst_ format is highly recommended when working with large data on a powerful computer. Not only will the resulting files be smaller, but the writing and reading process will be considerably faster.

#' 
#' 
#' ## SQLite Files
#' 
#' The use of _.csv_ or _.rds_ files for storing o
#' 
#' This brings us to the topic of **database softw
#' 
#' Before moving to the examples, we need to under
#' 
#' 
#' ### Importing Data
#' 
#' Assuming the existence of an SQLite file in the
#' 
## -------------------------------------------------------------------------------------------------------------------
library(RSQLite)

# set name of SQLITE file
f_sqlite <- afedR::get_data_file('SQLite_db.SQLITE')

# open connection
my_con <- dbConnect(drv = SQLite(), f_sqlite)

# read table
my_df <- dbReadTable(conn = my_con,
                     name = 'MyTable1') # name of table in sqlite

# print with str
glimpse(my_df)

#' 
#' It worked. The `dataframe` from table `MyTable1
#' 
#' Another example of using SQLite is with the act
#' 
## -------------------------------------------------------------------------------------------------------------------
# set sql statement
my_SQL_statement <- "select * from myTable2 where G='A'"

# get query
my_df_A <- dbGetQuery(conn = my_con, 
                      statement = my_SQL_statement)

# disconnect from db
dbDisconnect(my_con)

# print with str
print(str(my_df_A))

#' 
#' It also worked, as expected. 
#' 
#' In this simple example, we can see how easy it 
#' 
#' 
#' ### Exporting Data
#' 
#' As an example of exporting data to an SQLite fi
#' 

#' 
## ---- tidy=FALSE----------------------------------------------------------------------------------------------------
library(RSQLite)

# set number of rows in df
N = 10^6 

# create simulated dataframe
my_large_df_1 <- data.frame(x=runif(N), 
                            G= sample(c('A','B'),
                                      size = N,
                                      replace = TRUE))

my_large_df_2 <- data.frame(x=runif(N), 
                            G = sample(c('A','B'),
                                       size = N,
                                       replace = TRUE))

# set name of SQLITE file
f_sqlite <- 'data/SQLite_db.SQLITE'

# open connection
my_con <- dbConnect(drv = SQLite(), f_sqlite)

# write df to sqlite
dbWriteTable(conn = my_con, name = 'MyTable1', 
             value = my_large_df_1)
dbWriteTable(conn = my_con, name = 'MyTable2', 
             value = my_large_df_2)

# disconnect
dbDisconnect(my_con)

#' 
#' The `TRUE` output of `dbWriteTable` indicates e
#' 
#' 
#' ## Unstructured Data and Other Formats
#' 
#' The previous packages and functions are suffici
#' 
#' Another example is the case of importing data f
#' 
#' 
#' ### Importing Data
#' 
#' You can read the contents of a text file with f
#' 
## -------------------------------------------------------------------------------------------------------------------
# set file to read
my_f <- afedR::get_data_file('pride_and_prejudice.txt')

# read file line by line
my_txt <- read_lines(my_f)

# print 50 characters of first fifteen lines
print(str_sub(string = my_txt[1:15], 
              start = 1, 
              end = 50))

#' 
#' In this example, file `r basename(my_f)` contai
#' 
## -------------------------------------------------------------------------------------------------------------------
# count number of lines
n_lines <- length(my_txt)

# set target text
name_to_search <- 'Bennet'
  
# set function for counting words
fct_count_bennet <- function(str_in, target_text) {
  
  require(stringr)
  

  n_words <- length(str_locate_all(string = str_in, 
                                   pattern = target_text)[[1]])
  
  return(n_words)
}

# use fct for all lines of Pride and Prejudice
n_times <- sum(sapply(X = my_txt, 
                      FUN = fct_count_bennet, 
                      target_text = name_to_search))

# print results
my_msg <- paste0('The number of lines found in the file is ', 
                 n_lines, '.\n',
                 'The word "', name_to_search, '" appears ', 
                 n_times, ' in the book.')
message(my_msg)

#' 
#' In the example, we once again used `sapply`. In
#' 
#' 
#' ### Exporting Data
#' 
#' A typical case of exporting unstructured text i
#' 
## -------------------------------------------------------------------------------------------------------------------
# set file
my_f <- 'data/temp.txt'

# set some string
my_text <- paste0('Today is ', Sys.Date(), '\n', 
                  'Tomorrow is ', Sys.Date()+1)

# save string to file
write_lines(x = my_text, file = my_f, append = FALSE)

#' 
#' In the previous example, we created a simple te
#' 
## -------------------------------------------------------------------------------------------------------------------
print(read_lines(my_f))

#' 
#' As we can see, it worked as expected.
#' 
#' 
#' ## How to Select a Format
#' 
#' The choice of file format is an important topic
#' 
#' - speed of reading and writing operations;
#' - size of the resulting file;
#' - compatibility with other software and operati
#' 
#' Usually, the use of _csv_ files easily satisfie
#' 
#' However, there are cases where the execution sp
#' 
#' 
#' ## Exercises {#exerc-importacao-exportacao}
#' 
## ---- echo=FALSE, results='asis'------------------------------------------------------------------------------------
f_in <- list.files('../02-EOCE-Rmd/Chapter04-Import-Local/', 
                   full.names = TRUE)

compile_eoc_exercises(f_in, type_doc = my_engine)
