#' # Basic Operations in R {#basicoperations}
#' 

#' 
#' Basic operation are the fundamental tasks that 
#' 
#' In this section, we will go through the initial
#' 
#' 
#' ## Working With R
#' 
#' The greatest difficulty new user experiences wh
#' 
#' The "point&click" format of visual and motor in
#' 
#' In the medium and long term, there is a signifi
#' 
#' In using R, the ideal format of work is to merg
#' 
#' Like other software, R allows us to import data
#' 
#' The final product of working with R and RStudio
#' 
#' 
#' ## Objects in R
#' 
#' **In R, everything is an object, and each type 
#' 
#' While we represent data as objects in R, a spec
#' 
#' Each function has its own name and a programmer
#' 
## -------------------------------------------------------------------------------------------------------------------
my_vec <- c(2, 1, 4, 3, 1)
sorted_vec <- sort(x = my_vec, decreasing = TRUE)
print(sorted_vec)

#' 
#' The `sort` function is used with start and end 
#' 
#' 
#' ## International and Local Formats
#' 
#' Before explaining the use of R and RStudio, it 
#' 
#' **decimal:** Following an international notatio
#' 
#' **Latin characters:** Due to its international 
#' 
#' **date format:** Dates in R are structured acco
#' 
#' If you want to learn more about your local form
#' 
## -------------------------------------------------------------------------------------------------------------------
Sys.localeconv()

#' 
#' The output of `Sys.localeconv()` shows how R in
#' 
## Be careful when modifying the format that R interprets the different symbols and notations. As a rule of thumb, if you need to use a specific format, do it separately within the context of the code. Avoid permanent changes as you never know where such formats are being used. That way, you can avoid unpleasant surprises in the future.

#' 
#' 
#' ## Types of Files in R
#' 
#' Like any other programming platform, R has a fi
#' 
#' **Files with extension _.R_**: text files conta
#' 
#' **Files with extension _.RData_ or _.rds_**: fi
#' 
#' **Files with extension _.Rmd_ and _.md_**: file
#' 
#' **Files with extension _.Rproj_**: contain file
#' 
#' 
#' ## Explaining the RStudio Screen
#' 
#' After installing the two programs, R and RStudi
#' 
#' After opening RStudio, the resulting window sho
#' 

#' 
#' Note that RStudio automatically detected the in
#' 
#' As a first exercise, click _file_, _New File_, 
#' 

#' 
## An important suggestion here is to change the color scheme of RStudio to a **dark mode** setting. It is not just an aesthetic issue, but also a strategy for preventing health problems. Since you will be spending a lot of time in front of the computer, it is smart to change the colors of the interface to relieve your eyes of the constant brightness of the screen. That way, you'll be able to work longer, without straining your vision. You can configure the color scheme of RStudio by going to the option _Tools_, _Global Options_ and then _Appearance_. A dark color scheme that I personally like and suggest is _Ambience_.

#' 
#' After the previous steps in RStudio, the result
#' 
#' **Script Editor:** located on the left side and
#' 
#' **R prompt:** on the left side and below the sc
#' 
#' **Environment:** located on the top-right of th
#' 
#' **Panel Packages:** shows the packages installe
#' 
#' As an introductory exercise, let's initialize t
#' 
## -------------------------------------------------------------------------------------------------------------------
# set x
x <- 1

# set y
y <- 'My humble text'

#' 
#' If done correctly, notice that two objects appe
#' 
#' Now, let's show the values of `x` on the screen
#' 
## -------------------------------------------------------------------------------------------------------------------
# print contents of x
print(x)

#' 
#' The `print` function is one of the main functio
#' 
## -------------------------------------------------------------------------------------------------------------------
# print a sequence
print(50:100)

#' 
#' Here, we use the colon symbol in `50:100` to cr
#' 
#' 
#' ## R Packages
#' 
#' One of the greatest benefits of using R is its 
#' 
#' Every function in R belongs to a package. When 
#' 
#' **CRAN is the official repository of R and it i
#' 
#' The suitability of the code to CRAN standards i
#' 
#' The complete list of packages available on CRAN
#' 
#' Another important source for finding packages i
#' 

#' 
#' A popular alternative to CRAN is [Github](https
#' 
#' The most interesting part of this is that the G
#' 
## -------------------------------------------------------------------------------------------------------------------
# get a matrix with available packages
df_cran_pkgs <- available.packages()

# find the number of packages
n_cran_packages <- nrow(df_cran_pkgs)

# print it
print(n_cran_packages)

#' 
#' Currently, `r Sys.time()`, there are `r n_cran_
#' 
#' You can also check the amount of **locally inst
#' 
## -------------------------------------------------------------------------------------------------------------------
# find number of packages currently installed
n_local_packages <- nrow(installed.packages())

# print it
print(n_local_packages)

#' 
#' In this case, the computer in which the book wa
#' 
#' 
#' ### Installing Packages from CRAN
#' 
#' To install a package, simply use the command `i
#' 
## ----eval=FALSE-----------------------------------------------------------------------------------------------------
## # install package readr
## install.packages("readr")

#' 
#' That's it! After executing this simple command,
#' 
#' 
#' ### Installing Packages from Github
#' 
#' To install a package hosted in Github, you must
#' 
## ----eval=FALSE-----------------------------------------------------------------------------------------------------
## # install devtools
## install.packages('devtools')

#' 
#' After that, use the function `devtools::install
#' 
## ----eval=FALSE-----------------------------------------------------------------------------------------------------
## # install ggplot2 from github
## devtools::install_github("hadley/dplyr")

#' 
#' Note that the username of the developer is incl
#' 
## Be aware that **github packages are not moderated**. Anyone can send code there and the content is not independently checked. Never install github packages without some confidence of the author's work. Although unlikely - it never happened to me for example - it is possible that they have malicious code.

#' 
#' 
#' ### Loading Packages
#' 
#' Within a script, use the function `library` to 
#' 
## ----eval=FALSE-----------------------------------------------------------------------------------------------------
## # load package readr
## library(readr)

#' 
#' After running this command, all functions of th
#' 
#' If the package you want to use is not available
#' 
## ---- eval=FALSE----------------------------------------------------------------------------------------------------
## library(unicorn)

#' 

#' 
#' Remember this error message. It will appear eve
#' 
#' Alternatively, if you use a specific package fu
#' 
## -------------------------------------------------------------------------------------------------------------------
# example of using a function without loading package
fortunes::fortune(10)

#' 
#' Here, we use function `fortune` from the packag
#' 
#' Another way of loading a package is by using th
#' 
#' The use of `require` is left for loading up pac
#' 
## ----eval=FALSE-----------------------------------------------------------------------------------------------------
## fct_example <- function(x){
## 
##   require(quantmod)
## 
## 	df <- getSymbols(x, auto.assign = F)
## 	return(df)
## }

#' 
#' In this case, the first time that `fct_example`
#' 
## Be aware that loading a package can cause a **conflict of functions**. For example, there is a function called `filter` in the `dplyr` package and also in the `stats` package. If we load both packages and call the `filter` function within the scope of the code, which one will R use? Well, the **preference is always for the last loaded package**. This is a type of problem that can be very confusing. Fortunately, note that R itself tests for conflicts when loading a package. Try it out: start a new R session and load the `dplyr` package. You will see that a message indicates that there are two conflicts with the `stats` package -- functions `filter` and `lag` -- and four with the `base` package.

## 
## A simple strategy to avoid bugs due to conflict of function is to call a function using the actual package name. For example, if I'm calling `lag` from `dplyr`, I can write the call as `dplyr::lag`. As you can see, the package name is explicit, avoiding any possible conflict.

#' 
#' 
#' ### Upgrading Packages
#' 
#' Over time, it is natural that packages availabl
#' 

#' 
#' The user can also update packages through the p
#' 
## ----eval=FALSE-----------------------------------------------------------------------------------------------------
## # update all installed packages
## update.packages()

#' 
#' The command `update.packages` compares the vers
#' 
## Package versioning is an extremely important topic for keeping your code reproducible. Although it is uncommon to happen, a package update might modify, for the same data, results obtained previously. I have a particularly memorable experience when a scientific article returned from a journal review and, due to the update of one of the R packages, I was unable to reproduce the results presented in the article. In the end everything went well, but the trauma remains.

## 
## One solution to this problem is to freeze the package versions for each project using RStudio's `packrat` tool. In summary, `packrat` makes local copies of the packages used in the project, which have preference over system packages. Thus, if a package is updated in the system, but not in the project, the R code will continue to use the older version and the R code will always run under the same conditions.

#' 
#' 
#' ## Running Scripts from RStudio
#' 
#' Now, let's combine all the previously typed cod
#' 

#' 
#' After pasting all the commands in the editor, s
#' 
#' 
#' ### RStudio shortcuts
#' 
#' In RStudio, there are some predefined and time-
#' 

#' 
#' Another way of executing code is with the short
#' 
#' Next, I highlight these and other RStudio short
#' 
#' control + shift + s
#' : executes (source) the current RStudio file;
#' 
#' control + shift + enter
#' : executes the current file with echo, showing 
#' 
#' control + enter
#' : executes the selected line, showing on-screen
#' 
#' control + shift + b
#' : executes the codes from the beginning of the 
#' 
#' control + shift + e
#' : executes the codes of the lines where the cur
#' 
#' I suggest using these shortcuts from day one, c
#' 
#' If you want to run code in a _.R_ file within a
#' 
#' To run the support _scripts_, just call it with
#' 
## ----eval=FALSE-----------------------------------------------------------------------------------------------------
## # execute import script
## source('01-import-data.R')
## 
## # execute analysis
## source('02-build-tables')

#' 
#' Here, all code in `01-import-data.R` and `02-bu
#' 
#' 
#' ## Testing and Debugging Code
#' 
#' Developing code follows a cycle. At first, you 
#' 
#' When trying to find an error in a preexisting s
#' 

#' 
#' This red circle indicates a flag that will forc
#' 
## ----eval=FALSE-----------------------------------------------------------------------------------------------------
## # set x
## x <- 1
## 
## # set y
## y <- 'My humble text'
## 
## browser()
## 
## # print contents of x
## print(x)

#' 
#' The practical result is the same as using RStud
#' 
#' 
#' ## Creating Simple Objects
#' 
#' One of the most basic and most used commands in
#' 
## -------------------------------------------------------------------------------------------------------------------
# set x
x <- 123

# set my_x, my_y and my_z in one line
my_x <- 1; my_y <- 2; my_z <- 3

#' 
#' We can read this code as _the value 123 is assi
#' 
## Using an arrow symbol `<-` for object definition is specific to R. The reason for this choice was that, at the time of conception of the _S_ language, keyboards had a specific key that directly defined the arrow symbol. This means that the programmer only had to hit one key in the keyboard to set the _assign_ symbol. Modern keyboards, however, are different. If you find it troublesome to type this symbol, you can use a shortcut as well. In _Windows_, the shortcut for the symbol `<-` is _alt_ plus -.

#' 
#' Most programming languages uses a equality symb
#' 
#' The name of the object is important in R. With 
#' 
#' R executes the code looking for objects availab
#' 
## ---- error=TRUE----------------------------------------------------------------------------------------------------
print(z)

#' 
#' The error occurred because the object `z` does 
#' 
#' 
#' ## Creating Vectors
#' 
#' In the previous examples, we created simple obj
#' 
#' When we gather many elements of the same class,
#' 
#' Atomic vectors are created in R using the `c` c
#' 
## -------------------------------------------------------------------------------------------------------------------
# create numeric atomic vector
x <- c(1, 2, 3)

# print it
print(x)

#' 
#' The `c` command works the same way for any othe
#' 
## -------------------------------------------------------------------------------------------------------------------
# create character atomic vector
y <- c('text 1', 'text 2', 'text 3', 'text 4')

# print it
print(y)

#' 
#' The only restriction on the use of the `c` comm
#' 
## -------------------------------------------------------------------------------------------------------------------
# a mixed vector
x <- c(1, 2, '3')

# print result of forced conversion
print(x)

#' 
#' The values of `x` are all of type `character`. 
#' 
## -------------------------------------------------------------------------------------------------------------------
# print class of x
class(x)

#' 
#' 
#' ## Knowing Your Environment and Objects
#' 
#' After using various commands, further developme
#' 

#' 
## ---- eval=FALSE----------------------------------------------------------------------------------------------------
## # set some objects
## x <- 1
## y <- 2
## z <- 3
## 
## # print all objects in the environment
## print(ls())

#' 

#' 
#' Objects `x`, `y` and `z` were created and are a
#' 
#' To display the content of each object, just ent
#' 
## -------------------------------------------------------------------------------------------------------------------
# print objects by their name
x
y
z

#' 
#' Typing the object name on the screen has the sa
#' 
#' In R, all objects belong to a class. As previou
#' 
## -------------------------------------------------------------------------------------------------------------------
# set objects
x <- 1
y <- 'a'
fct_example <- function(){}

# print their classes
print(class(x))
print(class(y))
print(class(fct_example))

#' 
#' Another way to learn more about an object is to
#' 
## -------------------------------------------------------------------------------------------------------------------
# set vec
x <- 1:10
# print the textual representation of a vector
print(str(x))

#' 
#' We find that object `x` is a vector of class `i
#' 
#' 
#' ## Displaying and Formatting Output
#' 
#' You can show the value of an R object on the sc
#' 
#' However, there are other specific functions to 
#' 
#' For example, if we wanted to show the text, `Th
#' 
## -------------------------------------------------------------------------------------------------------------------
# set x
x <- 2

# print customized message
message('The value of x is', x)

#' 
#' You can also customize the screen output using 
#' 
## -------------------------------------------------------------------------------------------------------------------
# set text with break line
my_text <- ' First Line,\n Second line'

# print it
message(my_text)

#' 
#' Note that the use of `print` would not result i
#' 
## -------------------------------------------------------------------------------------------------------------------
print(my_text)

#' 
#' Another example in the use of specific commands
#' 
## -------------------------------------------------------------------------------------------------------------------
# set char with \t
my_text_1 <- 'A and B'
my_text_2 <- '\tA and B'
my_text_3 <- '\t\tA and B'

# print with message()
message(my_text_1)
message(my_text_2)
message(my_text_3)

#' 
#' We’ve only scratched the surface of the possibl
#' 
#' 
#' ### Customizing the Output
#' 
#' Another way to customize text output is by usin
#' 
#' Function `paste` _glues_ a series of character 
#' 
## -------------------------------------------------------------------------------------------------------------------
# set some text objects
my_text_1 <- 'I am a text'
my_text_2 <- 'very beautiful'
my_text_3 <- 'and informative.'

# paste all objects together and print
message(paste(my_text_1, my_text_2, my_text_3))

#' 
#' The previous result is not far from what we did
#' 
## -------------------------------------------------------------------------------------------------------------------
# example of paste0
message(paste0(my_text_1, my_text_2, my_text_3))

#' 
## An alternative to the `message` function is `cat` (_concatenate and print_). It is not uncommon to find code where messages to the user are transmitted with `cat` and not `message`. As a rule of thumb, give preference to `message` which provides a output that is easier to control. For example, if the user wants to silence a function, omitting all outputs from the screen, he/she could just use the command `suppressMessages`.

#' 
#' Another very useful possibility with the `paste
#' 
## -------------------------------------------------------------------------------------------------------------------
# example using the argument sep
message(paste(my_text_1, my_text_2, my_text_3, sep = ', '))

#' 
#' If we had an atomic vector with all elements to
#' 
## -------------------------------------------------------------------------------------------------------------------
# set character object
my_text <-c('I am a text', 'very beautiful', 'and informative.')

# example of using the collapse argument in paste
message(paste(my_text, collapse = ', '))

#' 
#' Another key feature of the `paste` command is t
#' 
## -------------------------------------------------------------------------------------------------------------------
# set size and vector
my_size <- 10
my_vec <- 1:my_size

# define string vector
my_str <- paste0('My value is equal to ', my_vec)

# print it
print(my_str)

#' 
#' Going forward, command `format` is used to form
#' 
## -------------------------------------------------------------------------------------------------------------------
# example of decimal points in R
message(1/3)

#' 
#' If we wanted only two digits on the screen, we 
#' 
## -------------------------------------------------------------------------------------------------------------------
# example of using the format on numerical objects
message(format(1/3, digits=2))

#' 
#' Likewise, if we wanted to use a scientific form
#' 
## -------------------------------------------------------------------------------------------------------------------
# example of using a scientific format
message(format(1/3, scientific=TRUE))

#' 
#' Function `format` has many more options. If you
#' 
#' 
#' ## Finding the Size of Objects
#' 
#' In R, an object size can mean different things 
#' 
#' In R, the size of an object can be checked with
#' 
#' Function `length` is intended for objects with 
#' 
## -------------------------------------------------------------------------------------------------------------------
# create atomic vector
x <- c(2, 3, 3, 4, 2,1)

# get length of x
n <- length(x)

# display message
message('The length of x is ', n)

#' 
#' For objects with more than one dimension, such 
#' 
## -------------------------------------------------------------------------------------------------------------------
# create a matrix
M <- matrix(1:20, nrow = 4, ncol = 5)

# print matrix
print(M)

# calculate size in different ways
my_nrow <- nrow(M)
my_ncol <- ncol(M)
my_n_elements <- length(M)

# display messages
message('The number of lines in M is ', my_nrow)
message('The number of columns in M is ', my_ncol)
message('The number of elements in M is ', my_n_elements)

#' 
#' The `dim` function shows the dimension of the o
#' 
## -------------------------------------------------------------------------------------------------------------------
# get dimension of M
my_dim <- dim(M)

# print it
print(my_dim)

#' 
#' In the case of objects with more than two dimen
#' 
## -------------------------------------------------------------------------------------------------------------------
# create an array with three dimensions
my_array <- array(1:9, dim = c(3, 3, 3))

# print it
print(my_array)

# display its dimensions
print(dim(my_array))

#' 
#' An important note here is that **the use of fun
#' 
## -------------------------------------------------------------------------------------------------------------------
# set text object
my_char <- 'abcde'

# print result of length
print(length(my_char))

#' 
#' This occurred because the `length` function ret
#' 
## -------------------------------------------------------------------------------------------------------------------
# find the number of characters in an character object
print(nchar(my_char))

#' 
#' 
#' ## Selecting Elements from an Atomic Vector
#' 
#' After creating an atomic vector of a class, it 
#' 
#' The selection of _pieces_ of an atomic vector i
#' 
## -------------------------------------------------------------------------------------------------------------------
# set x
my_x <- c(1, 5, 4, 3, 2, 7, 3.5, 4.3)

#' 
#' If we wanted only the third element of `my_x`, 
#' 
## -------------------------------------------------------------------------------------------------------------------
# get the third element of x
elem_x <- my_x[3]

# print it
print(elem_x)

#' 
#' Indexing also works using vectors containing th
#' 
## -------------------------------------------------------------------------------------------------------------------
# set vector with indices
my_idx <-  (length(my_x)-1):length(my_x) 

# get last and penultimate value of my_x
piece_x_1 <- my_x[my_idx]

# print it
print(piece_x_1)

#' 
#' A cautionary note: **a unique property of the R
#' 
## -------------------------------------------------------------------------------------------------------------------
# set object
my_vec <- c(1, 2, 3)

# print non-existing fourth element
print(my_vec[4])

#' 
#' This is important because `NA` elements are con
#' 
## Generally, the occurrence of `NA` (_Not Available_) values suggests a code problem. Always remember that `NA` indicates lack of data and are contagious: anything that interacts with an `NA` value will turn into another `NA`. **You should become suspicious about your code every time that `NA` values are found unexpectedly**. A manual inspection in the length and indexation of vectors may be required.

#' 
#' The use of indices is very useful when you are 
#' 
## -------------------------------------------------------------------------------------------------------------------
# find all values in my_x that is greater than 3
piece_x_2 <- my_x[my_x>3]

# print it
print(piece_x_2)

#' 
#' It is also possible to index elements by more t
#' 
## -------------------------------------------------------------------------------------------------------------------
# find all values of my_x that are greater than 2 and lower then 4
piece_x_3 <- my_x[ (my_x > 2) & (my_x < 4) ]
print(piece_x_3)

#' 
#' Likewise, if we wanted all items that are lower
#' 
## -------------------------------------------------------------------------------------------------------------------
# find all values of my_x that are lower than 3 or higher than 6
piece_x_4 <- my_x[ (my_x < 3) | (my_x > 6) ]

# print it
print(piece_x_4)

#' 
#' Moreover, logic indexing also works with the in
#' 
## -------------------------------------------------------------------------------------------------------------------
# set my_x and my.y
my_x <- c(1, 4, 6, 8, 12)
my_y <- c(-2, -3, 4, 10, 14)

# find all elements of my_x where my.y is higher than 0
my_piece_x <- my_x[my_y > 0 ]

# print it
print(my_piece_x)

#' 
#' Looking more closely at the indexing process, i
#' 
## -------------------------------------------------------------------------------------------------------------------
# create a logical object
my_logical <- my_y > 0

# print it
print(my_logical)

# find its class
class(my_logical)

#' 
#' Logical objects are very useful whenever we are
#' 
#' 
#' ## Removing Objects from the Memory
#' 
#' After creating several variables, the R environ
#' 
#' For example, given an object `x`, we can delete
#' 
## -------------------------------------------------------------------------------------------------------------------
# set x
x <- 1

# remove x
rm('x')

#' 
#' After executing the command `rm('x')`, the valu
#' 
## ---- eval=FALSE----------------------------------------------------------------------------------------------------
## rm(list = ls())

#' 
#' The term `list` in `rm(list = ls())` is a funct
#' 
## Clearing memory in _scripts_ is a controversial topic. Some authors argue that it is better not to clear the memory as this can erase important results. In my opinion, I think it is important to clear the memory at the top of the script, as long as all results are reproducible. When you start a code in a clean state -- no variables or functions -- it becomes easier to understand and solve possible _bugs_.

#' 
#' ## Displaying and Setting the Working Directory
#' 
#' Like other programming platforms, **R always wo
#' 
#' The simplest way of checking the current workin
#' 
## ----eval=FALSE-----------------------------------------------------------------------------------------------------
## # get current dir
## my_dir <- getwd()
## 
## # display it
## print(my_dir)

#' 
## ----echo=FALSE-----------------------------------------------------------------------------------------------------
message("C:/Dropbox/06-My Books/afedR-ed2/Book Content")

#' 
#' The result of the previous code shows the folde
#' 
#' The change of the working directory is performe
#' 
## ----eval=FALSE-----------------------------------------------------------------------------------------------------
## # set where to change directory
## my_d <- 'C:/My Research/'
## 
## # change it
## setwd(my_d)

#' 
#' After changing the directory, importing and sav
#' 
#' As for simple cases such as the above, remember
#' 
#' 1) Write command `setwd('')` in a script as it 
#'   
#' 2) Place your cursor between the `'` symbols;
#'    
#' 3) Press the _tab_ key.
#' 
#' Now you'll be able to see your folders in a sma
#' 
#' Another, more modern, way of setting the direct
#' 
## ---- eval=FALSE----------------------------------------------------------------------------------------------------
## my_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
## setwd(my_path)

#' 
#' This way, the script will change the directory 
#' 
#' Once you are working on the same path as the sc
#' 
## ----eval=FALSE-----------------------------------------------------------------------------------------------------
## # change to subfolder
## setwd('data')

#' 
#' Another possibility is to go to a previous leve
#' 
## ----eval=FALSE-----------------------------------------------------------------------------------------------------
## # change to the previous level
## setwd('..')

#' 
#' So, if you are working in directory `C:/My Rese
#' 
#' 
#' ## Canceling Code Execution
#' 
#' Whenever R is running some code, a visual cue i
#' 
#' To try it out, run the next chunk of code in RS
#' 
## ---- tidy=FALSE, eval=FALSE----------------------------------------------------------------------------------------
## for (i in 1:100) {
##   message'\nRunning code (please make it stop by hitting esc!)')
##   Sys.sleep(1)
## }

#' 
#' In the previous code, we used a `for` loop and 
#' 
## Another very useful trick for defining working directories in R is to use the `~` symbol. The tilda defines the "Documents" folder in _Windows_, which is unique for each user. Therefore, by running `setwd('~')`, you will direct R to a folder that is easily accessible.

#' 
#' 
#' ## Code Comments
#' 
#' In R, comments are set using the hashtag symbol
#' 
## -------------------------------------------------------------------------------------------------------------------
# this is a comment (R will not parse it)
# this is another comment (R will again not parse it)

x <- 'abc' # this is an inline comment

#' 
#' Comments are an effective way to communicate an
#' 
## ---- eval=FALSE----------------------------------------------------------------------------------------------------
## # read CSV file
## df <- read.csv('data/data_file.csv')

#' 
#' As you can see, it is quite obvious from the li
#' 
## ---- eval=FALSE, tidy=FALSE----------------------------------------------------------------------------------------
## # Script for reproducing the results of JOHN (2019)
## # Author: Mr data analyst (dontspamme@emailprovider.com)
## # Last script update: 2020-01-10
## #
## # File downloaded from www.site.com/data-files/data_file.csv
## # The description of the data goes here
## # Last file update: 2020-01-10
## 
## df <- read.csv('data/data_file.csv')

#' 
#' So, by reading the comments, the user will know
#' 
#' Another productive use of comments is to set se
#' 
## ---- eval=FALSE, tidy=FALSE----------------------------------------------------------------------------------------
## # Script for reproducing the results of JOHN (2019)
## # Author: Mr data analyst (dontspamme@emailprovider.com)
## # Last script update: 2020-01-10
## #
## # File downloaded from www.site.com/data-files/data_file.csv
## # The description of the data goes here
## # Last file update: 2020-01-10
## 
## # Clean data -------------------------
## # - remove outliers
## # - remove unnecessary columns
## 
## # Create descriptive tables ----------
## 
## 
## # Estimate models --------------------
## 
## 
## # Report results ---------------------

#' 
#' The use of a long line of dashes (-) at each se
#' 
## When you start to share code with other people, you'll soon realize that comments are essential and expected. They help transmit information that is not available from the code. This is one way of a discerning  novice from experienced programmers. The later is always very communicative in its comments (sometimes too much!). A note here, throughout the book you'll see that the code comments are, most of the time, a bit obvious. This was intentional as clear and direct messages are important for new users, which is part of the audience of this book.

#' 
#' 
#' ## Looking for Help
#' 
#' A common task in the use of R is to seek help. 
#' 
#' You can get help by using the _help_ panel in R
#' 
#' In R, the help screen of a function is the same
#' 

#' 
#' If we are looking for help for a given text and
#' 
#' As a suggestion, the easiest and most direct wa
#' 
#' Another very important source of help is the In
#' 
## Whenever you ask for help on the internet, always try to 1) describe your problem clearly and 2) add a reproducible code of your problem. Thus, the reader can easily verify what is happening by running the example on his computer. I have no doubt that if you respect both rules, a charitable person will soon help you with your problem.

#' 
#' 
#' ## Using Code Completion with _tab_ {#autocompl
#' 
#' A very useful feature of RStudio is _code compl
#' 

#' 
#' The autocomplete feature is self-aware and will
#' 

#' 
#' Note that a description of the package or objec
#' 
#' The use of this tool becomes even more benefici
#' 
#' As mentioned in the previous section, you can a
#' 

#' 
#' The use of autocomplete is also possible for fi
#' 

#' 
#' Likewise, you can also search for a function wi
#' 

#' 
#' Summing up, using code completion will make you
#' 
## _Autocomplete_ is one of the most important tools of RStudio, helping users to find object names, locations on the hard disk, packages and functions. Get used to using the _tab_ key and, soon enough, you'll see how much the _autocomplete_ tool can help you write code quickly, and without typos.

#' 
#' 
#' ## Interacting with Files and the Operating Sys
#' 
#' As you are learning R, soon enough you'll find 
#' 
#' 
#' ### Listing Files and Folders
#' 
#' To list files from your computer, use function 
#' 
## -------------------------------------------------------------------------------------------------------------------
# list files in data folder
my_files <- list.files(path = "data", full.names = TRUE)
print(my_files)

#' 
#' There are several files with different extensio
#' 
## ----eval=FALSE-----------------------------------------------------------------------------------------------------
## # list all files for all subfolders (IT MAY TAKE SOME TIME...)
## list.files(path = getwd(), recursive = T, full.names = TRUE)

#' 
#' The previous command will list all files in the
#' 
#' To list folders (directories) on your computer,
#' 
## -------------------------------------------------------------------------------------------------------------------
# store names of directories
my_dirs <- list.dirs(recursive = F)

# print it
print(my_dirs)

#' 
#' The command `list.dirs(recursive = F)` listed a
#' 
## -------------------------------------------------------------------------------------------------------------------
# list all files with the extension .Rmd
list.files(pattern = "*.Rmd")

#' 
#' The files presented above contain all the conte
#' 
#' 
#' ### Deleting Files and Directories
#' 
#' You can also use an R session to delete files a
#' 
#' You can delete files with command `file.remove`
#' 
## -------------------------------------------------------------------------------------------------------------------
# create temporary file
my_file <- 'data/tempfile.csv'
write.csv(x = data.frame(x=1:10),
          file = my_file)

# delete it
file.remove(my_file)

#' 
#' Remember that you must have permission from you
#' 
#' To delete directories and all their elements, w
#' 
## ---- echo=FALSE----------------------------------------------------------------------------------------------------
if (dir.exists('temp')) unlink('temp')

#' 
## -------------------------------------------------------------------------------------------------------------------
# create temp dir
dir.create('temp')

# create a file inside of temp
my_file <- 'temp/tempfile.csv'
write.csv(x = data.frame(x=1:10),
          file = my_file)

unlink(x = 'temp', recursive = TRUE)

#' 
#' Notice that, unlike `file.remove`, function `un
#' 
## -------------------------------------------------------------------------------------------------------------------
dir.exists('temp')

#' 
#' As expected, the directory was not found.
#' 
#' 
#' ### Downloading Files from the Internet
#' 
#' We can also use R to download files from the In
#' 
## -------------------------------------------------------------------------------------------------------------------
# set link
link_dl <- 'go.microsoft.com/fwlink/?LinkID=521962'
local_file <- 'data/temp_file.xlsx' # name of local file

download.file(url = link_dl,
              destfile = local_file)

#' 
#' Using `download.file` is quite handy when you a
#' 
#' One trick worth knowing is that you can also do
#' 
## Needless to say, **be very careful** with commands `file.remove` and `unlink`, especially when using recursion (`recursive = TRUE`). One simple mistake and important parts of your hard drive can be erased, leaving your computer inoperable. Be aware  that R **permanently deletes** files and do move them to the trash folder. Therefore, when deleting directories with `unlink`, you will be unable to recover the files easily.

#' 
#' 
#' ### Using Temporary Files and Directories
#' 
#' An interesting aspect of R is that every new se
#' 
## ---- eval=FALSE----------------------------------------------------------------------------------------------------
## windows_tempdir <- tempdir()
## print(windows_tempdir)

#' 
## ---- echo = FALSE--------------------------------------------------------------------------------------------------
windows_tempdir <- "C:\\Users\\NAME\\AppData\\Local\\Temp\\Rtmp8E"
message(windows_tempdir)

#' 
#' The name of the temporary directory, in this ca
#' 
#' The same dynamic is found for file names. If yo
#' 
## ---- eval=FALSE----------------------------------------------------------------------------------------------------
## windows_tempfile <- tempfile(pattern = 'temp_',
##                              fileext = '.xlsx')
## message(windows_tempfile)

#' 
## ---- echo = FALSE--------------------------------------------------------------------------------------------------
message('C:\\Users\\NAME\\AppData\\Local\\Temp\\Rtmp8E\\temp_4365730565.xlsx')

#' 
#' You can also set its extension and name:
#' 
## ---- eval=FALSE----------------------------------------------------------------------------------------------------
## windows_tempfile <- tempfile(pattern = 'temp_',
##                              fileext = '.csv')
## message(windows_tempfile)

#' 

#' 
#' As a practical case of using temporary files an
#' 
## -------------------------------------------------------------------------------------------------------------------
# set link
link_dl <- 'go.microsoft.com/fwlink/?LinkID=521962'
local_file <- tempfile(fileext = '.xlsx', tmpdir = tempdir())

download.file(url = link_dl,
              destfile = local_file)

df_msft <- readxl::read_excel(local_file)

print(head(df_msft))

#' 
#' The example Excel file contains the sales repor
#' 
#' By using `tempfile`, we do not need to delete (
#' 
#' 
#' ## Exercises {#exercises-basic-exercises}
#' 
## ---- echo=FALSE, results='asis'------------------------------------------------------------------------------------
f_in <- list.files('../02-EOCE-Rmd/Chapter02-Basic-Operations/', 
                   full.names = TRUE)

compile_eoc_exercises(f_in, type_doc = my_engine)
