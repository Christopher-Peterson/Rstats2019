# This is the r_code for week 0
# Anything after a pound/hash/number sign is a comment

#### Installing some packages #### 
# First, there are a few packages that need to be installed 
# I'll explain what they are and how they work in a minute,
# but this can take a minute so let's get it started now

# Move your cursor to the next line and press Ctrl+Enter
install.packages("tidyverse") # It may be Cmd+Enter for Mac users
# This sends the line to the Console window and executes it
# If you're asked to pick a server/mirror, go with the first option
# (O-cloud)
install.packages("cowplot") # Install this package too
install.packages("ggforce") # Also this one
# If you already have these installed, make sure they're the 
# latest versions.  If not, then please run the above code to
# update them

# The library() function loads a package into the memory; 
# you need to do this each time you start an R session
library(tidyverse)

#### Some R Basics

# First, R can be used as a fancy calculator
  # To run these lines, move the cursor to them and
  # hit Ctrl + Enter (Cmd + Enter on a Mac)

(4^2 + 8)/10
log(5) + 12 # R has a bunch of built-in funcitons, like log,
sqrt(abs(-20)) # sqrt() [Square root], and abs() [absolute value]

# R works naturally with vectors of numbers (or text).

1:10 # Create a sequence of numbers
c(1, 4, 9, 12, 98.7) # use c() to make a vector
c("A", "B", "C", "D") # Here's a character vector

#  Most operations work with vectors
(1:10) + 2
(1:5) + c(10, 20, 30, 40, 50)

# Vectors can only be of one type; 
  # mixing numbers & text will convert them all to text
c("I have been at UT for ", 5, "Years")

# You can save values & objects by creating variables.

# You can use either <- or = to assign a variable
first_ten <- 1:10
second_ten = 11:20 

# Run the variable's name to see it's value (this is callled printing)
first_ten
second_ten

# You can use variables just like you would use their values
first_ten + 1 
first_ten + second_ten

# Note that variable names are case-sensitive
first_Ten # doesn't work

#### Reading data into R ####

# You can read csv files into R with read_csv(), from the readr package
  # readr is part of the tidyverse package
  # Note that there's also a read.csv() function; 
    # It can do some weird & unexpected stuff, so I recommed avoiding it

lizards <- read_csv("data/anoles.csv") # This loads in the dataset

lizards # This is a tidy data frame;
# Each column is a separate variable; each row is an observation
glimpse(lizards)
View(lizards)

# Each column of the data frame is a vector of the same length.
# We can pull our columns and work with them directly:

# Let's extract the color column
lizards$Color_morph
lizards[["Color_morph"]] 
pull(lizards, Color_morph) # requires dplyr package, which is in the tidyverse
# Note that some of these require quotes, some of them don't

#### Functions ####

# Pretty much everything that isn't data is a function.  
  # Some of the examples we've used include `log`, `abs`, `read_csv`, and `mean`. 
  # Most functions have arguments, which tell the function what to work with.  
  # For example: 

mean(x = 1:5) # mean of 1 through 5
sd(x = lizards$Mass) # standard deviation of lizard mass

# Functions can have multiple arguments; 
  # for example `log` has the arguments `x` and `base`. 
  # Arguments can be matched by name or by their position. 
  # Some arguments have default values that are used if the 
    # argument isn't provided.
    # 
log(x = 1:5) # argument is matched by name; base uses it's default value
log(1:5, base = 10) # specifies a base; this overrides the default
log(1:5, 10) # same as above, but matched by position

## Getting Help:

# R has a built-in help system to look up functions, 
  # their arguments, and what they do:

?read_csv
?mean
?log

# If you don't know the name of a function, you can search the 
  # help system for key words like this:
??read

