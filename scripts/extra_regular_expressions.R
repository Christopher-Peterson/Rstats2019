#This code is for Skill Development course 2018

library(tidyverse)
library(stringr)

# String is the object that holds character data type

fruits <- c("apples", "pineapples", "bananas", "oranges", "blueberries", "blackberries")

#Join
# Join all elements of vector by "_"
str_c(fruits, collapse = "_")

# Join two vectors of same length by ":"
colours <- c("green", "yellow", "yellow", "orange", "blue", "black")
str_c(fruits,colours, sep = ":")

#Or by one vector with one character object
str_c(fruits,"Fruit", sep = ":")

#Split or cut
# Or cut each element of a character vector with a fixed length
# Start at 1 and end at 4
str_sub(fruits,1,4)

str_split(str_c(fruits, collapse = "_"), "_") #this return a list

str_split_fixed(str_c(fruits, collapse = "_"), "_", length(fruits)) #return a character matrix

#Subset
# subset with define match
str_subset(fruits, "apple")

# subset with group/pattern
# this will match anything with "b" or "o"
str_subset(fruits, "[bo]")

#match anything but newline
str_subset(fruits, ".")

#Another way of grouping
str_subset(colours, "bl(u|a)")

# Search and replace
str_replace(fruits, "[bo]", "*")

#Regular expresion
#  . = matches any character except a newline
#  \n = newline
# \t = tab
# \ = escape
#  \d= any digit
#\s = any white space, this is very powerful which can replace tab, newline
#^ =start 
#$ =end 

## Escape \
# If \ is used as an escape character in regular expressions, how do you match a literal \? 
# Well you need to escape it, creating the regular expression \\. To create that regular 
# expression, you need to use a string, which also needs to escape \. That means to match 
# a literal \ you need to write "\\\\" - you need four backslashes to match one!

#Pattern match
exampleString <- "This\n line  \t is \tbadly\n\t\tformated"
#> [1] "Some  \t badly\n\t\tspaced \f text"
str_replace_all(exampleString, "\\s+", " ")


# Serach and replace with mapping
str_replace_all(fruits, c("b"="c","o"="p"))


#Complex pattern match

newString<-c("AB234 THG22457")

pattern<-"([A-Z]{2})(\\d{3})[\\s]([A-Z]{3})(\\d{5})"

str_match(newString,pattern )


#  You can also create your own character classes using []:
#   
# [abc]: matches a, b, or c.
#  [a-z]: matches every character between a and z (in Unicode code point order).
#  [^abc]: matches anything except a, b, or c.
#  [\^\-]: matches ^ or -.
# 
#  There are a number of pre-built classes that you can use inside []:
#   
#    [:punct:]: punctuation.
#  [:alpha:]: letters.
#  [:lower:]: lowercase letters.
#  [:upper:]: upperclass letters.
#  [:digit:]: digits.
#  [:xdigit:]: hex digits.
#  [:alnum:]: letters and numbers.
#  [:cntrl:]: control characters.
#  [:graph:]: letters, numbers, and punctuation.
#  [:print:]: letters, numbers, punctuation, and whitespace.
#  [:space:]: space characters (basically equivalent to \s).
#  [:blank:]: space and tab.
# 

library(glue)

testString<-c("AB234 THG22457", "GH256 OPL87432", "GF900 DSE79833","VH007 DLP89654")

mat<-as.data.frame(str_match(testString,pattern )) #  glue_data takes a data frame or tibble

colnames(mat)<-c("Full","F1","F2","F3","F4")


mat %>% glue_data("The 3rd field of {Full} string is {F3}")

