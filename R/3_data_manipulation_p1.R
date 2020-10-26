# Week 2: Data manipulation

## This week, we're talking about dplyr
## It corresponds with chapter 5 of R for Data Science 
  ## (http://r4ds.had.co.nz/transform.html)
library(tidyverse)
library(cowplot)

theme_set(theme_cowplot())

# Get the data; the quakes dataset already exists in R
lizards <- read_csv("data/anoles.csv")

#### mutate(): add a new column to a data frame ####

# Create a new column Total_length
# The resulting column is the same length as the existing ones
mutate(.data = lizards, Total_length = SVL + Tail) %>% View()

# Note that you don't have to explicitly name the .data argument; just put it first
mutate(lizards, Total_length = SVL + Tail)
  
# You can mutate multiple variables in one command
mutate(lizards, 
       total_length = SVL + Tail,
       rel_limb = Limb/SVL,
       log_total_length = log(total_length)) # You can refer to previously created columns 

# Note that the above commands create a new dataframe with added variables, but the result hasn't 
# been saved
lizards_full = 
  mutate(lizards, 
         total_length = SVL + Tail,
         rel_limb = Limb/SVL,
         log_total_length = log(total_length)) 
View(lizards_full)
  
#### The Pipe ( %>% ) ####

# The pipe strings together functions.
  # It takes the result of one function and makes it 
    # the first argument of the next
  # b(a(x)) can be re-written as x %>% a() %>% b() 
# Think of it as "then"
  # Take x, then do a, then do b
  # This makes the order you write your code the same as the order it's executed.
# Almost all of the functions in the tidyverse take data 
  # as the first argument, so the pipe can be very powerful

# These are the same:  
mutate(lizards, Total_length = SVL + Tail)
lizards %>% mutate(Total_length = SVL + Tail)

# Example: take the lizard data set, then calculate the total length, 
  # then plot how it varies among sites
lizards %>% 
  mutate(Total_length = SVL + Tail) %>% 
  ggplot(aes(x = Site, y = Total_length)) + 
  geom_boxplot()

# RStudio shortcut: Ctrl + Shift + M (Cmd + Shift + M on Mac)

# Some other useful mutate commands
lizards %>% mutate(intercept = 1) # creates a fixed column
lizards %>% mutate(row_number = 1:n()) # creates row numbers

#### Exercises: 
# 1. Add a column to the lizards dataset that gives the lizard's height
  # relative to the maximum height.  
  # Hint: max(x) shows the largest value of x. 


# 2. Make a plot of relative limb length (Limb/SVL) vs. 
  # perch circumference (Diameter * pi); 
  # define the new column as perch_circum
lizards %>% 
  # your code here %>% 
  ggplot(aes(x = rel_limb, y = perch_circum)) +
  geom_point()
  

#### filter(): returns subsets of a data frame ####

# Let's define large lizards:
lizards %>%
  mutate( large = SVL > 60) %>% 
  View()
# This is a logical (TRUE/FALSE) column;

# What if we wanted to subset by the large lizards?
lizards %>%
  mutate(large = SVL > 60) %>% 
  filter(large) %>% 
  View()
# selects only the rows where large == TRUE

# Alternatively, we don't have to define large:
lizards %>% filter(SVL > 60)
# A filter command returns every row where the logical statement is TRUE

# Logical vectors are created from conditional statements:
# x == y; TRUE if x equals y (This is not the same as x = y)!
# x != y; TRUE if x does not equal y
# x > y; x >= y; 
# x < y; x <= y; 
# between(x, y, z); TRUE if x >= y AND x <= z
# x %in% y; TRUE if x is an element in y
# is.na(x); TRUE if x is a missing value (NA)

# Combining and modifying logical values:
# x & y  - returns TRUE if both x and y are TRUE
# x | y - returns TRUE if either x or y are TRUE
# !x - returns the opposite of x
  # This is useful to combine with other logical functions
  # filter(data, !is.na(x))  will remove all rows where x is a missing value

# What's the SVL-Height relationship for Blue lizards in
#  sites A through E?
lizards %>% # You can filter multiple statements;
  filter(Color_morph == "Blue", # the result is the case where all conditions
         Site %in% c("A", "B", "C", "D", "E")) %>% # are TRUE
  ggplot(aes(x = SVL, y = Height, color = Site)) + geom_point()

#### Exercises:
#### 
# 3. Print a dataframe that shows only the lizards higher than 150 cm. Working off your code, 
# count how many there were.


# 4. How many lizards perching on trees or shrubs aren't brown? Visualize the height to diameter 
# relationship between them.


#### select(): keep or remove columns ####

lizards %>% select(Site, Color_morph, SVL) # keeps the columns Site, Color_morph, SVL

# Three ways to select variables:
# By name (which we just did)

# By position
lizards %>% select( 1, 2, 7) 
lizards %>% select(1:4) # this is more useful for ranges of values

# From a character vector
keep_cols = c("Site", "Color_morph", "SVL")
lizards %>% select(keep_cols)

# Note that if you want to use a variable that has column names saved
# as a character vector, you'll need to use a helper function 
# all_of() to tell select() that you want to look for the contents 
# of the variable, not the name of the variable:

select_vars = c("Site", "Color_morph", "SVL")
lizards %>% 
  select(all_of(select_vars)) %>% # without all_of, it would try to look for a column called "select_vars"
  View()

# Use negative signs to REMOVE columns
lizards %>% select(-Color_morph, -Limb)
lizards %>% select(-(1:5))

# Selecting by column properties:
# You can use the where() helper function to select columns
# based on their characteristics.  
# For example, the is.numeric() function returns TRUE
# if its argument is a number; you can use it to select all
# numeric columns.

lizards %>% 
  select(where(is.numeric))
# Returns only numeric columns

# You could do the same for text or logical vectors with
  # `is.character` or `is.logical`, respectively.

# There's a lot more you can do with this if you want to get fancy;
# the documentation is available at 
?tidyselect::language

#### arrange(): Sorts your columns ####

lizards %>% arrange(Mass) # Sorts from lowest to highest
lizards %>% arrange(desc(Mass)) # use desc() to sort from highest to lowest

  # Categorical variables:
lizards %>% arrange(Site) # Note how there's some ties
lizards %>% arrange(Site, Color_morph, SVL) # You can add extra variables to break ties

  # This can be useful for creating rankings
lizards %>% 
  arrange(desc(SVL)) %>% 
  mutate(size_rank = 1:n()) %>%
  View()

#### summarize(): create summary statistics ####

# summarize (or summarise) is like mutate, but it produces 
  # columns that are shorter than the input
lizards %>% summarize(mean_SVL = mean(SVL), 
                      sd_SVL = sd(SVL),
                      med_SVL = median(SVL), 
                      count = n())
# Summarize takes in a bunch of rows and returns one.  
  # You can use any function that takes in a vector and 
  # returns a single value.
# One useful trick for getting proportions:
  # The mean of a logical statement is the proportion that's TRUE
lizards %>% summarize(prop_tree = mean(Perch_type == "Tree"), 
                      prop_shrub = mean(Perch_type == "Shrub"),
                      prop_building = mean(Perch_type == "Building"),
                      prop_other = 1 - (prop_tree + prop_shrub + prop_building))


## Summarize across multiple columns
# You can use the across() helper to apply the same summary
  # function to multiple rows.

lizards %>% 
  summarize(
    across(.cols = c(SVL, Tail), # Summarize at SVL & Tail columns
           .fns = mean) # With the mean function
  ) %>% View()

# The .cols argument works just like select(), 
  # so it it takes the same helper functions
# The .fns argument can take a named vector or list of functions, 
  # and will apply all of them

lizards %>% 
  summarize(
    across(.cols = where(is.numeric), # apply to all numeric functions
           .fns = c(Mean = mean, StDev = sd)) # named vector (Mean and StDev)
  ) # Note the names of the resulting table

# This applies the functions `mean()` and `sd()` to all numeric columns; 
# The results have the names `"Mean"` and `"StDev"`
#  that we gave each function applied to the end of the column.
 
#### group_by(): apply functions separately to each group ####

# Summarize (and other functions) become much powerful when applied to group_data

lizards %>% group_by(Site) # This doesn't seem to do much 

lizards %>% group_by(Site) %>% 
  summarize(mean_SVL = mean(SVL), 
            count = n()) 
# But that's interesting...

# You can group by multiple factors:
lizards %>% group_by(Site, Color_morph) %>% 
  summarize(mean_SVL = mean(SVL), 
            count = n()) %>% 
  ungroup() 

# What's the relative height of each lizard within each site?
lizards %>% group_by(Site) %>% 
  mutate(rel_height = Height/max(Height)) %>% 
  # Note here that max(Height) was applied within sites
  ungroup %>% 
  ggplot(aes(x = Site, y = rel_height)) + geom_sina() #(width = .2)

#### Exercises:

# 5. Visualize the relationship between the maximum height at a site and the average limb length
lizards %>% 
  # Your code here %>% 
  ggplot(aes(x = max_height, y = mean_limb)) + 
  geom_smooth(method = "lm") + geom_point() 
  
# Advanced question: For each site, what's the mean limb length 
  # of the five largest individuals (by SVL)? 
  # What proportion of these indiviuals is blue? 






