library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())
############ Vectors and Lists (Refresher) #############

# Vectors in R

# most objects in R are vectors of a sort
c(2.4, 5,4, 2.3) # a numeric/double vector
c(TRUE, TRUE, FALSE) # a logical vector
c("A", "b", "cat") # a character vector
c(1L, 4L, -3L) # an integer vector; note the L's at the end tell R not to use decimals

# These are all atomic vectors, since each element is of the same type
  # You can't use c() to combine different types of vectors
c("A", TRUE, -2.3) # These are all coerced into characters

# Non-atomic vectors in R are called lists

# lists are the most general data structure in R.
# They can contain anything (even other lists)

test_list = list(1:5, 
  its_a =list(c("bird", "plane", "superman"), status = c(FALSE, FALSE, TRUE)), 
  x~y+z, 
  mean_function = mean)
test_list

# lists elements can be accessed by their position or their names (if they have them)
test_list[[2]] # Note the double brackets
test_list$its_a 
test_list[["its_a"]]
# You can access SUBSETS of a list with single brackets
test_list[2]  # compare with test_list[[2]]
test_list[2:3] # (Pull up pepper jar analogy)

# This nests
test_list[[2]][[2]]
test_list$its_a$status
test_list$its_a[[2]]

######## List columns and nested Data Frames ######
# A list is a type of vector, so you can make it a column in a data frame

nested_example = tibble(A = 1:4, B = c("A","B","A","B")) %>% 
  mutate(C = list(
    c(1L,2L), c(1:4), c(2L), c(85:90)))
nested_example
nested_example$C
unnested_example = nested_example %>% unnest(C) # expand the list-column C

# You can also compress columns into nested data frames
re_nested = unnested_example %>%  
  nest(nested_data = c(B, C))
re_nested
re_nested$nested_data

# If you have a grouped data frame, nest will automatically nest by group
unnested_example %>% group_by(B) %>% nest()
# Except for grouping variables

lizards = read_csv("data/anoles.csv")
lizards %>% group_by(Site) %>% nest()

# Let's put these aside for a minute and look at something else

########### Map Function (purrr package) ################
# Large problems in data analysis are often made of a bunch of small problems
  # Frequently, these smaller problems are similar and repetitive
  # Strategy:   
    # 1. Identify what varies among the repetitions
    # 2. Figure out how to do a single small problem
    # 3. Use a map() function to apply your solution to the small problem to all the variants

# Simple example:
# calculate the mean of each element in this list
x_list = list(a = 1:10, b = exp(-3:3), c = c(TRUE,FALSE,FALSE,TRUE))
x_list

# Brute-force method
list(
  mean(x_list[[1]]),
  mean(x_list[[2]]),
  mean(x_list[[3]])  )
# What if we had 500 objects in x_list?  
# It's clear that we want to use the function 'mean' on every element of x_list

map(.x = x_list, # A vector or list
    .f = mean) # a function to apply to each element of the vector or list

# The map function: argument 1 is a vector or list; 
# argument 2 is the function to be applied to each element of the list
# Note that this is mostly the same as the lapply() function, but with some added perks

# median and quartiles for each list element
map(x_list, .f = quantile, # Named arguments after this are passed to the function
    probs = c(.25, .5, .75)) # probs is an argument passed to quantile() that doesn't vary

# You can specify the return type if you know it ahead of time
map_dbl(x_list, mean) # returns a numeric (double) vector
map_int(x_list, length) # returns an integer vector
# In general you should make sure that the declared output type works with the function
map_chr(x_list, mean) # This works because anything can be turned into a character
map_int(x_list, mean) # But you can't convert a numeric value to an integer w/o losing information

# Also map_chr(), map_lgl(), 
# Note that these only work for one-to-one functions:
  # one output (int, char, etc) for each input (.x)
  # when you have more than one output length 
  # (or aren't sure), use map()

# There's also  map_dfr(), map_dfc() 
  # (these create data frames by binding the output into 
  # rows (dfr) or columns (dfc)
# the default map() returns a list

# map also works with pipes
x_list %>% map_dbl(mean)

# Anonymous functions: 
# You can use map with newly defined functions

# For example:
x_list %>% map_dbl(~exp(mean(log(.x))))
  # The tilde tells map that you're creating a new function 
    # (note: this doesn't work outside of map and some other tidyverse functions)
    # The function's first argument will be called .x
    # this is called "lambda syntax"
# This is the same as:
geometric_mean = function(.x) {
  exp(mean(log(.x)))
}
map_dbl(x_list, geometric_mean)

# Also equivalent:
x_list %>% map_dbl(function(.x) {
  exp(mean(log(.x)))
})
# Using the lambda syntax is just a bit shorter

1:26 %>%  map_dfr(~tibble(position = .x, 
                          caps = LETTERS[.x],
                          lower = letters[.x])) 
# Note how it returns a row-bound data frame

########## Exercise: ##############
# There are 19 data files in data/sites
file_names = dir("data/sites", full.names=TRUE)

# use read_csv and map to read all of them in; the output should be a data frame

#Single use case: 

read_csv(file_names[1])

#### Mapping with 2 variables #####

# If you have two variables you'd like to iterate over, use map2()
# The two need to be the same length, of course

map2_dbl(.x = lizards$SVL, .y = lizards$Tail, 
         .f = ~.x + .y) # Note that anonymous functions w/ two variables use
                        # .x and .y
# There's also imap
# It takes one argument, and acts like 
# map2(.x, .y = names(.x))

x_list %>% imap_chr(~paste("The mean of", .y, "is", mean(.x) %>% round(2)))

### Exercise:   #####
### modify the above read_csv code to include a column with the filename 
  ### or site name as an extra column 
  ### (hint: use a mutate command inside the map function)

########### pmap: mapping with multiple variables)

# The pmap function takes a data frame or a list of variables
  #  all elements of the list must have the same length; 
# It applies uses the list elements as function arguments
  # list elements with names are matched to the function argument with the same name
  # the rest are matched by position.

# Example: We want to generate some random numbers
# the function: runif(n, min, max) generates n random numbers between min and max
args(runif)

runif(n = 5, min = -1, max = 1)

# We want to create a data frame with columns that share the name of the function args
random_num_pars = tibble(
          n = c(100, 25, 75, 50), 
          min = c(-40, 0, 135, 15), 
          max = min + c(100, 22, 68, 40))   
random_num_pars
random_nums = random_num_pars %>% pmap(runif)  

random_nums

random_nums %>% 
  flatten_dbl # this squashes the list into a numeric vector 

# You can apply a map function to the results of a map function
random_nums %>% 
  map_dbl(mean) 

# What if we want to associate our random numbers with the matrix that created them?
random_num_pars %>% 
  mutate(random_nums = pmap(random_num_pars, runif))
# We can create a list column!

# Let's say we want to take the quantiles of each random number set,

quantile_list <-  c(.025, .1, .25, .5, .75, .9, .975)

rng_quantiles <- random_num_pars %>% 
  mutate(random_nums = pmap(random_num_pars, runif)) %>% 
  mutate(quantile_vals = random_nums %>% 
           map(quantile, probs = quantile_list),
         quantile_levels = map(quantile_vals, names))
  
rng_quantiles %>% unnest(quantile_vals, quantile_levels) # note this automatically drops the random nums

rng_quantiles %>% unnest(quantile_vals, quantile_levels) %>% 
  spread(key = quantile_levels, value = quantile_vals)

###### Some more complicated functions

# Last week, we looked at plotting the Limb:Height relationship at a single site

lizards %>% 
  filter(Site == "A") %>%
    ggplot(aes(x = Limb, y = Height)) +
    facet_wrap(~Site, scale = "free")+
    geom_smooth(method = "lm") + geom_point()

# What if we wanted to make separate plots for each of these?  

# First, let's define a function

plot_limb_height = function(.data, Site = "All") {
  .data %>% ggplot(aes(x = Limb, y = Height)) +
    geom_smooth(method = "lm") + geom_point() +
    ggtitle(Site)
}
plot_limb_height(lizards)

# Let's separate it
plot_table = lizards %>% group_by(Site) %>% 
  nest(.key = "data") %>% # Take a nested table
  mutate(plot = map2(data, Site, plot_limb_height)) # and use the mapping function
plot_table # the plot column is a list of ggplot objects

plot_table$plot[5]

dir.create("output") # create an output directory

# Let's save these plots
# First, create a data frame with the plot and the filename
save_table = plot_table %>% mutate(name = paste0("Site_", Site, ".png")) %>% 
  select(name, plot)
# Then write a function that will save it
save_plot = function(name, plot, ...) {
  ggsave(filename = file.path("output", name), plot = plot,
         ...) # ... allows for extra, fixed options
}
# And map it; note that the function arguments match the data frame column names
save_table %>% pmap(save_plot, width = 5, height = 4, dpi = 200) # width, height, and dpi go to the dots


# reduce() and accumulate() ###########

# map functions work really well if the different input groups 
  # don't interact with each other
# unfortunately, it doesn't work well with iterated functions,
  # where the output of one run of the function becomes its input in the next one.

# There are two functions that can handle this well, however:

# for example, to take the cumulative sum of a set of numbers, 
  # r has a built-in function called cumsum

cumsum(1:10)

# We can re-create this with an accumulate function

plus = function(left, right) {
  left + right
}

accumulate(.x = 1:10, 
           .f = plus)
# Let's break down what's going here:
# .x is the data we're passing,
# .f must be a function that takes at least two arguments
  # When accumulate is first called, 
  # it calls plus(.x[1], .x[2])
  # Then it takes the result of that and calls plus(result, .x[3])
  # It continues this until it runs out of .x
  # The result is each output of plus()

# If you only want the final value, you can instead use reduce()
reduce(.x = 1:10, .f = plus)

# accumulate and reduce also take lambda functions
accumulate(1:10, ~.x + .y)

# alternatively, you could just pass the + operator directly; 
  # it's also a function that takes two functions
accumulate(1:10, `+`) # note the back-ticks; this tells R that "+" is a function's name

# Finally, you can also specify an initial value that isn't part of .x;
  # In this case, the first evaluation of .f would be .f(.init, .x[1])
accumulate(1:10, ~.x + .y, .init = 1000)

#### Exercise #####
#### Idea?

# Combining map and reduce functions can be a powerful goal
  # For example, let's say you wanted to remove all of the rows
  # of a data frame that contain any NA values, but you don't know
  # how many columns there are or what their names are.
  # (note that tidyr contains the drop_na() function, which does this already)
library(readxl)
# Let's pull in some data with lots of NAs
messy_lizards = read_excel("data/anoles_messy.xlsx", sheet = "individual_data")

messy_lizards_with_na = messy_lizards %>% 
  mutate(na_row = 
           map(messy_lizards, is.na) %>% 
           # mapping a data frame applies the function to all columns
           # The output will be a list of logical vectors
           reduce(`|`) # the | is the vectorized logical OR; 
              # if any of the logical vectors are true at a given position,
              # the output will be true there
         ) 
messy_lizards_with_na %>% filter(na_row) %>% View # All remaining rows contain at least one NA
messy_lizards_with_na %>% filter(!na_row) # drop the rows

# Accumulate can even be used for iterated simulations
# For example: here's a simple model of genetic drift

drift_one = function(allele_freq, generation, N) {
  # Generate next generation from a binomial distribution
  new_allele_freq = sum(rbinom(n = N, size = 2, prob = allele_freq)) / (2.0*N)
  new_allele_freq # return this
}

# Simulate 100 generations of drift 
accumulate(1:50, # This is passed to generation
           drift_one, N = 100, # As with map(), this is a fixed parameter
           .init = .1) #initial allele freq

# This would be easier as a function
genetic_drift = function(initial_freq, N, generations = 50) {
  accumulate(1:generations, # This is passed to generation
           drift_one, N = N, # As with map(), this is a fixed parameter
           .init = initial_freq) #initial allele freq
}
genetic_drift(.1, 100, 100)

# Note that in many cases, most of the final generations are at 0; 
  # there's no real reason to run this the allele fixes
  # functions passed to reduce() and accumulate() can have special 
  # triggers to terminate early
drift_two = function(allele_freq, generation, N) {
  # Generate next generation from a binomial distribution
  new_allele_freq = sum(rbinom(n = N, size = 2, prob = allele_freq)) / (2.0*N)
  fixation = new_allele_freq <= 0 || new_allele_freq >= 1
  if(fixation) {
     done(new_allele_freq) # wrap the final value in done() if it has finished
  } else {
    new_allele_freq # return this
  }
}
genetic_drift_2 = function(initial_freq, N, generations = 50) {
  accumulate(1:generations, # This is passed to generation
             drift_two, N = N, # As with map(), this is a fixed parameter
             .init = initial_freq) #initial allele freq
}
genetic_drift_2(.1, 100, 100)
