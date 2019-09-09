library(tidyverse)
library(cowplot)
############ List Refresher #############
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
test_list[2] 
test_list[2:3] # (Pull up pepper jar analogy)

# This nests
test_list[[2]][[2]]
test_list$its_a$status
test_list$its_a[[2]]


######## List columns and nested Data Frames ######
# A list is a type of vector, so you can make it a column in a data frame

example = tibble(a = 1:4, b = c("A","B","A","B")) %>% 
  mutate(c = list(
    c(1L,2L), c(1:4), c(2L), c(85:90)))
example
example$c
example %>% unnest(c) # expand the list-column C

# You can also compress columns into nested data frames
tmp = example %>% unnest(c) %>%  
  nest(c, .key = "nested_data")
tmp$nested_data

example %>% unnest(c) %>%  
  nest(b, c, .key = "nested_data")
# By default, it will nest everything
example %>% unnest(c) %>% nest()
# Except for grouping variables
example %>% unnest(c) %>% group_by(b) %>% 
  nest()

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
    probs = (1:3)/4) # probs is an argument passed to quantile() that doesn't vary

# You can specify the return type if you know it ahead of time
map_dbl(x_list, mean) # returns a numeric (double) vector
map_int(x_list, length) # returns an integer vector
# Also map_chr, map_lgl, 
# Note that these only work for one-to-one functions:
  # one output (int, char, etc) for each input (.x)
  # when you have more than one output length 
  # (or aren't sure), use map()

# There's also
# map_dfr, map_dfc (these create data frames by binding the output into rows (dfr) or columns (dfc)
# default map returns a list

# map also works with pipes
x_list %>% map_dbl(mean)

# Anonymous functions: 
# You can use map with short custom functions
# For example:
x_list %>% map_dbl(~exp(mean(log(.x))))
  # The tilde tells map that you're creating a new function 
    # (note: this doesn't work outside of map and some other tidyverse functions)
    # The function's first argument will be called .x
# This is the same as:
geometric_mean = function(.x) {
  exp(mean(log(.x)))
}
map_dbl(x_list, geometric_mean)

1:26 %>%  map_dfr(~tibble(position = .x, caps = LETTERS[.x], lower = letters[.x])) # Note how it returns a row-bound data frame

########## Exercise: ##############
# There are 19 data files in data/sites
file_names = dir("data/sites", full.names=TRUE)
# file_names = dir("../RStats/automation/data/sites", full.names=TRUE)

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