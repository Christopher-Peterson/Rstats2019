# Week 2: Data manipulation

## This week, we're talking about dplyr
## It corresponds with chapter 5 of R for Data Science 
  ## (http://r4ds.had.co.nz/transform.html)
library(tidyverse)
library(cowplot)

lizards <- read_csv("data/anoles.csv")
tidy_quakes <- as_tibble(quakes) # A dataset of earthquakes near Fiji

#### mutate(): add a new column to a data frame ####

  mutate(.data = lizards, Total_length = SVL + Tail)
  # Note that you don't have to explicitly name the .data argument; just put it first
  mutate(lizards, Total_length = SVL + Tail)
  
  # You can mutate multiple variables in one command
  mutate(lizards, 
         total_length = SVL + Tail,
         rel_limb = Limb/SVL,
         log_total_length = log(total_length)) # You can refer to previously created columns 
  
#### The Pipe ( %>% ) ####
  
  mutate(lizards, Total_length = SVL + Tail)
  lizards %>% mutate(Total_length = SVL + Tail)
  
  # The pipe strings together functions.
  # It takes the result of one function and makes it the first argument of the next
  # b(a(x)) can be re-written as x %>% a() %>% b()
  # Think of it as "then"
  # Take x, then do a, then do b
  # This makes the order you write your code the same as the order it's executed.
  
  # Almost all of the functions in the tidyverse take data as the first argument, 
    # so the pipe can be very powerful
  
  # Example: take the lizard data set, then calculate the total length, then see how it varies among sites
  lizards %>% 
    mutate(Total_length = SVL + Tail) %>% 
    ggplot()+ aes(x = Site, y = Total_length) + geom_boxplot()

  # RStudio shortcut: Ctrl + Shift + M (Cmd + Shift + M on Mac)

  
  # Some other useful mutate commands
  lizards %>% mutate(intercept = 1) # creates a fixed column
  lizards %>% mutate(row_number = 1:n()) 
  
#### Exercises: 
  # 1. Add a column to the lizards dataset that gives the lizard's height relative to the maximum height.  Hint: max(x) shows the largest value of x. 
 
  # 2. Make a plot of relative limb length (Limb/SVL) vs. perch circumference (Diameter * pi)
  lizards %>% 
    # your code here %>% 
  ggplot() + aes(x = rel_limb, y = perch_circum) + geom_point()
  
#### filter(): returns subsets of a data frame ####
  filter(lizards, SVL > 60)
  # See what's going on here, look at the large column:
  mutate(lizards, large = SVL > 60)

  # This is a logical (TRUE/FALSE) column;
  # A filter command returns every row where SVL > 60 is TRUE
  
  # Logical vectors are created from conditional statements:
  # x == y; TRUE if x equals y
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
  

  # What's the SVL-Height relationship for Blue lizards in sites A through E?
  lizards %>% # You can filter multiple statements
    filter(Color == "Blue",
           Site %in% c("A", "B", "C", "D", "E")) %>%
    ggplot(aes(x = SVL, y = Height, color = Site)) + geom_point()

#### Exercises:
#### 
  # 3. Print a dataframe that shows only the lizards higher than 150 cm. Working off your code, count how many there were.
  
  # 4. How many lizards perching on trees or shrubs aren't brown?  Visualize the height to diameter relationship between them.
  
#### select(): keep or remove columns ####

lizards %>% select(Site, Color, SVL) # keeps the columns Site, Color, SVL

# Three ways to select variables:
# By name (which we just did)

# From a character vector
keep_cols = c("Site", "Color", "SVL")
lizards %>% select(keep_cols)

# By position
lizards %>% select( 1, 2, 7) 
lizards %>% select(1:4) # this is more useful for ranges of values

# Use negative signs to REMOVE columns
lizards %>% select(-Color, -Limb)
lizards %>% select(-(1:5))

#### arrange(): Sorts your columns ####

lizards %>% arrange(Mass) # Sorts from lowest to highest
lizards %>% arrange(desc(Mass)) # use desc() to sort from highest to lowest

  # Categorical variables:
lizards %>% arrange(Site) # Note how there's some ties
lizards %>% arrange(Site, Color, SVL) # You can add extra variables to break ties

  # This can be useful for creating rankings
  lizards %>% arrange(desc(SVL)) %>% 
    mutate(size_rank = 1:n())

#### summarize(): create summary statistics ####
  lizards %>% summarize(mean_SVL = mean(SVL), sd_SVL = sd(SVL),
                        med_SVL = median(SVL), count = n())
  # Summarize takes in a bunch of rows and returns one.  
  # You can use any function that takes in a vector and returns a single value.
  
  # One useful trick for getting proportions:
  lizards %>% summarize(prop_tree = mean(Perch_type == "Tree"), 
                        prop_shrub = mean(Perch_type == "Shrub"),
                        prop_building = mean(Perch_type == "Building"),
                        prop_other = 1 - (prop_tree + prop_shrub + prop_building))
  
  lizards %>% summarize_all(mean) # summarize_all applies the function to each variable.  Note that we're getting NAs for some, because you can't take the mean of a category.  
   
#### group_by(): apply functions separately to each group ####

# Summarize (and other functions) become much powerful when applied to group_data

  lizards %>% group_by(Site) # This doesn't seem to do much 
  
  lizards %>% group_by(Site) %>% 
    summarize(mean_SVL = mean(SVL), count = n()) 
  # But that's interesting...

  # You can group by multiple factors:
  lizards %>% group_by(Site, Color) %>% 
    summarize(mean_SVL = mean(SVL), count = n()) %>% ungroup() 
  
  # A more complicated example:
  # Plot the mean +/- standard error of each color at each site
  lizards %>% group_by(Site, Color) %>% 
    # Calculate Mean and SE
    summarize(mean_SVL = mean(SVL), std_err = sd(SVL) / sqrt(n())) %>% #
    ungroup() %>% 
    # Calculate upper/lower limits for error bars
    mutate(lower = mean_SVL - std_err, upper = mean_SVL + std_err) %>% 
    ggplot() + # Create the plot
    aes(x = Site, y = mean_SVL, color = Color, ymin = lower, ymax = upper) + 
    # Add the points and error bars
    geom_errorbar(position = position_dodge(width = .45)) +
    # Note that position argument is used to keep the colors from overlapping
    geom_point(position = position_dodge(width = .45))  
  
# group_by() doesn't just work with summarize()
  
  # What's the relative height of each lizard within each site?
  lizards %>% group_by(Site) %>% 
    mutate(rel_height = Height/max(Height)) %>% 
    # Note here that max(Height) was applied within sites
    ungroup %>% 
    ggplot(aes(x = Site, y = rel_height)) + geom_jitter(width = .2)

#### Exercises:

  # 5. Visualize the relationship between the maximum height at a site and the average limb length
  lizards %>% 
    # Your code here %>% 
    ggplot() + aes(x = max_height, y = mean_limb) + 
    geom_smooth(method = "lm") + geom_point() 
    
  # Advanced question: For each site, what's the mean limb length of the five largest individuals (by SVL)? What proportion of these indiviuals is blue? 
  