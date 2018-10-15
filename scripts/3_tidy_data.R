# Tidyr
library(tidyverse)
library(cowplot)
library(readxl)

## New data set: 
succession_data = read_csv("data/wide_succession_data.csv")
glimpse(succession_data)
View(succession_data)

## These data were collected by my Field Ecology students
## Four Teams surveyed the diversity and density of 
## three habitat types at Brackenridge Field Lab
## In each Habitat type, each team selected three points (Sample),
## and recorded 4 canopy (C) and 4 understory (U) trees at each point
## The distance to each tree, the DBH of the canopy trees, 
## and the species of tree was also recorded

## A few notes about the dataset:
  ## Habitat type and canopy/understory type are combined into the "Type" column
  ## Species presence is indicated by a "1" in the appropriate column
  ## A specific sample point is identified by the combination of 
  ## Habitat, Sample, and Team

# First, we should convert all of the presence/absense species 
  # columns into a single column
# This is done with the gather() command
gathered_succ = succession_data %>% 
  gather(key = "Species", # Name of the column that stores the old column names
         value = "is_present", # Name of the oclumn that stores old cell values
         -Type, -Team, -Sample,  # Here, you put the columns that you want to gather
         -Quadrant, -Distance, -DBH) # this works just like select()
         # In this case, it's easier to list the columns we DON'T want to gather
# Equivalently:
succession_data %>% 
  gather(key = "Species", value = "is_present", 7:22) # Column numbers also work
View(gathered_succ) # Note the Species and is_present column

# All the is_present column is telling us is whether that particular species was 
# there or not; we don't need the absences, so we can get rid of them
gathered_succ %>% filter(!is.na(is_present)) %>% select(-is_present)
# To do anything interesting with this, we should be able to separately 
# work with habitat type and canopy type

# the separate() command works for that

gathered_succ %>% filter(!is.na(is_present)) %>% select(-is_present) %>% 
  separate(Type, # Column to separate
           c("Habitat", "Canopy"), # Names of the new columns to separate into
           sep = "-") # the character used to mark the separation

# If we want to be able to identify individual sample points, we should make a new column

tidy_succession =  gathered_succ %>% 
  filter(!is.na(is_present)) %>% select(-is_present) %>% 
  separate(Type, c("Habitat", "Canopy"), sep = "-")  %>% 
  mutate(Sample_point = paste(Habitat, Team, Sample, sep = "-")) %>% 
  select(-Team, -Sample, -Quadrant)

# Now we can ask some questions about the data
# How many of each speceis were found in each canopy type?  Does it differe between Habitat and Canopy?
tidy_succession %>% group_by(Habitat, Canopy, Species) %>% 
  summarize(N = n()) %>% mutate(freq = N/sum(N)) # Note: this is the relative frequency of the habitat/canopy combination, due to the dataframe's grouping

# Create a bar plot showing how species abundnace varies among 
# habitats and canopy types
tidy_succession %>% group_by(Habitat, Canopy, Species) %>% 
  summarize(N = n()) %>%
  ggplot() + aes(x = Canopy, y = N, fill = Species) +
  facet_wrap(~Habitat) + 
  geom_col(position = position_dodge()) # Column chart; try re-running w/o position argument


### A look back at the lizard data

# This is more or less the same lizard data, but in a less processed state
messy_lizards <- read_csv("data/anoles_messy.csv")

# Take a look at the data
messy_lizards
glimpse(messy_lizards) # glimpse is helpful when you have a lot of columns
View(messy_lizards)

# This data is tidy
# Each row is a single observation,
# Each column is a variable.
# However, sometimes it might be convienient to over-tidy the data.

# Let's put all of the variables other than ID, Site, and Color into a single column.

# This can help us see if there are any obvious problems with our data 

over_tidy_anoles <- messy_lizards %>% 
  gather(key = "Variable", value = "Value", contains("("))
# the contains() function selects all variables with a "(" in their name
# You can find simlar functions by putting ?select_helpers into the console
# an alternative way to write this: 
  # over_tidy_anoles <- gather(messy_lizards, key = "Variable", value = "Value", -`Anole ID`, -Site, -Color)

over_tidy_anoles %>% ggplot() + 
  aes(x = Value) + # aes() connects columns in the data frame to graph features
  geom_histogram() + # Alternatively, you could try:
  # geom_density() + 
  # geom_freqpoly() +
  facet_wrap(~Variable, scales = "free") # A facet creates different sub-plots; 
# scales = "free" lets the different plots have different axis ranges

# Something looks funny with Snout-Vent Length

messy_lizards %>% # arrange(desc(...)) sorts in descending order
  arrange(desc(`Snout-Vent Length (mm)`)) %>% glimpse

## Our problem lizard is apparently 80 meters long.
## You can either replace this with a missing value (NA) or check if there was a data entry error.
## For this hypothetical example, let's say we our data sheet said it was supposed to be 80.4

# Most of the time, you'll probably just edit in your data sheet, but
# this is another option
messy_lizards <- messy_lizards %>% 
  mutate(`Snout-Vent Length (mm)` = # The if_else() function
           if_else(`Snout-Vent Length (mm)` == 80469.2, # This is a condition
                   80.4, # It returns this value where the condition is TRUE
                   `Snout-Vent Length (mm)`)) # and this value where it is FALSE

messy_lizards <- messy_lizards %>% 
  rename(ID = `Anole ID`, SVL = `Snout-Vent Length (mm)`,
         Tail = `Tail Length (mm)`, Limb = `Limb Length (mm)`, Mass = `Mass (g)`,
         Height = `Perch Height (cm)`, Diameter = `Perch Diameter (cm)`)

messy_lizards %>% ggplot(aes(x = SVL)) + geom_histogram()


##############
## Exercise: 
## Re-make over_tidy_anoles now that we've corrected the data
##############


# We also have some data about each Site.  Let's read this in.
anole_sites = read_csv("data/anole_sites.csv")

anole_sites
## The Precipitation and Temperature rows have text included with their values.  

##########
## Exercise:
## Use remove the units from Precipitation and Temperature columns;
## What does the 'convert = TRUE' argument in separate() do, and would it be useful in this case?
##########



# We'd like to combine this table with the individual anole data.  To do that, we need a join operation
anole_data = left_join(messy_lizards, anole_sites, by = "Site") # there are a variety of join functions; look at the dplyr cheatsheet to get a feel for them
anole_data # This adds new columns to anole corresponding to the site's temp., precip., lat., and long.

# Let's save our cleaned and joined data.
write_csv(anole_data, "data/clean_anole_data.csv")

