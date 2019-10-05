#### MOVE ALL OF THIS TO EDA SECTION! ########
#### Data Cleaning ####
### I think this stuff below belongs in the following EDA lecture
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

### Throw in some list columns !!!!




##### Exploratory analysis review ####
library(tidyverse)
# Clean the following dataset
yeast_dat_messy = read_csv("data/yeast_data.csv")

#### Data Cleaning info ####

# The GID, YORF, and GWEIGHT columns are not necessary

# The NAME column contains multiple variables, which are separated by "---":
  # gene name, biological process, molecular function, systematic_id, and an extra ID number

# You will probalby want to remove the extra spaces around these new variables
  # The function trimws() can do this; use it on each of the newly created columns
  # (Hint: You can use individual mutate() commands, or try mutate_at())
  # mutate_at works on columns the same way that select() does

# The reamining columns (G0.05 through U0.3) indicate TWO variables in their name
  # The nutrient that was added to the substrate (first letter) and rate at which 
  # it was added (the rest)
  # the values in these columns is the gene expression level
  # Finally, remove anything with no expression data or no systematic id 
  # (empty quotes, "" or NA values)

clean_data = yeast_dat_messy %>% 
  select(-GID, -YORF, -GWEIGHT) %>%
  pivot_longer(-NAME, names_to = c("nutrient", "concentration"),
               names_sep = 1,
               values_to = "expression",
               names_ptype = list(nutrient = character(),
                                  concentration = numeric())) %>%
  separate(NAME, 
           into = c("gene_name", "bio_process", "mol_function", "syst_id", NA),
           sep = "---") %>%
  mutate_at(1:4, trimws) %>% 
  filter(syst_id != "", !is.na(syst_id), !is.na(expression))



# You can use this code to get the full names of the nutrients
# just be sure to adjust the names of 'clean_data' and 'nutrient' if necessary
clean_data %>%
  mutate(nutrient = recode(nutrient, 
                           G = "Glucose", L = "Leucine", P = "Phosphate",
                           S = "Sulfate", N = "Ammonia", U = "Uracil"))


# Visually explore the data

# There's way too much here to visually explore, but we can look at some interesting subsets.

# Exercise: How does the gene expression of LEU1 (gene_name == "LEU1") 
  # differ with increased nutrient rate?  How does this differ among nutrients? 
  # Is this pattern similar amongst different genes associated with 
  # leucine biosynthesis (BP == "leucine biosynthesis")?  
  # Visually explore the results

