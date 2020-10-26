# Tidyr
library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())

## New data set: ####
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
  
#### pivot_longer ####

# This is done with the gather() command
succession_data %>% 
  pivot_longer(
    # The column names you want to reshape
    # In this case, as with dplyr::select, the minus indicates everything BUT 
    # these columns
    cols = -c(Type, Team, Sample, Quadrant, Distance, DBH),
    names_to = "Species", # Name of the column that stores the old column names
    values_to = "is_present") %>%  # Name of the oclumn that stores old cell values
  View

# Equivalently:
succession_data %>% pivot_longer(
    7:22, # positions of the columns to pivot
    names_to = "Species", values_to = "is_present")
succession_data %>% pivot_longer(
  # You can use ranges of column names, though that's not practical here
    `Acer negundo (Boxwood elder)`:`Ulmus crassifolia (Cedar elm)`,
    names_to = "Species", values_to = "is_present")
succession_data %>% pivot_longer(
  contains("("), # All of the species names have a parentheses in them
  names_to = "Species", values_to = "is_present")


# Note that there are a lot of NAs in the is_present column,
  # we can drop these with:
succession_longer = succession_data %>% 
  pivot_longer(7:22, names_to = "Species",
               values_to = "is_present", 
               values_drop_na = TRUE)
succession_longer



# All the is_present column is telling us is whether that particular species was 
# there or not; we don't need the absences, so we can get rid of them
succession_longer %>%  select(-is_present)

# Add an example?

#############
## Exercise 1
#############

# This is a sample of simulated genotypes from 100 individuals at 89 loci
# genotypes are coded 0, 1, 2 for how many of the minor alleles they have
genotypes_wide = read_csv("data/genotypes_wide.csv")

# reshape this data so that it has three columns: 
  # Indiv (as is)
  # POS (the position/locus, corresponding to column headers)
  # gt (the genotype, corresponding to cell values)
  # Remove the missing data
# Bonus: try using the "names_prefix" argument remove the "POS_"


#### Separate ####

# To do anything interesting with this, we should be able to separately 
# work with habitat type and canopy type

# the separate() command works for that

succession_sep = succession_longer %>%  
  select(-is_present) %>% 
  separate(col = Type, # Column to separate
           into = c("Habitat", "Canopy"), # Names of the new columns to separate into
           sep = "-") # the character used to mark the separation

# Individual sample points are identified by a combination of 
  # Habitat, Team, and Sample
  # we could make a new column for them with unite()

tidy_succession =  succession_sep %>% 
  # Combine the indicator
  unite(col = "Sample_quadrant", # new column name 
        Team, Habitat, Sample,# columns being combined
        sep = "-", remove = FALSE) %>% # remove = FALSE keeps the old columns around
  select(-Team, -Sample)
# Note: 
# you could have replaced unite() with 
# mutate(Sample_quadrant = paste(Team, Habitat, Sample, sep = "-)) %>% 


# Now we can ask some questions about the data
# How many of each speceis were found in each canopy type?  Does it differe between Habitat and Canopy?
tidy_succession %>% 
  group_by(Habitat, Canopy, Species) %>% 
  summarize(N = n()) %>% # total number of species per group
  # the summarize command ungrouped by Species
  mutate(freq = N/sum(N)) 
  # frequency of the species in the Habitat/Canopy

# Create a bar plot showing how species abundnace varies among 
# habitats and canopy types
tidy_succession %>% group_by(Habitat, Canopy, Species) %>% 
  summarize(N = n()) %>%
  ggplot() + aes(x = Canopy, y = N, fill = Species) +
  facet_wrap(~Habitat) + 
  scale_fill_viridis_d(option = "magma")+
  geom_col(position = position_dodge()) # Column chart; try re-running w/o position argument



#### TODO: ####
  ## pivot_longer() with multiple names or values columns
  ## Find some examples to use the more complicated pivot function examples
  ## pivot_wider()
  ## Maybe also use extract(), though that could require some regex bits?

## Pivoting with multiple columns

# These are data from an experiment that tested yeast gene expression
# under different levels of nutrient limitation
yeast_data = read_csv("data/yeast_data_partial.csv")
# yeast_data2 = read_csv("data/yeast_data_partial.csv")

# The first four columns indicate 
  # gene name, biological process, 
  # molecular function, and systematic_id

  # The remaining columns (G0.05 through U0.3) 
    # contain TWO variables in their name
    # The nutrient that was added to the substrate (first letter)
    # and rate at which it was added (the rest)
    # the values in these columns is the gene expression level
glimpse(yeast_data)

# We would like to convert all of these columns into three:
  # Substrate
  # Concentration
  # Gene_expression

yeast_data %>% 
  pivot_longer(G0.05:U0.3,
    names_to = c("Substrate", "Concentration"),
    names_sep = 1, # separate the names after the first character
    values_to = "Gene_expression")

# Notice that Concentration is listed as a 
  # <chr> column (i.e., text, not a number)
  # we can fix that with the names_ptype argument

tidy_yeast = yeast_data %>% 
  pivot_longer(G0.05:U0.3,
               names_to = c("Substrate", "Concentration"),
               names_sep = 1, # separate the names after the first character
               names_ptypes = list(Substrate = character(),
                                   Concentration = numeric()),
               values_to = "Gene_expression")
tidy_yeast

# Let's look at another way to do names_sep

### Another example:

# This contains site-level summary statistics for our lizard data
lizards_smry = read_csv("data/anoles_smry.csv")
View(lizards_smry)

# The column names are trait_summary
  # We want to create one column for each summary stat
  # and an extra column that indicates the trait
  # Desired columns:
    # Site
    # Trait
    # mean
    # sd
    # ...
    # max

lizards_smry %>% 
  pivot_longer(-Site, 
               names_to = c("Trait", ".value"),
               names_sep = "_")
# Two important things to note about this:
  # the ".value" in the names_to
    # This is a special indicator that tells 
    # pivot to create one column for each matching name
    # and place the corresponding values into it
    # Note we didn't use a values_to argument
  # names_sep used text instead of an integer
    # This splits at and removes the separator 
      # Note; names_sep treats this as regular expression (regex), 
      # so you may occasionally get some weird behavior 
      # we'll try to cover some regex stuff later in this seminar;
      # there's also a file "extra_regular_expressions.R" that you 
      # may wish to check out.

## Widening data #####

# Sometimes, you need to reverse a pivot_longer, 
  # many packages or external programs require
  # data to be in a matrix-like form, for example

# Let's widen our succession data
tidy_succession %>% 
  mutate(present = 1) %>% # We need to add this column because otherwise there's no value
  pivot_wider(
    names_from = Species, 
    values_from = present)
# We can provide options to automatically replace NAs with defaults
tidy_succession %>% 
  mutate(present = 1) %>% # We need to add this column because otherwise there's no value
  pivot_wider(
    names_from = Species, 
    values_from = present, 
    # This fills missing values from the present column w/ zeros
    values_fill = list(present = 0))

############
## Exercise 2
############
  # re-Widen the genotype data you tidied in exercise 1
  # each POS should become its own column
  # missing values should be replaced with -1


