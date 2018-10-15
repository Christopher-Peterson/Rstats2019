##### Exploratory analysis review ####
library(tidyverse)
# Clean the following dataset
yeast_dat_messy = read_csv("data/yeast_data.csv")

# Data Cleaning info
# Notes: The GID, YORF, and GWEIGHT columns are not necessary
# The NAME column contains multiple variables, which are separated by "---":
# gene name, biological process, molecular function, systematic_id, and an extra ID number

# You will probalby want to remove the extra spaces around these new variables
# The function trimws() can do this; use it on each of the newly created columns
# (Hint: You can use individual mutate() commands, or try mutate_at())
# mutate_at works on columns the same way that select() does

# The reamining columns (G0.05 through U0.3) indicate TWO variables in their name
# The nutrient that was added to the substrate (first letter) and rate at which it was added (the rest)
# the values in these columns is the gene expression level
# Finally, remove anything with no expression data or no systematic id (empty quotes, "" or NA values)
# Note: I may have previously refered to this as the "cancer dataset."  These data actually come from experiments on gene expression in Yeast after nutrient starvation.  I'm not really sure where I got the 'cancer' part from. 

# You can use this code to get the full names of the nutrients
# just be sure to adjust the names of 'clean_data' and 'nutrient' if necessary
clean_data %>%
  mutate(nutrient = recode(nutrient, 
                           G = "Glucose", L = "Leucine", P = "Phosphate",
                           S = "Sulfate", N = "Ammonia", U = "Uracil"))


# Visually explore the data

# There's way too much here to visually explore, but we can look at some interesting subsets.

# Exercise: How does the gene expression of LEU1 (gene_name == "LEU1") differ with increased nutrient rate?  How does this differ among nutrients? Is this pattern similar amongst different genes associated with leucine biosynthesis (BP == "leucine biosynthesis")?  Visually explore the results

