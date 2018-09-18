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

# If you already have these installed, make sure they're the 
# latest versions.  If not, then please run the above code to
# update them


#### Read some data ####
library(tidyverse)
data_set <- read_csv("data/week_1.csv") # This loads in the dataset

data_set # This is a tidy data frame;
# Each column is a separate variable; each row is an observation
glimpse(data_set)
View(data_set)

## Data visualization

base_plot <- 
  ggplot(data = data_set) + # sets up a plot around data_set
  aes(x = SVL, y = Tail) + # Connects columns of data_set to plot aesthetics
  geom_point() # defines a visual layer; in this case, points
base_plot # show results

# This is functional, but doesn't look much like a scientific figure

library(cowplot) # Load this package to change the default appearance
base_plot 

# Some aesthetics
  # Color 

base_plot + aes(color = Color) # discrete color

base_plot + aes(color = Limb) # continuous color

# Scales change the way that data is mapped to visual elements
base_plot + aes(color = Limb) + scale_color_viridis_c()

  # Shape
base_plot + aes(shape = Color)
base_plot + aes(shape = Color) + scale_shape(solid = FALSE)

# Size
ggplot(data = data_set) + 
  aes(x = SVL, y = Tail, size = Limb) + 
  geom_point(color = "cornflowerblue", shape = 1)


# You can also give points fixed aesthetic values
ggplot(data = data_set) + 
  aes(x = SVL, y = Tail, shape = Color) + 
  geom_point(size = 2.5, color = "cornflowerblue") + 
  scale_shape(solid = FALSE)

# Or double up on Aesthetics
ggplot(data = data_set) + 
  aes(x = SVL, y = Tail, shape = Color, color = Color) + 
  geom_point(size = 2.5) + 
  scale_shape(solid = FALSE)

base_plot + aes(color = Limb, shape = Color) + 
  scale_shape(solid = FALSE) +
  scale_color_viridis_c()
  
# Geoms 

# boxplots
box_plots <- ggplot(data = data_set) + 
  aes(x = Site, y = Height) + 
  geom_boxplot() 

box_plots + aes(fill = Color)

# Histograms
ggplot(data = data_set) + 
  aes(x = Diameter) + 
  geom_histogram()

ggplot(data = data_set) + 
  aes(x = Diameter) + 
  geom_histogram(binwidth = 2.5)

# Combining geoms
regression_plot = ggplot(data = data_set) + 
  aes(x = SVL, y = Tail) + 
  geom_point(size = 2.5, shape = 1, alpha = .5) + 
  geom_smooth(method = "lm") # adds regression line
regression_plot 

# Facets
# Facets are a way to create several smaller plots out of one dataset

regression_plot + facet_wrap(~Color) # This splits the colors into separate plots

regression_plot + facet_wrap(~Site) # Note that by default, the scales are fixed to be the same
regression_plot + facet_wrap(~Site, scales = "free_x")

regression_plot + facet_grid(Color~Perch_type)

# Bar graphs involve a statistical transformation of the data

# How many individuals were at each site?
ggplot(data = data_set) + 
  aes(x = Site) +
  geom_bar()

ggplot(data = data_set) + 
  aes(x = Site, fill = Perch_type) +
  geom_bar()

ggplot(data = data_set) + 
  aes(x = Site, fill = Perch_type) +
  geom_bar(position = "dodge")

ggplot(data = data_set) + 
  aes(x = Site, fill = Perch_type) +
  geom_bar() + facet_wrap(~Color)

# Cheat sheet
# Go to https://ggplot2.tidyverse.org/ for the help files