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
# install.packages("ggforce")

# If you already have these installed, make sure they're the 
# latest versions.  If not, then please run the above code to
# update them

library(tidyverse)
library(cowplot)
# library(ggforce)

#### let's take a look at some data ####

lizards <- read_csv("data/anoles.csv") # This loads in the dataset

lizards # This is a tidy data frame;
# Each column is a separate variable; each row is an observation
glimpse(lizards)
View(lizards)

## Data visualization

base_plot <- 
  ggplot(data = lizards) + # sets up a plot around lizards
  aes(x = SVL, y = Tail) + # Connects columns of lizards to plot aesthetics
  geom_point() # defines a visual layer; in this case, points
base_plot # show results

# This is functional, but doesn't look much like a scientific figure
theme_set(theme_cowplot()) # changes the default theme 
base_plot 

#### Aesthetics ####
# Aesthetics connect a column of data to a visual representation on the plot
# for example, x and y are aesthetics that correspond to axis positions

# Color is another commonly used aesthetic

base_plot + aes(color = Color) # discrete color

base_plot + aes(color = Limb) # continuous color

# Scales change the way aesthetics look
base_plot + aes(color = Limb) + scale_color_viridis_c()

  # Shape
base_plot + aes(shape = Color)
base_plot + aes(shape = Color) + scale_shape(solid = FALSE)

# Size
ggplot(data = lizards, aes(x = SVL, y = Tail, size = Limb)) + 
  geom_point(color = "cornflowerblue", shape = 1)
# Note that aes() was included in the ggplot call instead of being 
  # Added to it; both ways are valid

# You can also give points fixed aesthetic values
ggplot(data = lizards) + 
  aes(x = SVL, y = Tail, shape = Color) + 
  geom_point(size = 2.5, color = "cornflowerblue") + 
  scale_shape(solid = FALSE)

# Or double up on Aesthetics
ggplot(data = lizards) + 
  aes(x = SVL, y = Tail, shape = Color, color = Color) + 
  geom_point(size = 2.5) + 
  scale_shape(solid = FALSE)

base_plot + aes(color = Limb, shape = Color) + 
  scale_shape(solid = FALSE) +
  scale_color_viridis_c()
  
#### Geoms ####
# Geoms are different ways to visualize data

## Discrete X ##
# Histograms
ggplot(data = lizards) + 
  aes(x = Diameter) + 
  geom_histogram()
# Note that there's a warning here; let's address it

ggplot(data = lizards) + 
  aes(x = Diameter) + 
  geom_histogram(binwidth = 2.5) # binwidths option fixes the warning

# Density Plots
ggplot(data = lizards) + 
  aes(x = Diameter) + 
  geom_density()

# Exercise: How could you use a density plot or histogram to compare
  # Diameter distribution of different lizard Color morphs?

# Discrete X, continuous Y

# boxplots
box_plots <- ggplot(data = lizards) + 
  aes(x = Site, y = Height) + 
  geom_boxplot() 

box_plots + aes(fill = Color) # Note that we're using fill as the aesthetic, not color
# Generally, fill is used to color solid objects, color is used for lines or points

# Violin plot
ggplot(data = lizards) + 
  aes(x = Site, y = Height) + 
  geom_violin()  

# Jitter plot
ggplot(data = lizards) + 
  aes(x = Site, y = Height) + 
  geom_jitter(height = 0, width = .3)  # height and width control how much they can jiter


# Continuous X and Y

# Line graph
ggplot(data = lizards) + 
  aes(x = SVL, y = Tail) + 
  geom_line()
ggplot(data = lizards) + 
  aes(x = SVL, y = Tail, color = Color) + 
  geom_line()
# SO these are clearly not a good way to visualize this sort of data...


# Regression line, with errors
ggplot(data = lizards) + 
  aes(x = SVL, y = Tail) + 
  geom_smooth(method = "lm", se = TRUE) # use se = FALSE to disable error regions

ggplot(data = lizards) + 
  aes(x = SVL, y = Tail, color = Color) + 
  geom_smooth(method = "lm", se = TRUE) # use se = FALSE to disable error regions

# Combining geoms
regression_plot <- ggplot(data = lizards) + 
  aes(x = SVL, y = Tail) + 
  geom_point(size = 2.5, shape = 1, alpha = .5) + 
  geom_smooth(method = "lm") # adds regression line
regression_plot 

# Bar graphs involve a statistical transformation of the data

# How many individuals were at each site?
ggplot(data = lizards) + 
  aes(x = Site) +
  geom_bar()

ggplot(data = lizards) + 
  aes(x = Site, fill = Perch_type) +
  geom_bar()

ggplot(data = lizards) + 
  aes(x = Site, fill = Perch_type) +
  geom_bar(position = "dodge")

ggplot(data = lizards) + 
  aes(x = Site, fill = Perch_type) +
  geom_bar() + facet_wrap(~Color)

# re-ordering bar graphs
  # You can use fct_infreq() on the x aesthetic to re-order it by frequency 
bars_in_order = 
  ggplot(data = lizards) + 
  aes(x = fct_infreq(Site), 
      fill = Perch_type) +
  geom_bar()
bars_in_order
bars_in_order + xlab("Site")

# Exercise: explore the lizards dataset to see if there are any 
# apparent relationships between different variables

#### Facets ####
# Facets are a way to create several smaller plots out of one dataset

regression_plot + facet_wrap(~Color) # This splits the colors into separate plots

regression_plot + facet_wrap(~Site) # Note that by default, the scales are fixed to be the same
regression_plot + facet_wrap(~Site, scales = "free_x")

# Two_way faceting
regression_plot + facet_grid(Color~Perch_type)

# Cheat sheet
# Go to https://ggplot2.tidyverse.org/ for the help files