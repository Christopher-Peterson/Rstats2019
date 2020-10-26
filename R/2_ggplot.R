# Visualizing data with ggplot2

# Let's make some figures.
# We'll be using the ggplot2 package, which is part of tidyverse
library(tidyverse)
library(cowplot) 
library(ggforce)

# Read the data we'll be using
lizards <- read_csv("data/anoles.csv")

#### Base plotting ####
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

# Color_morph is another commonly used aesthetic

base_plot + aes(color = Color_morph) # discrete color

base_plot + aes(color = Limb) # continuous color

# Scales change the way aesthetics look
base_plot + aes(color = Limb) + scale_color_viridis_c()
base_plot + aes(color = Color_morph) + scale_color_viridis_d()

# Shape
base_plot + aes(shape = Color_morph)
base_plot + aes(shape = Color_morph) + scale_shape(solid = FALSE)

# Size
ggplot(data = lizards, aes(x = SVL, y = Tail, size = Limb)) + 
  geom_point(color = "cornflowerblue", shape = 1)
# Note that aes() was included in the ggplot call instead of being 
# Added to it; both ways are valid

# You can also give points fixed aesthetic values
ggplot(data = lizards) + 
  aes(x = SVL, y = Tail, shape = Color_morph) + 
  geom_point(size = 2.5, color = "cornflowerblue") + 
  scale_shape(solid = FALSE)

# Or double up on Aesthetics
ggplot(data = lizards) + 
  aes(x = SVL, y = Tail, shape = Color_morph, color = Color_morph) + 
  geom_point(size = 2.5) + 
  scale_shape(solid = FALSE)

base_plot + aes(color = Limb, shape = Color_morph) + 
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
  geom_histogram(binwidth = 2.5, 
                 color = "black", fill = "white") # binwidths option fixes the warning

# Density Plots
ggplot(data = lizards) + 
  aes(x = Diameter) + 
  geom_density()

# Exercise: How could you use a density plot or histogram to compare
# Diameter distribution of different lizard Color_morph morphs?

ggplot(data = lizards) + 
  aes(x = Diameter, color = Color_morph) + 
  geom_density()

ggplot(data = lizards) + 
  aes(x = Diameter) + 
  geom_histogram(
    aes(fill = Color_morph), color = "black",
    binwidth = 2.5)


# Discrete X, continuous Y

# boxplots
box_plots <- ggplot(data = lizards) + 
  aes(x = Site, y = Height) + 
  geom_boxplot() 

box_plots + aes(fill = Color_morph) # Note that we're using fill as the aesthetic, not color
# Generally, fill is used to color solid objects, color is used for lines or points

# Violin plot
ggplot(data = lizards) + 
  aes(x = Site, y = Height) + 
  geom_violin()  

# Jitter plot
ggplot(data = lizards) + 
  aes(x = Site, y = Height) + 
  geom_jitter(width = .25, height = 0)

# Continuous X and Y

# Line graph
ggplot(data = lizards) + 
  aes(x = SVL, y = Tail) + 
  geom_line()
ggplot(data = lizards) + 
  aes(x = SVL, y = Tail, color = Color_morph) + 
  geom_line()
# SO these are clearly not a good way to visualize this sort of data...


# Regression line, with errors
ggplot(data = lizards) + 
  aes(x = SVL, y = Tail) + 
  geom_smooth(method = "lm", se = TRUE) # use se = FALSE to disable error regions

ggplot(data = lizards) + 
  aes(x = SVL, y = Tail, color = Color_morph) + 
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
  geom_bar() + facet_wrap(~Color_morph)

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

regression_plot + facet_wrap(~Color_morph) # This splits the colors into separate plots

regression_plot + facet_wrap(~Site) # Note that by default, the scales are fixed to be the same
regression_plot + facet_wrap(~Site, scales = "free_x")

# Two_way faceting
regression_plot + facet_grid(Color_morph~Perch_type)

# Cheat sheet
# Go to https://ggplot2.tidyverse.org/ for the help files