library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())

lizards = read_csv("data/anoles.csv")

## Arranging figures in a grid ####
# Let's say you want to arrange some figures.

fig_1 = lizards %>% 
  ggplot() + 
  aes(x = Limb, y = Height, color = Site) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_d(guide = "none")
fig_2 = lizards %>% 
  ggplot() + 
  aes(x = Site, y = SVL, fill = Site)+
  geom_boxplot() +
  scale_fill_viridis_d()

fig_1
fig_2

# Let's say we wanted to arrange these for a manuscript
# plot_grid (from cowplot) can do this
plot_grid(fig_1, fig_2, ncol = 2)

plot_grid(fig_1, fig_2, nrow = 2)

# Alternate method, if you have a list of ggplots:
fig_list = list(fig_1, fig_2)
plot_grid(plotlist = fig_list, ncol = 2) # note that the plotlist argument must be named

# Note in this case that the axes aren't aligned
plot_grid(fig_1, fig_2, nrow = 2, align = "v", # v(ertical), h(orizontal), or both (vh)
          axis = "lr") # l(eft), r(ight), t(op), b(ottom)

plot_grid(fig_1, fig_2, nrow = 2, align = "v", # align can be "v", "h", "vh", or "hv"
          axis = "l") # Axis tells plot_grid where to align things


plot_grid(fig_1, fig_2, ncol = 2, align = "h", axis = "tb", 
          rel_widths = c(1, 1.5)) # Give the second column a bit more room
# There's also rel_heights for rows

# Let's add some labels to each panel
plot_grid(fig_1, fig_2, ncol = 2, align = "h", axis = "tb", rel_widths = c(1, 1.3),
          labels = c("A", "B"), 
          label_x = 0, label_y = 1, # x & y go from 0 (left/bottom) to 1 (top/right)
          hjust = -0.5, vjust = 1.5) 
# hjust and vjust affect the horizontal and vertical justification
# 0 is right/top justified, 1 is left/bottom justified, and numbers outside that range go further in that direction
# Thus, hjust = -0.5, vjust = 1.5 brings the labels slightly below and to the right of their x and y positions

# Let's go with horizontal arragement 
plot_grid(fig_1, fig_2, ncol = 1, align = "v", axis = "lr", rel_heights = c(1,1),
          labels = c("A", "B")) 
# The legend isn't really working with these
legend_2 = get_legend(fig_2)
no_legend = plot_grid(fig_1, fig_2 + theme(legend.position = "none"),
          ncol = 1, align = "v", axis = "lr", rel_heights = c(1,1),
          labels = c("A", "B")) 
no_legend
plot_grid(no_legend, legend_2, ncol = 2, rel_widths = c(1, .1))

  # seting the font scale
theme_set(theme_cowplot(font_size = 20, line_size = 1)) # This is good for presentations
fig_2
theme_set(theme_cowplot(font_size = 14)) # This is good for papers
fig_2
# Note that theme_set wont work on anything that's already been created by plot_grid()

## Customizing the appearance of a plot ####

# adding extra labels:
fig_2 + ylab("Snout-Vent Length") # also works with xlab
fig_2 + ggtitle(label = "SVL by Site", subtitle = "We don't usually use these")
fig_2 + annotate("text", x = 10, y = 84.5, label = "This is an  outlier", hjust = 1.05 )

# Fun fact: you can add ggplot elements from a list
fig_2 +
list(ylab("Snout-Vent Length"), 
     ggtitle(label = "SVL by Site", subtitle = "We don't usually use these"),
     annotate("text", x = 10, y = 84.5, label = "This is an  outlier", hjust = 1.05))


# Changing x/y limits
fig_2 + ylim(50, 100) # ylim/xlim remove data that isn't in the range
fig_2 + coord_cartesian(ylim = c(50, 100)) # coord_cartesian doesn't remove data

#### theme customization ####
# more custom options: theme()
# You can control almost any part of a plot with theme()
?theme
# themes are composed of graphical elements
# You see their arguments here:
?element_text
fig_2 + theme(axis.text = element_text(size = 25)) 
# redefines text on the axis to have size 25

fig_2 + theme(axis.line.x = element_blank(), 
              axis.ticks.x = element_blank(), 
              axis.text.x = element_text(size = 25),
              axis.title = element_text(size = 8))
# element_blank() is used to remove something from a theme

# Facet elements
fig_3 = lizards %>% 
  ggplot(aes(x = Limb, y = Diameter)) + facet_grid(Color_morph~., switch = "y") + geom_point()
fig_3
fig_3 + theme(strip.background = element_blank(),
              strip.placement = "outside",
              strip.text = element_text(size = 20)) # strip refers to facet labels

# legend elements
fig_2 + theme(legend.position = "top", 
              legend.background = element_rect(fill = "grey", # bg color
                       color = "black",  linetype = 1, size = .5), # line details
              legend.title = element_text(size = 20)) 
# Placing the legend inside the plot
fig_2 + theme(legend.direction = "horizontal", 
              legend.position = c(0, 1), # coords range from 0 to 1 inside the plot range
              legend.justification = c(0,.8)) # hjust and vjust

# ggplot: scales ####
fig_3 + aes(color = SVL, size = Height) + 
  scale_size_continuous() + 
  scale_color_viridis_c(guide = "none") # a way to remove specific parts of the legend

fig_3 + aes(color = SVL) + 
  scale_color_viridis_c(direction = -1) # Reverse direction of colors
fig_3 + aes(color = SVL) + 
  scale_color_viridis_c(option = "magma") # Reverse direction of colors

# There are a number of available color scales
fig_3 + aes(color = SVL) + scale_color_gradient(low = "black", high = "pink")
fig_3 + aes(color = SVL) + scale_color_distiller(palette = 4)

# X and y scales ####
fig_2
fig_2 + scale_y_continuous(breaks = seq(40, 85, by = 5), position = "right")

labeled_breaks = c(40, "", 50, "", 60, "", 70, "", 80, "")

# What if we only want to label a subset of our axis ticks?
fig_2 + scale_y_continuous(breaks = seq(40, 85, by = 5), 
                           labels = labeled_breaks)
# If we wanted to automate this, we could write a function
thin_axis_label = function(thin_by = 10) { 
  # Only shows labels when the break is amultiple of thin_by
  function(breaks) if_else((breaks %% thin_by) == 0, as.character(breaks), "")
}
fig_2 + scale_y_continuous(breaks = seq(40, 85, by = 5), 
                           labels = thin_axis_label(10))
fig_2 + scale_y_continuous(breaks = seq(40, 85, by = 5), 
                           labels = thin_axis_label(20))
# Adding subplots ####

grand_cayman_map = read_rds("data/cayman_map.rds")
library(readxl)
lizard_sites = read_excel('data/anoles_messy.xlsx', sheet = "site_data") %>% 
  select(Site, Latitude, Longitude)

# Let's say we wanted to add a pie chart showing different color combinations
  # at each point on the map

anole_map = 
  ggplot(grand_cayman_map, aes(x,y)) +
  geom_polygon(aes(group = group),
    fill = "transparent", 
    color = "black") + 
  coord_fixed() + # keeps fixed 1:1 aspect ratio
  theme_nothing() # removes axes and everything
anole_map

# Here's the sites
anole_map + 
  geom_text(aes(x = Longitude, 
                 y = Latitude, label = Site), color = "red",
             data = lizard_sites)
# What if we want to show the proportion of colors at each site?
lizards


# Let's do it for one site
make_proportion_plot = function(.data, 
                                .extra = list()) {
  ggplot(.data, aes(x=1,fill = Color_morph)) +
    geom_bar() + theme_nothing() + 
    # scale_color_manual(values = c("skyblue1", "chocolate", "green3")) +
    coord_polar("y", 0) +
    .extra # lets us flexibly add extra elements to the plot
}
plot_A = lizards %>% filter(Site == "A") %>% make_proportion_plot
plot_A
# Add it to the map

geom_proportion = function(subplot, Longitude, Latitude, 
                           N, size_scale = .001, ...) {
  # We want the size of the dot to vary with sample size
  # size-scale converts from units of N to units of lat/long
  size = log10(N) * size_scale
  # Define the extend borders of the subplot
  xmin = Longitude - size/2
  xmax = Longitude + size/2
  ymin = Latitude - size/2
  ymax = Latitude + size/2
  # create an annotation object that can be added to a ggplot
  annotation_custom(ggplotGrob(subplot),
                              xmin, xmax, ymin, ymax)
}

x_a = lizard_sites$Longitude[1]
y_a = lizard_sites$Latitude[1]

anole_map + geom_proportion(plot_A, x_a, y_a, 
                    N = sum(lizards$Site == "A"), size_scale = .002)

# Now let's add all of them
# we're going to use pmap on geom_proportions, so we need columns:
  # Longitude, Latitude, subplot, and N (subscale will be a constant)
color_data = lizards %>% 
  select(Site, Color_morph) %>% 
  group_by(Site) %>%
  mutate(N = n()) %>% # this is per-site, due to group_by()
  # nest to one row per Site
  nest(.color = Color_morph) %>% 
  # Join this with site locations
  left_join(lizard_sites, by = "Site")
color_data

prop_plots = color_data %>%
  # Create the subplot as an extra column
  mutate(subplot = map(.color, make_proportion_plot, #create the plot
                       .extra = scale_fill_viridis_d())) %>% # define a better color scale as an extra object
  pmap(geom_proportion, size_scale = 0.0025)
    

anole_map + prop_plots


# Let's try this with a dataset that doesn't have all colors at all sites
lizards_reduced = read_csv("data/anoles_reduced.csv")

lizards_reduced %>% distinct(Site, Color_morph) %>% View


color_data_reduced = 
  lizards_reduced %>% 
  select(Site, Color_morph) %>% 
  group_by(Site) %>%
  mutate(N = n()) %>% # this is per-site, due to group_by()
  # nest to one row per Site
  nest(.color = Color_morph) %>% 
  # Join this with site locations
  left_join(lizard_sites, by = "Site")
prop_plots_reduced = color_data_reduced %>%
  # Create the subplot as an extra column
  mutate(subplot = map(.color, make_proportion_plot,
                       .extra = scale_fill_viridis_d())) %>% #create the plot
  pmap(geom_proportion, size_scale = 0.008)

anole_map + prop_plots_reduced

# From this plot, All of the single-morph sites have the same color, 
  # even though it's not the same color morph
  # This is because the automatic scales don't know there's more data there

# How to fix this: 
geom_blank() # adds nothing to the plot, but records scaling information
  # This lets you make sure that certain values are included in the scale, 
  # even if they don't appear in the plot

# How it works:
fixed_color_morphs = 
  lizards %>% distinct(Color_morph) %>% 
  mutate(x = 1)
fixed_color_morphs # This matches the defined
    # aesthetics in the make_proportion_plot function
scale_fixer = geom_blank(data = fixed_color_morphs) # add this to each subplot

prop_plots_reduced2 = color_data_reduced %>%
  # Create the subplot as an extra column
  mutate(subplot = map(.color, make_proportion_plot,
                       .extra = list( # list of extra objects
                         scale_fill_viridis_d(),
                         scale_fixer))) %>% 
  pmap(geom_proportion, size_scale = 0.01)

morph_plot = anole_map + prop_plots_reduced2
morph_plot

# Let's add the legend in general to this:
the_legend = get_legend( # from cowplot, extracts the legend from a plot
  make_proportion_plot(lizards_reduced) + 
    theme(legend.position = "top") + 
    scale_fill_viridis_d(name = "Color Morph")
)
plot_grid(morph_plot, the_legend,
          nrow = 2, rel_heights = c(10, 1))


### Reviewer 3 wants axis ticks under your facets ####

# Let's say you have a manuscript in review with the following figure:
original_plot = ggplot(lizards) + 
  aes(x = Height, y = Diameter) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(Color_morph ~ Perch_type, scales = "free")
original_plot

# Reviewer 3 wants you to add x axis ticks under each subplot and is unwilling to negotiate.
# Since ggplot doesn't do naturally do this, how could you proceede?  

# As a first step, we can make each row separately and combine them with plot_grid

# Let's make a function for it, first
make_plot = function(data) {
  ggplot(data) + 
    aes(x = Height, y = Diameter) + 
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE) +
    facet_grid(Color_morph ~ Perch_type, scales = "free")
}

# Split the lizard data frame into a lby color group
lizards_by_color = lizards %>% 
  group_by(Color_morph) %>% 
  group_split()
lizards_by_color

lizards_by_color %>% map(make_plot) %>% 
  plot_grid(plotlist = ., # Note that the "." means to take the pipe's input and put it ther
            align = "v", axis = "tb", nrow =3)

# So this is a mess; there are lots of unnecessary repeated elements that take up 
  # way too much space; also, the vertical axes don't have the same scales

# let's start by fixing the visual cruft

reduced_x_axis = theme(axis.text.x = element_blank(),
                       axis.title.x = element_blank(), 
                       )
reduced_y_axis = theme(axis.title.y = element_blank())
reduced_facet_labs = theme(strip.background.x = element_blank(),
                            strip.text.x = element_blank(), 
                           strip)
# This function will return a combination of the above elements depending on the position in the graph
reduce_cruft = function(position = c("top", "middle", "bottom")) {
  position = match.arg(position)
  # Return the appropriate theme combo depending on position
  switch(position,
         top = reduced_x_axis + reduced_y_axis,
         middle = reduced_x_axis + reduced_facet_labs,
         bottom = reduced_y_axis + reduced_facet_labs)
}

plot_parameters = tibble(data = lizards_by_color, 
                         position = c("top", "middle", "bottom"),
                         rel_heights = c(1.1, 1, 1.3)) 
                      # rel_heights lets us make the top and bottom a bit wider,
                      # to accomodate the extra text/strips

# We're going to define a function that takes a table of plot parameters and
  # a plotting function
  # the parameters should have data, position, and rel_heights as columns
  # We're using this formulation so that we can update the 
    # plotting function without having to update the scaffolding around it
pseudo_facet_x = function(plot_parameters, plot_function) {
  plt_tbl = plot_parameters %>% 
    mutate(plotlist = map2(data, position, plot_function)) %>% 
    select(plotlist, rel_heights)
  # This creates a data frame with the row's plot in one column
  # and the relative height in the other
  
  plot_grid(plotlist = plt_tbl$plotlist,  
              rel_heights = plt_tbl$rel_heights,
              nrow = 3, align = "v", axis = "tb")
}

# This just recreates the previous plot
plot_parameters %>% pseudo_facet_x(~make_plot(.x)) 
# recall, ~ creates an anonymous function; 
# ~ .x + .y  is equivalent to function(.x, .y) {.x + .y}


plot_parameters %>% pseudo_facet_x(~make_plot(.x) + reduce_cruft(.y)) 

# So this looks a lot better; unfortunately, the x axis ticks are still not ligned up
  # We could try adding xlim() or scale_x_continuous()
plot_parameters %>% 
  pseudo_facet_x(~ make_plot(.x) + reduce_cruft(.y) +
                   scale_x_continuous(limits = range(lizards$Height)))

# Unfortunately, this requires each column to have the same scale
  # To address this, we can once again use geom_blank(),
  # this time with faceting variables included

# Determine the min and max Height values for each perch type
perch_type_limits = lizards %>% group_by(Perch_type) %>% 
  summarize(min = min(Height), max = max(Height)) %>% 
  ungroup %>% 
  pivot_longer(c(min, max), names_to = "side", values_to = "Height")
perch_type_limits

make_plot_with_lims = function(data, position) {
  base_plot = make_plot(data)
# Create limits for this row that will fit easily within the y limits
  plot_lims = perch_type_limits %>% 
    mutate(Diameter = mean(data$Diameter, na.rm = TRUE))
  base_plot + geom_blank(data = plot_lims) + reduce_cruft(position)
}

plot_parameters %>% pseudo_facet_x(make_plot_with_lims)


# with a bit of work, you could genericize this process into a function
  # that starts with lizards and ends with the plot, without relying on 
  # any external data