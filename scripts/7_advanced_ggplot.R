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
                                fill_scale = scale_fill_viridis_d()) {
  ggplot(.data, aes(x=1,fill = Color_morph)) +
    geom_bar() + theme_nothing()+
    fill_scale + 
    # scale_color_manual(values = c("skyblue1", "chocolate", "green3")) +
    coord_polar("y", 0)
}
lizards %>% filter(Site == "A") %>% make_proportion_plot

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
plot_A = lizards %>% filter(Site == "A") %>% make_proportion_plot

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
  mutate(subplot = map(.color, make_proportion_plot)) %>% #create the plot
  pmap(geom_proportion, size_scale = 0.0025)
    

anole_map + prop_plots
