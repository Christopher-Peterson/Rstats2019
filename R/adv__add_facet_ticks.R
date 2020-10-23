str(original_plot)
names(original_plot)

check_facet_grid = function(gg) {
  if(!inherits(gg$facet, "FacetGrid")) {
    stop("plot does not use facet_grid()", call. = FALSE)
  }
}
facet_pars = function(gg) {
  gg$facet$params[c("rows", "cols", "margins", "free", "space_free", 
                    "as.table", "switch", "drop")]
}

get_data_limits = function(facet_vars, axis_var, data) {
  var_name = rlang::expr_deparse(rlang::expr(axis_var))
  data %>% dplyr::group_by(!!!facet_vars) %>% 
    dplyr::summarise(min = min(!!axis_var, na.rm = TRUE), 
                     max = max(!!axis_var, na.rm = TRUE)) %>% 
    tidyr::pivot_longer(c(min, max), names_to = "side", values_to = var_name)
}
get_aes_limits = function(aes_vars, data) {
  dplyr::summarise_at(data, aes_vars, 
                      .funs = list(min = min, max = max), na.rm = TRUE) %>% 
    tidyr::pivot_longer(everything(), names_to = c(".value", "side"),
                        names_pattern = "(.+)_(m..)$")
}
rev_row = function(row) {
  the_expr = rlang::quo_get_expr(row)
  new_expr = rlang::expr(rev(!!the_expr))
  rlang::quo_set_expr(row, new_expr)
}

on_edge_2d = function(data_list, facet_vars, first_last = c("top", "bottom")) {
  # This determines if a data panel is on an edge
  panel_vars = purrr::map(data_list, dplyr::distinct, !!!facet_vars)
  first_facets = panel_vars[[1]]
  last_facets = panel_vars[[length(panel_vars)]]
  # the "first" edge is contained in the first panel; the last in the last
  # this function finds the other panels that match
  which_side = function(panel) {
    if(dplyr::all_equal(first_facets, panel)) {
      return(first_last[1])
    } else if(dplyr::all_equal(first_facets, panel)) {
      return(first_last[2])
    } else return("middle")
  }
  panel_vars %>% purrr:map_chr(which_side)
}
on_edge_1d = function(n, first_last = c("top", "bottom")) {
  c(first_last[1], rep("middle", n-2), first_last[2])
}

nonpos_aes = function(gg) {
  aes = gg$mapping %>% 
    purrr::discard(~names(.x) %in% c("x", "y")) %>% 
    unclass %>% unname
}
### THeme options ####
no_text_yl = theme(axis.text.y.left =  element_blank())
no_strip_x = theme(strip.background.x = element_blank(),
                            strip.text.x = element_blank())
no_strip_y = theme(strip.background.y = element_blank(),
                            strip.text.y = element_blank())
no_text_yr = theme(axis.text.y.r =  element_blank())
no_text_xr = theme(axis.text.x.r =  element_blank())
no_text_xl = theme(axis.text.x.l =  element_blank())

common_theme = theme(axis.title = element_blank(),
                              legend.position = "none")

themes_lr = function(side, switch_lgl, base = common_theme) {
  null_theme = theme()
  no_strip = function(yes) if(yes) return(no_strip_x) else return(null_theme) 
  base + switch(side, 
         middle = no_text_yl + no_text_yr,
         left = no_text_yr + no_strip(!switch_lgl),
         right = no_text_yl + no_strip(switch_lgl),
         )
}
themes_tb = function(side, switch_lgl, base = common_theme) {
  null_theme = theme()
  no_strip = function(yes) if(yes) return(no_strip_y) else return(null_theme) 
  base + switch(side, 
                    middle = no_text_xl + no_text_xr,
                    top = no_text_xr + no_strip(!switch_lgl),
                    bottom = no_text_xl + no_strip(switch_lgl),
  )
}
themes_2d = function(side_tb, side_lr, switch_chr){
  null_theme = theme()
  no_strip_x = function(yes) if(yes) return(no_strip_x) else return(null_theme) 
  no_strip_y = function(yes) if(yes) return(no_strip_y) else return(null_theme) 
  lr = themes_lr(side_lr, switch_chr %in% c("x", "both"))
  themes_tb(side_tb, switch_chr %in% c("y", "both"), base = lr)
}

subplot_themes(limit_list, axes, cols, rows, switch_strip) {
  # rows should be sort_rows
  if(is.null(switch_strip)) switch_strip = "none"
  if(axes != "xy") {
    theme_fun = switch(axes,
                       x = themes_tb,
                       y = themes_lr)
    # 1d options
    sides = on_edge_1d(length(limit_list), 
                       first_last = switch(axes,
                                           x = c("top", "bottom"),
                                           y= c("left", "right")))
    
    return(purrr::map(sides, theme_fun, 
               switch_lgl = switch_strip %in% c(axes, "both")))
  } else {
    side_lr = on_edge_2d(limit_list, cols, c("left", "right"))
    side_tb = on_edge_2d(limit_list, rows, c("top", "bottom"))
    purrr::map2(side_tb, side_lr, themes_2d, switch_chr = switch_strip)
  }
}
  ### Main function ####
add_facet_ticks = function(gg, axes = c("x", "y", "xy"), ...) {
  ## Initial checks and setup ####
  if(!is.ggplot(gg)) stop("gg must be a ggplot")
  check_facet_grid(gg) # throw error if false
  axes = match.arg(axes)

  aes_list = gg$mapping
  if(!all(c("x", "y") %in% names(aes_list))) stop(
    "add_facet_ticks only supports plots with x and y aesthetics.", call.=FALSE)
  
  facet = facet_pars(gg)
  if(!isFALSE(facet$margins)) stop(
    "add_facet_ticks does not support facets with margins", call.=FALSE  )
  data = gg$data
  legend = cowplot::get_legend(gg)
  # Create lists of data and limits tables ####
  # figure out what to do w/o y axis mappings...
  # Find data limits

  limit_tbl = purrr::reduce(
    c(purrr::map2(aes_list[c("x", "y")],
              facet[c("cols", "rows")],
              get_data_limits, data = data),
    get_aes_limits(nonpos_aes(gg), data)),
    .f = dplyr::full_join, by = "side")
  
  
  # figure out data order
    # plot_grid plots by column, so they should be sorted by 
    # column first, then by row
    # row sorting will be affected by "as.table"
  rows = facet$rows
  if(facet$as.table) {
    # apply rev() if necessary
    sort_rows = purrr::map(rows, rev_row)
    # Should this list self be reversed?  Check that
  } else {
    sort_rows = facet$rows
  }
  cols = facet$cols
  
  facet_vars_sort = switch(axes, 
                      x = sort_rows,
                      y = cols,
                      xy = c(cols, sort_rows))
  facet_vars = switch(axes, 
                      x = rows,
                      y = cols,
                      xy = c(cols, rows))
  # If this is only in x or y, skip the rows or cols...
  # data_list = data %>% 
  #   dplyr::arrange(!!!facet_vars_sort) %>% 
  #   dplyr::group_split(!!!facet_vars)

  
  limit_list = limit_tbl %>% 
    dplyr::arrange(!!!facet_vars_sort) %>% 
    dplyr::group_split(!!!facet_vars)
  # define n_rows and n_cols
  ## Element removal rules from subplots ####
  
  # limit list works as well as data list and has lower overhead
  theme_list = subplot_themes(limit_lists, axes, cols, sort_rows, facet$switch)
  
  gglist = map2(limit_list, theme_list, 
                function(lims, themes, gg) {
                  gg_subset(gg, dplyr::select(lims, !!!facet_vars)) + 
                    geom_blank(data = lims) +
                    themes
                }, gg = gg)
  
    
  
  # Now we just need to figure out:
  n_row = if(axis == "y") 1 else nrow(distinct(data, !!!rows))
  n_row = if(axis == "y") 1 else nrow(distinct(data, !!!cols))
  

  # 2. relative heights & widths
  cow_axis = switch(axes, y = "tb", x = "lr", xy = "lrtb")
  cow_align = switch(axes, x = "v", y = "h", xy = "vh")
  
  main_plot = cowplot::plot_grid(plotlist = gglist, nrow = nrow, ncol = ncol,
                     align = cow_align, axis = cow_axis,
                     rel_heights = rel_heights,
                     rel_widths = rel_widths)
  # Insert this into a generic ggplot w/ appropriate axis labels
  oob_x = limit_tbl[]
  scaffold = gg + facet_null() +
    coord_cartesian(xlim = oob_x) +
    theme(axis.text =  element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank())
  
  scaffold + annotation_custom(ggplotGrob(main_plot))
}
gg_subset = function(gg, match_tbl) {
  # match_tbl should be a data frame containing the relevant facet variables
  gg$data = gg$data %>% dplyr::right_join(match_tbl, by = names(match_tbl))
  gg
}
gg_subset(original_plot, tibble(Color_morph = "Blue"))


original_plot$facet$params %>% names
original_plot$facet %>% class

plot2 %>% get_facet_vars

plot2 = ggplot(lizards) + 
  aes(x = Height, y = Diameter) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(. ~ Perch_type, scales = "free")
