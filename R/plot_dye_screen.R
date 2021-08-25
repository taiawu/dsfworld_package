# plot_subscreen()
# ---- plot_dye_screen()
# --------- theme_screen_austere()
# --------- add_facet_grid()
# --------- add_facet_wrap_10()
# --------- force_panel_sizing()

#' Default dye screen plot theme
#'
#' ggplot2 theme used in the dye screen plots.
#'
#' @param ... additional theme arguments which overwrite defaults within theme_bw()
#'
#' @return a plot theme which can be applied to any ggplot2 object with theme_screen_austere()
#'
#' @importFrom ggplot2 theme_bw theme element_text element_blank element_rect element_line unit %+replace%
#' @export
theme_screen_austere <-
  function(...) {
    theme_bw(...) %+replace%
      theme( # for the first hit-calling plot, the most austere
        # text
        plot.title = element_text(lineheight = .8, face = "bold", size = 12), # have a title
        plot.subtitle = element_text(lineheight = .8,  size = 12), # have a title
        legend.text = element_text(size = 6),
        strip.text = element_text(size = 6),
        axis.text = element_text(size = 4),
        axis.title = element_blank(), # don't label the axes
        strip.text.y = element_text(angle = 0),

        # borders and grids
        panel.border = element_rect(fill = NA, color = "black", size = 0.2),
        axis.ticks = element_line(size = 0.1, color = "darkgrey"), # dont have ticks
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),

        # positioning / arrangement
        # legend.position = "top", # put a legent on the right of the plot
        strip.background = element_blank(),
        panel.spacing.x = unit(0.1, "lines"),
        panel.spacing.y = unit(0.1, "lines"))
  }

#' Map dye screen aesthetics to a ggplot
#'
#' This function maps the core dye screening data to aesthetics used in the dye screen plots.
#' Later functions introduce faceting (add_facet_wrap_10(), add_facet_grid()), and enforce standard panel sizing (force_panel_sizing()).
#'
#' @param tidied_screen a tibble containing raw data from a dye screen, both protein and buffer, e.g. as output by tidy_dye_screen()
#' @param .temp_col the name of the column in tidied_screen containing Temperatures, as a string. Defaults to "Temperature".
#' @param .value_col the name of the column in tidied_screen containing raw values, as a string. Defaults to "value".
#' @param .color_col the name of the column in tidied_screen to be mapped to color. Defaults to "channel_f". If this column does not contain only the values "FAM", "JOE", "TAMRA", "ROX", "Cy5", and "Cy5.5", a vector mapping values to colors will need to be specified in the .scale_color_vals argument of this fucntion.
#' @param .linetype_col the name of the column in tidied_screen to be mapped to linetype. Defaults to "type". If this column does not contain only the values "protein", and "buffer", a vector mapping values to linetypes will need to be specified in the .scale_linetype_vals argument of this fucntion.
#' @param .variable_col the name of the column in tidied_screen which uniquely identified each trace. Defaults to "variable".
#' @param .line_size a number, giving the size of the lines on the plot. Defaults to 0.3
#' @param .line_alpha a number, giving the transparency of the lines on the plot. Defaults to 0.8
#' @param .color_legend_title a string, which appears as the title of the color variable in the legend. Defaults to "Channel"
#' @param .linetype_legend_title a string, which appears as the title of the linetype variable in the legend. Defaults to "", as this legend is pretty much self-explanatory in the default DSFworld plots.
#' @param .scale_color_vals a named character vector, passed to the values argument of scale_color_manual, assigning values mapped to the color aesthetic via the .color_col argument of this function, to their specific colors. Defaults to c("Cy5.5" = "#67000d", "Cy5" = "#a50f15", "ROX" = "#ef3b2c", "TAMRA" = "#f16913", "JOE" = "#74c476", "FAM" = "#2171b5").
#' @param .scale_linetype_vals a named character vector, passed to the values argument of scale_linetype_manaul, assigning values mapped to the linetype aesthetic via the .linetype_col argument of this function, to their specific linetypes. Defaults to c("Cy5.5" = "#67000d", "Cy5" = "#a50f15", "ROX" = "#ef3b2c", "TAMRA" = "#f16913", "JOE" = "#74c476", "FAM" = "#2171b5").
#' @param .plot_theme a theme to apply to the final plot. Defaults to 'theme_screen_austere', a theme defined inside this package.
#' @param .use_default_x_scaling a boolean value; whether or not to use the default x scale from ggplot2. Defaults to TRUE. If FALSE, the x axis is created with breaks set by the argument '.x_scale_breaks' to this function.
#' @param .x_scale_breaks a vector, giving values at which to place breaks on the x axis, if .use_default_x_scaling is set to FALSE. Defaults to NULL.
#' @param ... additional argument, passed to theme_screen_austere.
#'
#' @return a ggplot2 object containing dye screen data, and aesthetic mappings for geom_line(), along with olors, and linetypes.
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual scale_linetype_manual labs scale_x_continuous
#' @export
plot_dye_screen <-
  function(tidied_screen,
           .temp_col = "Temperature",
           .value_col = "value",
           .color_col = "channel_f",
           .linetype_col = "type",
           .variable_col = "variable",
           .line_size = 0.3,
           .line_alpha = 0.8,
           .color_legend_title = "Channel",
           .linetype_legend_title = "",
           .scale_color_vals  = c("Cy5.5" = "#67000d", "Cy5" = "#a50f15", "ROX" = "#ef3b2c", "TAMRA" = "#f16913", "JOE" = "#74c476", "FAM" = "#2171b5"),
           .scale_linetype_vals = c("buffer" = "dashed","protein" = "solid"),
           .plot_theme = theme_screen_austere,
           .use_default_x_scaling = TRUE,
           .x_scale_breaks = NULL,
           ...) {

    if(.use_default_x_scaling) {
      .x_breaks <-   seq(from = min(tidied_screen[[.temp_col]]), to = max(tidied_screen[[.temp_col]]), by = 10)
    } else {
      .x_breaks <- .x_scale_breaks
    }

    p <- tidied_screen %>%
      ggplot(
        aes(x        = .data[[.temp_col]], # temperature on X
            y        = .data[[.value_col]], # RFU on y
            color    = .data[[.color_col]], # colored by the state
            linetype = .data[[.linetype_col]],
            group    = .data[[.variable_col]]) # group means series, as in, this defines the unique data sets
      ) +

      # geoms
      geom_line(size = .line_size,
                alpha = .line_alpha) + # change the line type depending on the dye concentration # linetype = df_melt$conc #

      # manual aesthetic mappings
      scale_color_manual( values = .scale_color_vals) +
      scale_linetype_manual(values = .scale_linetype_vals) +

      # legend labels
      labs(color = .color_legend_title,
           linetype = .linetype_legend_title) +

      # themes
      theme_screen_austere(...) +

      # axis styling
      scale_x_continuous(breaks = .x_breaks)

  }

# # example
# screen_test <- plot_dye_screen(tidied_screen %>%
#                                  filter(dye %in% c("A016", "TW408")))
#
#
# # example
# hit_test <- plot_dye_screen(tidied_screen %>%
#                               filter(dye %in% c("A016", "TW408")),
#                             .value_col = "value_group_norm")

# ggplot2 facet_wrap vars
#' Add specific facet wrapping to to a ggplot2 object
#'
#' Has ncol = 10 and scales = "free"
#'
#' @param ... unquoted names of the variables by which to facet.
#'
#' @return a ggplot2 object, with facets added.
#'
#' @export
add_facet_wrap_10 <- function(...) {
  list(ggplot2::facet_wrap(ggplot2::vars(...),
                  scales = "free",
                  ncol = 10))
}

# ggplot2 facet_grid vars
#' Add specific facet grids to to a ggplot2 object
#'
#' has scales = "free"
#'
#' @param row_var unquoted name of a single variable mapped to facet rows.
#' @param col_var unquoted name of a single variable mapped to facet columns
#'
#' @return a ggplot2 object, with grid facets added.
#'
#' @export
add_facet_grid <- function(row_var, col_var) {
  list(ggplot2::facet_grid(rows = ggplot2::vars({{ row_var }}),
                  cols = ggplot2::vars({{ col_var }}),
                  scales = "free"))
}

#' Set specific panel sizes
#'
#' @param .force_height height of each panel in a facet. Defaults to 0.4
#' @param .force_width width of each panel in a facet. Defaults to 0.4 * 1.618 (golden ratio)
#' @param .units units of the given values. Defaults to "in"
#' @param ... additoinal arguments, to be tolerated from upstream functions but do nothing here.
#'
#' @return a ggplot2 object, with facets of specific sizes.
#'
#' @export
force_panel_sizing <-
  function(.force_height  = 0.4,
           .force_width = 0.4 * 1.618,
           .units = "in",
           ...) {

    list(ggh4x::force_panelsizes(rows = ggplot2::unit(.force_height, .units),
                                 cols = ggplot2::unit(.force_width, .units)) )
  }

#' Plot a dye screen for a subset of dyes, and either wrap or grid for facets.
#'
#'  A wrapper function for plot_dye_screen, meant to ease the creation of the standard plots (wrap by dye, and grid by dye and channel) created during a dye screen analysis workflow.
#'
#' @param tidied_screen a tibble containing raw data from a dye screen, both protein and buffer, e.g. as output by tidy_dye_screen()
#' @param dyes a character vector, containing names of the dyes to be displayed in the output plot. Gets passed to filter. Must be present in the column with the name supplied in the .dye_col argument of this function (defaults to "dye")
#' @param .facet_type the type of faceting to apply. Options are "wrap" or "grid", which call the helper functions add_facet_wrap_10 or add_facet_grid, respectively.
#' @param plot_title a title to add to the plot.
#' @param .dye_col a string, containing the name of the column in user-supplied data 'tidied_screen', containing dye names which can be filtered using the supplied vector passed to the 'dyes' argument of this function.
#' @param .channel_col a string, containing the name of the column in user-supplied data 'tidied_screen', containing channels.
#' @param ... additiona arguments, passed to force_panel_sizing
#'
#' @return a ggplot2 object containing dye screening data, complete with titles, facet sizes, and appropraite faceting.
#'
#' @export
plot_subscreen <-
  function(tidied_screen,
           dyes,
           .facet_type = "wrap",
           plot_title = "",
           .dye_col = "dye",
           .channel_col = "channel_f",
           ...) {

    p_int <-
      tidied_screen %>%
      dplyr::filter(.data[[.dye_col]] %in% dyes) %>%
      plot_dye_screen() +
      ggplot2::labs(title = plot_title)

    if(.facet_type == "grid") {
      p <-
        p_int +
        add_facet_grid(row_var = .data[[.dye_col]],
                       col_var = .data[[.channel_col]])

    } else {
      p <-
        p_int +
        add_facet_wrap_10(.data[[.dye_col]])

    }

    out_p <- p + force_panel_sizing(...)

  }
