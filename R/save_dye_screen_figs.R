# save_dye_screen_figs()

# ----- depends on the save-specifics family
# ----------- save_full_screen()
# ----------- save_stacked_screen()
# ----------- save_stacked_hits()

# ------- save-specifics family depend on
# ---------------- save_stacked_plots()
# ------------------------- make_figure_title()
# ------------------------- convert_heights()
# ------------------------- pull_assigned()

#' Save dye screen figures
#'
#' The \code{save_dye_screen_figs()} function is used to quickly generate and save three of the four standard raw data figures in a dye screening workflow. See the 'Details' section below for more information on each of these plots.
#'
#'  The three plots which can be generating using \code{save_dye_screen_figs()} are:
#' \enumerate{
#' \item all raw data, with no assignments displayed. Used for calling hits, as created by the \code{save_full_screen()} function.
#' \item all raw data, with distinct sub-plots containing dyes given each assignment--hit, sensitive, or none--as created by the \code{save_stacked_screen()} function.
#' \item raw data for just dyes assigned to "hit" and "sensitive" categories, with data collected for each dye displayed in a separate row, and data for the raw data collected for that dye in each fluorescent channel displayed in a separate colummn, as created by the \code{save_stacked_hits()} function
#' \item Not accessed though this function. For reference, the fourth plot displays the hit and/or sensitive dyes, in specific channels, carried through model fitting and tma extraction, alongside the results of the model fitting and tma extraction.
#' }
#'
#' @param tidied_screen a tibble containing raw data from a dye screen, both protein and buffer, e.g. as output by the \code{tidy_dye_screen()} function.
#' @param plot_type type of figure to create. Options: "full_screen", "divided", "hits_by_channel", which generate and save figures created by the \code{save_full_screen()} function, the \code{save_stacked_screen()} function, and the \code{save_stacked_hits()} function respectively.
#' @param hits a tibble containing hit assignments for each dye. Must have columns "dye" and "assignment", described below:
#' \itemize{
#'   \item \strong{"dye"}, a character column containing the names of all of the dyes tested in the experiment. In order for a dye to be displayed in this plot, its name in this dataframe must match its name in the tidied_screen dataframe exactly. Matches are case- and punctuation-sensitive. (e.g. a dye named "L045" in this tibble will match only to a dye named "L045" in the tidied_screen tibble; it will not match to "l045", "L45", "L-045", etc.).
#'   \item \strong{"assignment"}, a character column containing the assignment of how a given dye behaved with screened protein. These assignments must be supplied by the user. Assignments can be determined either manually, or with assistance using the hit-calling tools in this package (in development as of August 2021). Possible assignments are: "hit", "sensitive", and "none", which correspond to the following judgements:
#'   \itemize{
#'   \item \strong{"hit"}, any dye which appears to detect the melting of the protein of interest, while having minimal temperature-sensitive fluorescence in the absence of protein.
#'   \item \strong{"sensitive"}, a less stringent category than 'hit'. Useful for dyes where definition as a "hit" isn't quite warranted, but which demonstrating some fluorescent response to the protein (over buffer) which could be potentially interesting. Common reasons for a a dye to receive a "senstive" assignment detection of non-cannonical transitions, and/or detecting a cannonical transtion, but doing so with far weaker signal than dyes called as 'hits'. In particular, the "sensitive" assignment is useful for flagging dyes which may be interesting for follow-up later, or fleshing out SAR of the hit dyes for a given protein.
#'   \item \strong{"none"}, an assignment given to dyes which showed no major difference between a DSF run with or without the protein, suggesting no fluorescent response to the protein of interest at any temperature.
#'   }
#'   }
#' @param ... additional arguments passed to the \code{save_full_screen()} function, the \code{save_stacked_screen()} function, or the \code{save_stacked_hits()} function. Common use could be to specify a directory to save the output plot, via .save_path.
#'
#' @return if assigned to an object, returns a ggproto object. Also saves the figure.
#'
#' @export
save_dye_screen_figs <-
  function(tidied_screen, plot_type, hits, ...) {

    out <- switch(plot_type,
                  "full_screen" = save_full_screen(tidied_screen, ...),
                  "divided" =  save_stacked_screen(tidied_screen, hits, ...),
                  "hits_by_channel" = save_stacked_hits(tidied_screen, hits, ...))
  }

#' Create and save full dye screen, sub-plotted by assignment
#'
#' Creates a plot object, and saves a figure, containing raw data for all screened dyes in all measured fluorescent channels. Results are divided into three subplots, containing dyes which are declared hits, sensitives, and neither (or "none"). Each dye is represented in a single wrapped facet, and both protein and buffer data are displayed.
#'
#' This plot is closely related to the one created by the \code{save_stacked_screen()} function, but differs in that only "hit" and "sensitive" dyes are displayed. Unlike the plot generated by  \code{save_stacked_hits()}, all channels are displayed within a single facet.
#'
#' This function is often accessed through it's wrapper function, \code{save_dye_screen_figs()}, though it can be used identically on it's own. See the  documentation of the \code{save_dye_screen_figs()} function for more information on what each of these four plots display.
#'
#'
#' @param tidied_screen a tibble containing raw data from a dye screen, both protein and buffer, e.g. as output by the \code{tidy_dye_screen()} function.
#' @param hits a tibble containing hit assignments for each dye. Must have columns "dye" and "assignment", described below:
#' \itemize{
#'   \item \strong{"dye"}, a character column containing the names of all of the dyes tested in the experiment. In order for a dye to be displayed in this plot, its name in this dataframe must match its name in the tidied_screen dataframe exactly. Matches are case- and punctuation-sensitive. (e.g. a dye named "L045" in this tibble will match only to a dye named "L045" in the tidied_screen tibble; it will not match to "l045", "L45", "L-045", etc.).
#'   \item \strong{"assignment"}, a character column containing the assignment of how a given dye behaved with screened protein. These assignments must be supplied by the user. Assignments can be determined either manually, or with assistance using the hit-calling tools in this package (in development as of August 2021). Possible assignments are: "hit", "sensitive", and "none", which correspond to the following judgements:
#'   \itemize{
#'   \item \strong{"hit"}, any dye which appears to detect the melting of the protein of interest, while having minimal temperature-sensitive fluorescence in the absence of protein.
#'   \item \strong{"sensitive"}, a less stringent category than 'hit'. Useful for dyes where definition as a "hit" isn't quite warranted, but which demonstrating some fluorescent response to the protein (over buffer) which could be potentially interesting. Common reasons for a a dye to receive a "senstive" assignment detection of non-cannonical transitions, and/or detecting a cannonical transtion, but doing so with far weaker signal than dyes called as 'hits'. In particular, the "sensitive" assignment is useful for flagging dyes which may be interesting for follow-up later, or fleshing out SAR of the hit dyes for a given protein.
#'   \item \strong{"none"}, an assignment given to dyes which showed no major difference between a DSF run with or without the protein, suggesting no fluorescent response to the protein of interest at any temperature.
#'   }
#'   }
#' @param ratio_marg a number, giving the ratio between the total size of a facet's plot panel and the their facet panel, inclusive of borders, axis ticks, etc. Ideally this could be reliably extracted from the plot object itself in a future version of this package. Is greater than one.
#' @param ... additional arguments, passed to the \code{make_figure_title()} and  \code{saved_stacked_plots()} functions. See documentation for the \code{make_figure_title()} and \code{saved_stacked_plots()} functions for possible options, which include assigning a directory to which to write the plot object created, and requesting the plot be saved using a specific  the graphics device  (default is "pdf" but all extentions compatible with \code{ggsave()} can be used).
#'
#' @return if assigned to an object, returns a ggproto object. Always saves a figure to the current working directory, or the directory specified through arguments to ...
#'
#' @export
save_stacked_screen <-
  function(tidied_screen,
           hits,
           ratio_marg = 150/110,
           ...) {

    dyes <-
      list(hits = pull_assigned(hits, "hit"),
           sensitivies = pull_assigned(hits, "sensitive"),
           nones = pull_assigned(hits, "none"))

    plot_heights <-
      c(convert_heights(dyes$hits,
                        .wrap_margin_ratio = ratio_marg),
        convert_heights(dyes$sensitivies,
                        .wrap_margin_ratio  = ratio_marg),
        convert_heights(dyes$nones,
                        .wrap_margin_ratio  =  ratio_marg))

    fig_title <- make_figure_title(tidied_screen, ...)

    screen_plots <-
      list("p_hits" = plot_subscreen(tidied_screen, dyes$hits, plot_title = "Hit dyes"),
           "p_sens" = plot_subscreen(tidied_screen, dyes$sensitivies, plot_title = "Sensitive dyes"),
           "p_nones"= plot_subscreen(tidied_screen, dyes$nones, plot_title = "Non-hitting, insensitive dyes"))

    p_assembled <-
      save_stacked_plots(.plot_list = screen_plots,
                          .figure_titles = fig_title,
                          .plot_heights = plot_heights,
                          save_fig = TRUE,
                          ...)
  }


#' Create and save dyes called as "hits" or "sensitives" in a dye screen
#'
#' Creates a plot object, and saves a figure, containing raw data for all screened dyes in all measured fluorescent channels. Results are divided into two subplots, containing dyes which are declared hits, and sensitives. Each dye is represented in a single row, with each column containing raw data collected for that dye in a given fluorescent channel. Both protein and buffer data are displayed.
#'
#'
#'#' This plot is closely related to the one created by the \code{save_stacked_screen()} function, but differs in that (1) only "hit" and "sensitive" dyes are displayed, and (2) each channel is visualized separately, making it easier to identify the channels in which that dye may be called a hit.
#'
#' This function is often accessed through it's wrapper function, \code{save_dye_screen_figs()}, though it can be used identically on it's own. See the  documentation of the \code{save_dye_screen_figs()} function for more information on what each of these four plots display.
#'
#'
#' @param tidied_screen a tibble containing raw data from a dye screen, both protein and buffer, e.g. as output by the \code{tidy_dye_screen()} function.
#' @param hits a tibble containing hit assignments for each dye. Must have columns "dye" and "assignment", described below:
#' \itemize{
#'   \item \strong{"dye"}, a character column containing the names of all of the dyes tested in the experiment. In order for a dye to be displayed in this plot, its name in this dataframe must match its name in the tidied_screen dataframe exactly. Matches are case- and punctuation-sensitive. (e.g. a dye named "L045" in this tibble will match only to a dye named "L045" in the tidied_screen tibble; it will not match to "l045", "L45", "L-045", etc.).
#'   \item \strong{"assignment"}, a character column containing the assignment of how a given dye behaved with screened protein. These assignments must be supplied by the user. Assignments can be determined either manually, or with assistance using the hit-calling tools in this package (in development as of August 2021). Possible assignments are: "hit", "sensitive", and "none", which correspond to the following judgements:
#'   \itemize{
#'   \item \strong{"hit"}, any dye which appears to detect the melting of the protein of interest, while having minimal temperature-sensitive fluorescence in the absence of protein.
#'   \item \strong{"sensitive"}, a less stringent category than 'hit'. Useful for dyes where definition as a "hit" isn't quite warranted, but which demonstrating some fluorescent response to the protein (over buffer) which could be potentially interesting. Common reasons for a a dye to receive a "senstive" assignment detection of non-cannonical transitions, and/or detecting a cannonical transtion, but doing so with far weaker signal than dyes called as 'hits'. In particular, the "sensitive" assignment is useful for flagging dyes which may be interesting for follow-up later, or fleshing out SAR of the hit dyes for a given protein.
#'   \item \strong{"none"}, an assignment given to dyes which showed no major difference between a DSF run with or without the protein, suggesting no fluorescent response to the protein of interest at any temperature.
#'   }
#'   }
#' @param ratio_marg a number, giving the ratio between the total size of a facet's plot panel and the their facet panel, inclusive of borders, axis ticks, etc. Ideally this could be reliably extracted from the plot object itself in a future version of this package. Is greater than one.
#' @param ... additional arguments, passed to the \code{make_figure_title()} helper function and the \code{save_stacked_plots()} helper function. See the documentation for the function \code{make_figure_title()} and \code{save_stacked_plots()} for possible options, which include assigning a directory to which to write the plot object created, and requesting the plot be saved using a specific  the graphics device  (default is "pdf" but all extentions compatible with \code{ggsave()} can be used).
#'
#' @return if assigned to an object, returns a ggproto object. Always saves a figure to the current working directory, or the directory specified through arguments to ...
#'
#' @export
save_stacked_hits <-
  function(tidied_screen,
           hits,
           ratio_marg = 1,
           ...){

    dyes <-
      list(hits = pull_assigned(hits, "hit"),
           sensitivies = pull_assigned(hits, "sensitive"))

    hit_plots <-
      list("p_hits" = plot_subscreen(tidied_screen,
                                     dyes$hits,
                                     plot_title = "Hit dyes",
                                     .facet_type = "grid"),

           "p_sens" = plot_subscreen(tidied_screen,
                                     dyes$sensitivies,
                                     plot_title = "Sensitive dyes",
                                     .facet_type = "grid"))
    hit_plot_heights <-
      c(convert_heights(dyes$hits,
                        .grid_margin_ratio = ratio_marg,
                        facet_type = "grid"),

        convert_heights(dyes$sensitivies,
                        facet_type = "grid",
                        .grid_margin_ratio  = ratio_marg))

    fig_title <- make_figure_title(tidied_screen)

    p_assembled_hits <-
      save_stacked_plots(.plot_list = hit_plots,
                          .figure_titles = fig_title,
                          .plot_heights = hit_plot_heights,
                          .save_suffix = "hits_by_channel",
                          .override_save_width = TRUE,
                          .manual_save_width = 6,
                          save_fig = TRUE,
                          ...)

  }

#' Create and save all raw data from the screen, before hit/sensitive assignment.
#'
#' Creates a plot object, and saves a figure, containing raw data for all screened dyes in all measured fluorescent channels. Results are divided into two subplots, containing dyes which are declared hits, and sensitives. Each dye is represented in a single row, with each column containing raw data collected for that dye in a given fluorescent channel. Both protein and buffer data are displayed.
#'
#' This plot is closely related to the one created by the \code{save_stacked_screen()} function, but differs in that dyes are not yet divided into "hit", "sensitive", and "none" categories. It is often used during the process of creating those assignments, and as an output of the default dye screening workflow, this plot serves as an unbiased representation of screening data, so that screens can be re-assessed with fresh eyes after hits have been assigned if desired.
#'
#' This function is often accessed through it's wrapper function, save_dye_screen_figs(), though it can be used identically on it's own. It is the third of the four standard raw data plots created in a default dye-screening workflow, where the other four plots are (1) all raw data, with no assignments displayed. Used for calling hits, as created by the \code{save_full_screen()} function; (2) all raw data, with distinct sub-plots containing dyes given each assignment--hit, sensitive, or none--as created by the \code{save_stacked_screen()} function; (3) raw data for just dyes assigned to "hit" and "sensitive" categories, with data collected for each dye displayed in a separate row, and data for the raw data collected for that dye in each fluorescent channel displayed in a separate colummn; (4) the hit and/or sensitive dyes, in specific channels, carried through model fitting and tma extraction.
#'
#'
#' @param tidied_screen a tibble containing raw data from a dye screen, both protein and buffer, e.g. as output by the \code{tidy_dye_screen()} function.
#' @param ratio_marg a number, giving the ratio between the total size of a facet's plot panel and the their facet panel, inclusive of borders, axis ticks, etc. Ideally this could be reliably extracted from the plot object itself in a future version of this package. Is greater than one.
#' @param ... additional arguments, passed to make_figure_title() and save_stacked_plots(). See make_figure_title() and save_stacked_plots() for possible options, which include assigning a directory to which to write the plot object created, and requesting the plot be saved using a specific  the graphics device  (default is "pdf" but all extentions compatible with ggsave() can be used).
#'
#' @return if assigned to an object, returns a ggproto object. Always saves a figure to the current working directory, or the directory specified through arguments to ...
#'
#' @export
save_full_screen <-
  function(tidied_screen,
           ratio_marg = 1,
           ...) {

    dyes <-  tidied_screen$dye %>% unique()

    hit_plots <-
      list("p_hits" = plot_subscreen(tidied_screen,
                                     dyes,
                                     plot_title = "Full dye screen",
                                     .facet_type = "wrap",
                                     ...))

    hit_plot_heights <-
      c(convert_heights(dyes,
                        .grid_margin_ratio = ratio_marg,
                        facet_type = "wrap",
                        ...))

    fig_title <- make_figure_title(tidied_screen, ...)

    p_assembled_screen <-
      save_stacked_plots(.plot_list = hit_plots,
                          .figure_titles = fig_title,
                          .plot_heights = hit_plot_heights,
                          .save_suffix = "full_dye_screen",
                          .override_save_width = TRUE,
                          .manual_save_width = 10,
                          save_fig = TRUE,
                          ...)

  }

#' Save dye screen plots with sensible dimensions and informative names.
#'
#'
#' @param .plot_list a list, containing plot objects created by ggplot2, in the order in which they will appear in the final figure. Typically, these are the output of the \code{plot_dye_screen()} function.
#' @param .figure_titles a names list of two elements, typically, but not necessarily, created using the \code{make_figure_title()} function.
#' \itemize{
#' \item \strong{print} a string, providing the title which will be added to the output figure.
#' \item \strong{save} a string, providing the name under which the output figure will be saved. A directory can be specified either in this name, or using the .save_path argument in this function.
#' }
#' @param .plot_heights a numeric vector, determining the heights of each of the plots provided in .plot_list, in the final figure.
#' @param .default_width a number, diving the width of the plots in the final figure. Defaults to 10. (unit: inches)
#' @param .default_title_height a number, giving the number of inches to be added to the final figure to accomodate the plot title.
#' @param .use_common_legend a boolean, determining whether a single legend should be displayed for the figure in cases where multiple plots in .plot_list share identical legends. Defaults to TRUE.
#' @param .common_legend_position a string, giving the position within the figure for the common legend to appear. Defaults to "top". Is passed directly to 'legend' argument of the \code{ggpubr::ggarrange()} function.
#' @param .override_fig_title a boolean, which, if TRUE, overrides the automatically-generated figure title with the string provided to the '.manual_fig_title' argument of this function. Defaults to FALSE.
#' @param .override_save_width a boolean, which, if TRUE, overrides the automatically-generated width of the saved figure with the number provided to the'.manual_save_width' argument of this function. Defaults to FALSE.
#' @param .override_save_height a boolean, which, if TRUE, overrides the automatically-generated height of the final saved figure, with the number provided to the'.manual_save_height' argument of this function. Defaults to FALSE.
#' @param .override_plot_heights a boolean, which, if TRUE, overrides the hieghts given in the .plot_heights argument to this function. Is useful for overriding .plot_heights values which may be passed automatically from upstream functions to this one in longer workflows. Replaces with the numeric vector provided to the'.manual_plot_heights' argument of this function. Defaults to FALSE.
#' @param save_fig a boolean which, if TRUE, results in the final plot generataed in this function being saved.
#' @param .save_path a string which, if present, is appended to the beginning of the save path, determining the directory into which the output plot will be saved. No trailing '/' necessary; e.g. to save into the directory which can be accessed from the current working directory via "results/plots/<figure_name>", provide "results/plots"
#' @param .save_suffix a string, giving a suffix to append to the name of the saved plot. e.g. "dye_screen_results" or "hits_by_channel". This string is the last element of the saved name to appear before the file extension, and the main way to differentiate different plots made for the same dye screen.
#' @param .graphics_device a string, giving the extension of the graphics device used to save the final plot using the \code{ggsave()} function. No period necessary; defaults to "pdf". Is pasted onto to the saved file name directly following the string provided to .save_suffix.
#' @param .manual_fig_title a string which, if .override_fig_title is set to TRUE, will appear as the title of the saved figure.
#' @param .manual_save_width a number which, if .override_save_width is set to TRUE, will be passed directly to the 'width' argument of the \code{ggsave()} function, determining the width of the saved figure.
#' @param .manual_save_height a number which, if .override_save_height is set to TRUE, will be passed directly to the 'height' argument of the \code{ggsave()} function, determining the height of the saved figure.
#' @param .manual_plot_heights a numeric vector with the same lengths as .plot_list, which, if .override_plot_heights is set to TRUE, will be passed directly to ggarrange to set the heights of each plot within the figure.
#' @param ... additional arguments, which are not passed to anything within this function at this time.
#'
#' @return a saved plot
#'
#' @importFrom ggpubr ggarrange annotate_figure
#' @export
save_stacked_plots <-
  function(.plot_list,
           .figure_titles,
           .plot_heights,

           .default_width = 10,
           .default_title_height = 2,

           .use_common_legend = TRUE,
           .common_legend_position = "top",
           .override_fig_title = FALSE,
           .override_save_width = FALSE,
           .override_save_height = FALSE,
           .override_plot_heights = FALSE,

           save_fig = FALSE,
           .save_path = "",
           .save_suffix = "dye_screen_results",
           .graphics_device = "pdf",
           .manual_fig_title = "Manual figure title",
           .manual_save_width = 10,
           .manual_save_height = 10,
           .manual_plot_heights = c(1,1,1),
           ...) {

    # calculate full figure dimensions
    plot_widths <- rep(.default_width, times = length(.plot_list))
    save_width <- .default_width

    # extract titles
    figure_title <- glue::glue("{.figure_titles$print} {.save_suffix}")
    if(nchar(.save_path) > 0) .save_path <- paste0(.save_path, "/")
    save_name <- glue::glue("{.save_path}{.figure_titles$save}_{.save_suffix}.{.graphics_device}")

    # handle user overrides
    if(.override_fig_title)    figure_title  <- .manual_fig_title
    if(.override_plot_heights) .plot_heights   <- .manual_plot_heights
    if(.override_save_width)   save_width   <- .manual_save_width
    if(.override_save_height)  figure_height <- .manual_save_height

    # caculate the full figure height
    figure_height <- sum(.plot_heights) + .default_title_height

    # arrange the plots
    p_arranged <-
      ggpubr::ggarrange(plotlist = .plot_list, # user input
                        heights =  .plot_heights, # user input
                        widths = plot_widths, # calculated here
                        ncol = 1,
                        nrow = length(.plot_list),

                        align = "v",
                        common.legend = .use_common_legend,
                        legend = .common_legend_position)  %>%

      ggpubr::annotate_figure(top =
                                ggpubr::text_grob(.figure_titles$print,
                                                  color = "black",
                                                  face = "bold",
                                                  size = 14))
    if(save_fig) {
      ggplot2::ggsave(save_name,
                      p_arranged,
                      width = save_width,
                      height = figure_height,
                      limitsize = FALSE)
    }

    out <- p_arranged

  }

#' Not exported: helper function to the \code{save_stacked_plots()} function.
#'
#' @param hits the tibble containing hit assignments. See the function documentation for \code{save_stacked_plots()} for more information.
#' @param assignment a string, giving the assignment for which dyes should be pulled.
#' @param .dye_col a string, giving the name of the column in \code{'hits'} containing dye names.
#' @param .assign_col a string, giving the name of the column in \code{'hits'} containin assignments.
#' @param ... additional arguments, passed from upstream functions. Passed to nothing; places here to enable the passing of named arguments in this function using ... from upstream functions, which ignoring arguments passed in the same ... which do not match anything here.
#'
#' @return a character vector containing the names of the dyes which matched to the provided assignment.
pull_assigned <-
  function(hits,
           assignment,
           .dye_col = "dye",
           .assign_col = "assignment",
           ...) {
    hits[[.dye_col]][hits[[.assign_col]] == assignment]
  }


#' Not exported: helper function for the \code{save_stacked_plots()} function.
#'
#' This is a brute-force way of getting around challenges of autoamtically determining the height of a full ggplot object. While we can set the panel heights precisely, I haven't figured out a good way to set all additional obejct sizes to allow perfect computation of output plot height (e.g. axis text, axis ticks, margins...). This is a simple arithmetic operation, but i pulled it into a helper function to allow more robust tuning of this converstion, and to simplify the code for the functions which use it.
#'
#' Note that there is at least one function get_dims from the package infotroph/DeLuciatoR, which does this calculation, but function relied on unstable side effects of certain functions. I didn't use it, because I don't want to take a dependency on something like that.
#'
#' @param .paneled_by a list or vector containing the variable by which the plot was faceted. used only for it's length.
#' @param .raw_panel_height the fored height of the panel. defaults to 0.4
#' @param facet_type the type of faceting applied to the plot, "wrap" or "grid", because this number is different for wraps anada grids.
#' @param .ncol_wide the number of columns in the facet, used to calculate the number of rows in the facet.
#' @param .wrap_margin_ratio a number, giving the ratio between the height of a full panel and the height of just it's forced plot area, for wrapped facets. Defaults to 190/110, but this may be a bit too large for standard applications.
#' @param .grid_margin_ratio a number, giving the ratio between the height of a full panel and the height of just it's forced plot area, for grid facets. Defaults to 190/110, but this may be a bit too large for standard applications.
#' @param .title_height_add a number, giving a constant amount to add to each plot to accomodate single-appearance items like titles and legends.
#' @param ... additional arguments, passed from upstream functions. Passed to nothing; places here to enable the passing of named arguments in this function using ... from upstream functions, which ignoring arguments passed in the same ... which do not match anything here.
#'
#' @return a number, giving the estimated height of a faceted plot created based on the variable defined in paneled-by
convert_heights <-
  function( .paneled_by,
            .raw_panel_height = 0.4,
            facet_type = "wrap",
            .ncol_wide = 10,
            .wrap_margin_ratio = 190/110,
            .grid_margin_ratio = 190/110,
            .title_height_add = 1,
            ...) {

    n_panels <-  length(unique(.paneled_by))

    n_rows <-
      switch(facet_type,
             "wrap" = ceiling(n_panels/.ncol_wide),
             "grid" = n_panels
      )

    margin_mult <-
      switch(facet_type,
             "wrap" = .wrap_margin_ratio,
             "grid" = .grid_margin_ratio)

    row_height <-  .raw_panel_height * margin_mult

    height <- n_rows * row_height + .title_height_add

  }

#' Automatically generate acceptable figure and plot titles from data
#'
#' A helper function of limited scope and flexibility. Given a dataframe, extracts values from specified columns and assembles them into titles and names. Exported only because it can be useful for general workflows as well.
#' Generates experiment-specific names, of the format: Experiment_number (linebreak) Protein: protein name (linebreak) Buffer: buffer name
#' Defaults to creating a title of the form:  And a name of the form:  <Experiment_number>protein_<protein_name>buffer_<buffer_name>
#'
#' @param df a df, e.g. one created by the \code{tidy_dye_screen()} function.
#' @param .exp_num_col a string, giving the name of the column whose value will appear first in the titles/names. Defaults to "exp_num", which, if the dataframe comes from the \code{tidy_dye_screen()} function, contains the experiment number, e.g. "Exp1100". Ideally, this column would contain only one unique value. If more than one value is found, the first is used.
#' @param .prot_name_col a string, giving the name of the column whose value will appear second in the titles/names. Defaults to "identity", which, if the dataframe comes from the \code{tidy_dye_screen()} function., contains the specific names of the protein and buffer corresponding to the experiment. This argument takes a dependence on the df also having a "type" column (the name of which can be supplied via .type_col), which can be used to filter to contain only the values "protein" or "buffer", to extract the name of the protein and the name of the buffer, as single unique strings, respectively.
#' @param .override_names a boolean, which, if TRUE, overrides the names extracted from ..exp_num_col and .prot_name_col within the function, with the strings provided to the following arguments of this function. Defaults to FALSE.
#' @param .manual_exp_num a string, which, if .override_names is set to TRUE, is used as the first element of both plot title and saved name.
#' @param .manual_prot_name a string, which, if .override_names is set to TRUE, is used as the second element of both plot title and saved name, where the name of the protein typically appears.
#' @param .manual_buffer_name a string, which, if .override_names is set to TRUE, is used as the third element of both plot title and saved name, where the name of the buffer typically appears.
#' @param .type_col the name of the column which contains values of either "protein" or "buffer", which can be used to filter the input dataframe such that a single, unique name can be pulled from the .prot_name_col column for the protein (when filtered such that .type_col == "protein), or buffer (when filtered such that .type_col == "buffer").
#'
#' @return a names list with two named elements: "print" -- the name to be printed at the head of the plot, and "save" -- the name to be used to save the plot via ggsave. Additional elements can be appended to the save name in the saving functions, to specify which figure is being saved when more than one figure is saved for the same experiment. Specific paths to save to are also set in the save_* plot functions of this package.
#'
#' @export
make_figure_title <-
  function(df,
           .exp_num_col = "exp_num",
           .prot_name_col = "identity",
           .override_names = FALSE,
           .manual_exp_num = Sys.Date(),
           .manual_prot_name = "screened protein",
           .manual_buffer_name = "screening buffer",
           .type_col = "type") {

    manual_print_title <- glue::glue("{.manual_exp_num} \n Protein: {.manual_prot_name} \n Buffer: {.manual_buffer_name}")
    manual_save_title <- glue::glue("{.manual_exp_num}--protein_{.manual_prot_name}__buffer_{.manual_buffer_name}")

    if(.override_names) {

      return(list("print" = manual_print_title,
                  "save" = manual_save_title))

    } else {
      tryCatch({

        expnum <-
          df[[.exp_num_col]] %>%
          unique()

        prot_name <-
          df %>%
          dplyr::filter(.data[[.type_col]] == "protein") %>%
          dplyr::pull(.prot_name_col) %>%
          unique() %>%
          .[[1]] # ensure length of 1

        buffer_name <-
          df %>%
          dplyr::filter(.data[[.type_col]] == "buffer") %>%
          dplyr::pull(.prot_name_col) %>%
          unique() %>%
          .[[1]]  # ensure length of 1

        plot_title <- glue::glue("{expnum} \n Protein: {prot_name} \n Buffer: {buffer_name}")
        save_title <- glue::glue("{expnum}--protein_{prot_name}__buffer_{buffer_name}")

        return(list("print" = plot_title,
                    "save" = save_title))
      },
      error = function(e){
        return(list("print" = manual_print_title,
                    "save" = manual_save_title))},

      warning = function(w) {
        return(list("print" = manual_print_title,
                    "save" = manual_save_title))})
    }


  }
