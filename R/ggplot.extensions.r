styles_linetypes <- c("solid", "longdash", "dotdash", "longdash")
styles_colors <- c("black", "blue", "red", "darkgreen")

#' Extension for ggplot2
#'
#' scale_discrete_manual doesn't allow to use multiple aesthetics in one functions call.
#' This methods overcomes this limitation by specifying key value pair of aesthetic name and it's discrete values
#'
#' @param ... key value pairs, where key is aesthetic name and value is a vector of values to use for this aesthetic
#' @param aesthetics vector of aesthetics names in case if dots are empty or if number of aesthetics must be limited
#' @param values named list of values for each aesthetic
#' @param name legend title
#'
#' @return list of discrete scales
#' @export
scale_discrete_manual_ext <- function(..., aesthetics = NULL, values = NULL, name)
{
  values <- values %??% list(...)
  aesthetics <- aesthetics %??% names(values)

  lapply(aesthetics, function(aesthetic) {
    ggplot2::scale_discrete_manual(aesthetic, values = values[[aesthetic]], name = name)
  })
}


#' Custom theme for IT infrastructure statistics
#'
#' @param legend_title legend title
#'
#' @return list of discrete scales and theme_bw
#' @export
theme_itinf <- function(legend_title)
{
  list(
    scale_discrete_manual_ext(linetype = styles_linetypes, color = styles_colors, name = legend_title),
    ggplot2::theme_bw()
  )
}

#' Plot IT infrastructure statistics
#'
#' @param data data to use
#' @param x x axis variable name
#' @param y y axis variable name
#' @param group_var grouping variable name
#' @param legend_title legend title
#'
#' @return plot
#' @export
plot_itin_statistics <- function(data, x = "time_period", y, group_var, legend_title)
{
  x <- ifelse(is.character(substitute(x)), rlang::sym(x), substitute(x))

  ggplot2::ggplot(data,
                  ggplot2::aes_(
                    x = ggplot2::enquo(x),
                    y = ggplot2::enquo(y),
                    color = ggplot2::enquo(group_var),
                    linetype = ggplot2::enquo(group_var))) +
    ggplot2::geom_line(size = 1.25) +
    ggplot2::geom_point() +
    theme_itinf(legend_title)
}


#' Helper function to fix cyrillic symbols rendering on plots
#'
#' Issues details: https://github.com/rstudio/rstudio/issues/2142
#'
#' @export
fix_cyrillic_rendering <- function()
{
  trace(grDevices::png, quote({
    if (missing(type) && missing(antialias)) {
      type <- "cairo-png"
      antialias <- "subpixel"
    }
  }), print = FALSE)
}
