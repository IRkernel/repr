#' Representation as \link[plotly:plotly_json]{Plotly JSON}.
#' 
#' @param obj  The \link[plotly]{plot_ly} plot or \link[ggplot2]{ggplot} to create a representation for
#' @param ...  ignored
#' 
#' @name repr_plotly1.*
NULL

#' @name repr_plotly1.*
#' @export
repr_plotly1.plotly <- function(obj, ...) unclass(plotly::plotly_json(obj, jsonedit = FALSE))

#' @name repr_plotly1.*
#' @export
repr_plotly1.gg <- function(obj, ...) repr_plotly1(plotly::ggplotly(obj), ...)
