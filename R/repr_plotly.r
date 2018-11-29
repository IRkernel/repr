#' Representation as \link[plotly:plotly_json]{Plotly JSON}.
#' 
#' @param obj  The \link[plotly]{plot_ly} plot or \link[ggplot2]{ggplot} to create a representation for
#' @param ...  ignored
#' 
#' @name repr_plotly1.*
NULL

#' @name repr_plotly1.*
#' @export
repr_plotly1.plotly <- function(obj, ...) jsonlite::fromJSON(plotly::plotly_json(obj, jsonedit = FALSE))

#' @name repr_plotly1.*
#' @export
repr_plotly1.gg <- repr_plotly1.plotly
