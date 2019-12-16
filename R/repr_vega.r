#' Representation as \code{\link[vegalite]{vegalite}v2} or \code{vega4} JSON.
#' 
#' @param obj  The \link[vegalite]{vegalite} plot to create a representation for
#' @param ...  ignored
#' 
#' @name repr_vega*
NULL

#' @name repr_vega*
#' @export
repr_vegalite2.vegalite <- function(obj, ...) obj$x
