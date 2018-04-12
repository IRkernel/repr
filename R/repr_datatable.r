#' Representation of data.table objects
#' 
#' @param obj  The list to create a representation for
#' @param ...  ignored
#' 
#' @name repr_*.data.table
#' @export
repr_html.data.table <- function(obj, ...) {
	if (data.table::shouldPrint(obj))
		NextMethod()
}

#' @name repr_*.data.table
#' @export
repr_text.data.table <- function(obj, ...) {
	if (data.table::shouldPrint(obj))
		NextMethod()
}

#' @name repr_*.data.table
#' @export
repr_latex.data.table <- function(obj, ...) {
	if (data.table::shouldPrint(obj))
		NextMethod()
}
