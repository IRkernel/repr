#' HTML widget representations
#'
#' Standalone HTML representation and dummy text representation
#'
#' @param obj  The htmlwidget to create a representation for
#' @param ...  ignored
#'
#' @name repr_*.htmlwidget
#' @export
repr_text.htmlwidget <- function(obj, ...) 'HTML widgets cannot be represented in plain text (need html)'

#' @name repr_*.htmlwidget
#' @export
repr_html.htmlwidget <- function(obj, ...) {
	if (!requireNamespace('htmlwidgets', quietly = TRUE))
		stop('repr_html.htmlwidget called without loadable htmlwidgets')
	
	htmlfile <- tempfile(fileext = '.html')
	on.exit(unlink(htmlfile))
	
	htmlwidgets::saveWidget(obj, htmlfile)
	
	readChar(htmlfile, file.info(htmlfile)$size)
}


#' @name repr_*.htmlwidget
#' @export
repr_text.shiny.tag.list <- function(obj, ...) sprintf(
	'Use HTML to display this shiny-taglist of length %s with named elements %s',
	length(obj), paste(lapply(obj, function(t) dQuote(t$elementId)), collapse = '\n'))

#' @name repr_*.htmlwidget
#' @export
repr_html.shiny.tag.list <- function(obj, ...) {
	paste(lapply(obj, repr_html), collapse = '\n')
}
