#' HTML widget representations
#' 
#' Standalone HTML representation and dummy text representation
#' 
#' @name repr_*.htmlwidget
#' @export
repr_text.htmlwidget <- function(obj, ...) 'You need to use repr_html for HTML widgets'

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
