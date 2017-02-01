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



# sort of like htmlwidgets::saveWidget(), but for HTML tags
save_html <- function(obj, ...) {
	
	if (!requireNamespace('htmlwidgets')) {
		stop('Please install the htmlwidgets package')
	}
	if (!getFromNamespace("pandoc_available", asNamespace("htmlwidgets"))()) {
		stop("Printing HTML tags requires pandoc. For details see:\n", 
				 "https://github.com/rstudio/rmarkdown/blob/master/PANDOC.md")
	}
	
	htmlfile <- tempfile(fileext = '.html')
	libdir <- file.path(dirname(htmlfile), "lib")
	htmltools::save_html(obj, file = htmlfile, libdir = libdir)
	
	# TODO: it would be nice if we didn't have to rely on pandoc 
	# https://github.com/rstudio/htmltools/issues/73
	getFromNamespace("pandoc_self_contained_html", asNamespace("htmlwidgets"))(htmlfile, htmlfile)
	
	readChar(htmlfile, file.info(htmlfile)$size)
}

#' HTML tag lists
#'
#' Standalone HTML representation and dummy text representation
#'
#' @param obj  The tag list to create a representation for
#' @param ...  ignored
#'
#' @name repr_*.shiny.tag.list
#' @export
repr_text.shiny.tag.list <- function(obj, ...) 'HTML tag lists cannot be represented in plain text (need html)'

#' HTML tags
#'
#' Standalone HTML representation and dummy text representation
#'
#' @param obj  The tag list to create a representation for
#' @param ...  ignored
#'
#' @name repr_*.shiny.tag
#' @export
repr_text.shiny.tag <- function(obj, ...) 'HTML tags cannot be represented in plain text (need html)'
	
#' @name repr_*.shiny.tag.list
#' @export
repr_html.shiny.tag.list <- save_html

#' @name repr_*.shiny.tag
#' @export
repr_html.shiny.tag <- save_html
