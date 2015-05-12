#' Dynamic representation
#' 
#' Specify an object and a format to represent it in. Will \link{stop}\code{()} if no such format is known.
#' 
#' @param obj  The object to create a representation for
#' @param format  The representation format. \code{repr_<format>} is then called. (default: Call \link{repr_text})
#' @param ...  delegated to the specific \code{repr_<format>} function
#' 
#' @return A character or raw vector of that format or NULL if none is defined.
#' Only the \code{'text'} format is defined for everything (via \link{print}\code{()})
#' 
#' @seealso \link{repr_text}, \link{repr-generics}
#' @export
repr <- function(obj, format = 'text', ...) {
	delegate <- format2repr[[format]]
	if (is.null(delegate)) stop(sprintf('Repr format %s not known', format))
	delegate(obj, ...)
}

#' Text representation
#' 
#' The only representation defined per default for everthing (via \link{print}\code{()})
#' 
#' @param obj  The object to \link{print} and then return the output
#' @param ...  ignored
#' 
#' @seealso \link{repr-generics} for other generics
#' @export
repr_text <- function (obj, ...) UseMethod('repr_text', obj)
#' @export @name repr_text
repr_text.default <- function(obj, ...) {
	paste(capture.output(print(obj)), collapse = '\n')
}

#' Representations for specific formats
#' 
#' @param obj  The object to create a repr for
#' @param ...  parameters of the specific \code{repr_*} functions
#' 
#' @seealso \link{repr_text} for the only repr that is always defined
#' @aliases
#' repr_html repr_html.default
#' repr_markdown repr_markdown.default
#' repr_latex repr_latex.default
#' repr_json repr_json.default
#' repr_javascript repr_javascript.default
#' repr_pdf repr_pdf.default
#' repr_png repr_png.default
#' repr_jpg repr_jpg.default
#' repr_svg repr_svg.default
#' @name repr-generics
NULL


#' @export @name repr-generics
repr_html <- function (obj, ...) UseMethod('repr_html', obj)
#' @export @name repr-generics
repr_html.default <- function(obj, ...) NULL


#' @export @name repr-generics
repr_markdown <- function (obj, ...) UseMethod('repr_markdown', obj)
#' @export @name repr-generics
repr_markdown.default <- function(obj, ...) NULL


#' @export @name repr-generics
repr_latex <- function (obj, ...) UseMethod('repr_latex', obj)
#' @export @name repr-generics
repr_latex.default <- function(obj, ...) NULL


#' @export @name repr-generics
repr_json <- function (obj, ...) UseMethod('repr_json', obj)
#' @export @name repr-generics
repr_json.default <- function(obj, ...) NULL


#' @export @name repr-generics
repr_javascript <- function (obj, ...) UseMethod('repr_javascript', obj)
#' @export @name repr-generics
repr_javascript.default <- function(obj, ...) NULL


#' @export @name repr-generics
repr_pdf <- function (obj, ...) UseMethod('repr_pdf', obj)
#' @export @name repr-generics
repr_pdf.default <- function(obj, ...) NULL


#' @export @name repr-generics
repr_png <- function (obj, ...) UseMethod('repr_png', obj)
#' @export @name repr-generics
repr_png.default <- function(obj, ...) NULL


#' @export @name repr-generics
repr_jpg <- function (obj, ...) UseMethod('repr_jpg', obj)
#' @export @name repr-generics
repr_jpg.default <- function(obj, ...) NULL


#' @export @name repr-generics
repr_svg <- function (obj, ...) UseMethod('repr_svg', obj)
#' @export @name repr-generics
repr_svg.default <- function(obj, ...) NULL


#' Lists mapping mime types (\code{mime2repr}) or format names (\code{format2repr}) to \code{repr} functions
#' 
#' @aliases mime2repr format2repr
#' @export @name *2repr
mime2repr <- list(
	'text/plain' = repr_text,
	'text/html' = repr_html,
	'text/markdown' = repr_markdown,
	'text/latex' = repr_latex,
	'application/json' = repr_json,
	'application/javascript' = repr_javascript,
	'application/pdf' = repr_pdf,
	'image/png' = repr_png,
	'image/jpeg' = repr_jpg,
	'image/svg+xml' = repr_svg)

#' @export @name *2repr
format2repr <- sapply(
	c('text', 'html', 'markdown', 'latex', 'json', 'javascript', 'pdf', 'png', 'jpg', 'svg'),
	function(n) get(paste0('repr_', n)))
