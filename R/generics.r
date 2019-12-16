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
repr_text <- function(obj, ...) UseMethod('repr_text', obj)
#' @name repr_text
#' @importFrom utils capture.output
#' @export
repr_text.default <- function(obj, ...) {
	paste(capture.output(print(obj)), collapse = '\n')
}

#' Representations for specific formats
#' 
#' @param obj  The object to create a repr for
#' @param ...  parameters of the specific \code{repr_*} functions
#' 
#' @seealso \link{repr_text} for the only repr that is always defined
#' @name repr-generics
NULL


#' @name repr-generics
#' @export
repr_html <- function(obj, ...) UseMethod('repr_html', obj)
#' @name repr-generics
#' @export
repr_html.default <- function(obj, ...) NULL


#' @name repr-generics
#' @export
repr_markdown <- function(obj, ...) UseMethod('repr_markdown', obj)
#' @name repr-generics
#' @export
repr_markdown.default <- function(obj, ...) NULL


#' @name repr-generics
#' @export
repr_latex <- function(obj, ...) UseMethod('repr_latex', obj)
#' @name repr-generics
#' @export
repr_latex.default <- function(obj, ...) NULL


#' @name repr-generics
#' @export
repr_json <- function(obj, ...) UseMethod('repr_json', obj)
#' @name repr-generics
#' @export
repr_json.default <- function(obj, ...) NULL


#' @name repr-generics
#' @export
repr_javascript <- function(obj, ...) UseMethod('repr_javascript', obj)
#' @name repr-generics
#' @export
repr_javascript.default <- function(obj, ...) NULL


#' @name repr-generics
#' @export
repr_pdf <- function(obj, ...) UseMethod('repr_pdf', obj)
#' @name repr-generics
#' @export
repr_pdf.default <- function(obj, ...) NULL


#' @name repr-generics
#' @export
repr_png <- function(obj, ...) UseMethod('repr_png', obj)
#' @name repr-generics
#' @export
repr_png.default <- function(obj, ...) NULL


#' @name repr-generics
#' @export
repr_jpg <- function(obj, ...) UseMethod('repr_jpg', obj)
#' @name repr-generics
#' @export
repr_jpg.default <- function(obj, ...) NULL


#' @name repr-generics
#' @export
repr_svg <- function(obj, ...) UseMethod('repr_svg', obj)
#' @name repr-generics
#' @export
repr_svg.default <- function(obj, ...) NULL


# jupyterlab also handles those less-common mimetypes


#' @name repr-generics
#' @export
repr_geojson <- function(obj, ...) UseMethod('repr_geojson', obj)
#' @name repr-generics
#' @export
repr_geojson.default <- function(obj, ...) NULL


#' @name repr-generics
#' @export
repr_vdom1 <- function(obj, ...) UseMethod('repr_vdom1', obj)
#' @name repr-generics
#' @export
repr_vdom1.default <- function(obj, ...) NULL


#' @name repr-generics
#' @export
repr_plotly1 <- function(obj, ...) UseMethod('repr_plotly1', obj)
#' @name repr-generics
#' @export
repr_plotly1.default <- function(obj, ...) NULL


#' @name repr-generics
#' @export
repr_vegalite2 <- function(obj, ...) UseMethod('repr_vegalite2', obj)
#' @name repr-generics
#' @export
repr_vegalite2.default <- function(obj, ...) NULL


#' @name repr-generics
#' @export
repr_vegalite3 <- function(obj, ...) UseMethod('repr_vegalite3', obj)
#' @name repr-generics
#' @export
repr_vegalite3.default <- function(obj, ...) NULL


#' @name repr-generics
#' @export
repr_vegalite4 <- function(obj, ...) UseMethod('repr_vegalite4', obj)
#' @name repr-generics
#' @export
repr_vegalite4.default <- function(obj, ...) NULL


#' @name repr-generics
#' @export
repr_vega4 <- function(obj, ...) UseMethod('repr_vega4', obj)
#' @name repr-generics
#' @export
repr_vega4.default <- function(obj, ...) NULL


#' @name repr-generics
#' @export
repr_vega5 <- function(obj, ...) UseMethod('repr_vega5', obj)
#' @name repr-generics
#' @export
repr_vega5.default <- function(obj, ...) NULL


#' Lists mapping mime types (\code{mime2repr}) or format names (\code{format2repr}) to \code{repr} functions
#' 
#' @format Lists mapping mime/name to function
#' 
#' @examples
#' names(mime2repr)
#' names(format2repr)
#' 
#' @name *2repr
#' @export
mime2repr <- list(
	'text/plain' = repr_text,
	'text/html' = repr_html,
	'text/markdown' = repr_markdown,
	'text/latex' = repr_latex,
	'application/javascript' = repr_javascript,
	'application/json' = repr_json,
	'application/geo+json' = repr_geojson,
	'application/vdom.v1+json' = repr_vdom1,
	'application/vnd.plotly.v1+json' = repr_plotly1,
	'application/vnd.vegalite.v2+json' = repr_vegalite2,
	'application/vnd.vegalite.v3+json' = repr_vegalite3,
	'application/vnd.vegalite.v4+json' = repr_vegalite4,
	'application/vnd.vega.v4+json' = repr_vega4,
	'application/vnd.vega.v5+json' = repr_vega5,
	'application/pdf' = repr_pdf,
	'image/png' = repr_png,
	'image/jpeg' = repr_jpg,
	'image/svg+xml' = repr_svg)

#' @name *2repr
#' @export
format2repr <- sapply(
	c('text', 'html', 'markdown', 'latex', 'javascript', 'json', 'geojson', 'vdom1', 'plotly1', paste0('vegalite', 2:4), paste0('vega', 4:5), 'pdf', 'png', 'jpg', 'svg'),
	function(n) get(paste0('repr_', n)))
