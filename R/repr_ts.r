#' Time series representations
#' 
#' HTML, LaTeX, and Markdown representations of \link[stats]{ts} objects.
#' 
#' @param obj  The \link[stats]{ts} object to create a representation for
#' @param ...  ignored
#' @param colspec  The colspec for the LaTeX table. The default is given by the option \code{repr.matrix.latex.colspec}
#' 
#' @seealso \link{repr-options} for \code{repr.matrix.latex.colspec}
#' 
#' @importFrom stats ts .preformat.ts
#' @name repr_*.ts
NULL

repr_ts_generic <- function(obj, repr_func, wrap, ...) {
	vec <- .preformat.ts(obj)
	if (is.matrix(vec)) {
		# set rows and cols so the whole thing is always displayed
		rows <- max(nrow(vec), 2L)
		cols <- max(ncol(vec), 2L)
		repr_func(vec, ..., rows = rows, cols = cols, caption_override = 'Time Series')
	} else {  # Just a vector
		sprintf(wrap, 'A Time Series', repr_func(vec, ...))
	}
}

#' @name repr_*.ts
#' @export
repr_html.ts <- function(obj, ...) repr_ts_generic(obj, repr_html, '%s:<br>%s', ...)

#' @name repr_*.ts
#' @export
repr_latex.ts <- function(obj, ..., colspec = getOption('repr.matrix.latex.colspec'))
	repr_ts_generic(obj, repr_latex, '%s:\\\\%s', ..., colspec = colspec)

#' @name repr_*.ts
#' @export
repr_markdown.ts <- function(obj, ...) repr_ts_generic(obj, repr_markdown, '%s:  \n%s', ...)

#' @name repr_*.ts
#' @export
repr_text.ts <- function(obj, ...) repr_ts_generic(obj, repr_text, '%s:\n%s', ...)
