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

repr_ts_generic <- function(obj, repr_func, ...) {
	m <- .preformat.ts(obj)
	# set rows and cols so the whole thing is always displayed 
	repr_func(m, ..., rows = nrow(m), cols = ncol(m))
}

#' @name repr_*.ts
#' @export
repr_html.ts <- function(obj, ...) repr_ts_generic(obj, repr_html.matrix, ...)

#' @name repr_*.ts
#' @export
repr_latex.ts <- function(obj, ..., colspec = getOption('repr.matrix.latex.colspec'))
	repr_ts_generic(obj, repr_latex.matrix, ..., colspec = colspec)

#' @name repr_*.ts
#' @export
repr_markdown.ts <- function(obj, ...) repr_ts_generic(obj, repr_markdown.matrix, ...)

#' @name repr_*.ts
#' @export
repr_text.ts <- function(obj, ...) repr_ts_generic(obj, repr_text.matrix, ...)
