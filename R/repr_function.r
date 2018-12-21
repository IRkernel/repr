#' Representations of functions
#' 
#' @param obj  Function to create a representation for
#' @param highlight  Should code highlighting be performed
#' @param fenced  Should a fenced code block instead of an indented one be used?
#' @param ...  ignored
#' 
#' @name repr_*.function
#' @include utils.r
NULL

repr_function_generic <- function(f, fmt, escape, high_wrap, norm_wrap, highlight) {
	code <- deparse(f)
	if (highlight) {
		if (!requireNamespace('highr'))
			stop(sprintf('Tried to create a %s representation of a function with highlighting, but the `highlight` package is not installed!', fmt))
		code <- highr::hilight(code, fmt)
		wrap <- high_wrap
	} else {
		code <- escape(code)
		wrap <- norm_wrap
	}
	sprintf(wrap, paste(code, collapse = '\n'))
}

#' @name repr_*.function
#' @export
repr_html.function <- function(obj, highlight = getOption('repr.function.highlight'), ...) {
	wrap <- '<pre class=language-r><code>%s</code></pre>'
	repr_function_generic(obj, 'html', html_escape, wrap, wrap, highlight, ...)
}

#' @name repr_*.function
#' @export
repr_latex.function <- function(obj, highlight = getOption('repr.function.highlight'), ...) {
	minted_wrap <- '\\begin{minted}{r}\n%s\n\\end{minted}'
	repr_function_generic(obj, 'latex', latex_escape, '%s', minted_wrap, highlight, ...)
}

#' @name repr_*.function
#' @export
repr_markdown.function <- function(obj, fenced = TRUE, ...) {
	code <- deparse(obj)
	if (fenced) {
		code <- c('```r', code, '```')
	} else {
		code <- paste0('\t', code)
	}
	paste(code, collapse = '\n')
}
