#' @include utils.r
repr_function_generic <- function(f, fmt, escape, high.wrap, norm.wrap, highlight) {
	code <- deparse(f)
	if (highlight) {
		if (!requireNamespace('highr'))
			stop(sprintf('Tried to create a %s representation of a function with highlighting, but the `highlight` package is not installed!', fmt))
		code <- highr::hilight(code, fmt)
		wrap <- high.wrap
	} else {
		code <- escape(code)
		wrap <- norm.wrap
	}
	sprintf(wrap, paste(code, collapse = '\n'))
}

#' HTML representation of a function
#' 
#' @export
repr_html.function <- function(f, highlight = getOption('repr.function.highlight'), ...) {
	wrap <- '<pre class=language-r><code>%s</code></pre>'
	repr_function_generic(f, 'html', html.escape, wrap, wrap, highlight, ...)
}

#' LaTeX representation of a function
#' 
#' @export
repr_latex.function <- function(f, highlight = getOption('repr.function.highlight'), ...) {
	minted.wrap <- '\\begin{minted}{r}\n%s\n\\end{minted}'
	repr_function_generic(f, 'latex', latex.escape, '%s', minted.wrap, highlight, ...)
}

#' Markdown representation of a function
#' 
#' @export
repr_markdown.function <- function(f, fenced = TRUE, ...) {
	code <- deparse(f)
	if (fenced) {
		code <- c('```r', code, '```')
	} else {
		code <- paste0('\t', code)
	}
	paste(code, collapse = '\n')
}