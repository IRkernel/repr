#' Representations of vectors
#' 
#' @param obj  The vector to create a representation for
#' @param ...  ignored
#' 
#' @aliases
#' repr_markdown.logical repr_markdown.integer repr_markdown.numeric repr_markdown.factor repr_markdown.character
#'    repr_latex.logical    repr_latex.integer    repr_latex.numeric    repr_latex.factor    repr_latex.character
#'     repr_html.logical     repr_html.integer     repr_html.numeric     repr_html.factor     repr_html.character
#' @name repr_*.vector
#' @include repr_list.r
NULL

# repr_text is defined via print


repr_vector_generic <- function(
	vec, enum.item, named.item, only.named.item,
	enum.wrap, named.wrap = enum.wrap,
	...,
	numeric.item = named.item,
	item.uses.numbers = FALSE) {
	
	nms <- names(vec)
	if (is.character(vec) && getOption('repr.vector.quote'))
		char.vec <- shQuote(vec)
	else
		char.vec <- as.character(vec)
	
	if (length(char.vec) == 1) {
		if (is.null(nms)) {
			ret <- char.vec
		} else {
			ret <- sprintf(only.named.item, nms, char.vec)
		}
	} else {
		if (is.null(nms)) {
			if (item.uses.numbers)
				entries <- sprintf(enum.item, seq_along(char.vec), char.vec)
			else
				entries <- sprintf(enum.item, char.vec)
		} else {
			entries <- vapply(seq_along(char.vec), function(i) {
				nm <- nms[[i]]
				if (is.na(nm) || nchar(nm) == 0) {
					sprintf(numeric.item, i, char.vec[[i]])
				} else {
					sprintf(named.item, nms[[i]], char.vec[[i]])
				}
			}, character(1))
		}
		
		wrap <- if (is.null(nms)) enum.wrap else named.wrap
		
		ret <- sprintf(wrap, paste0(entries, collapse = ''))
	}
	ret
}



# HTML --------------------------------------------------------------------


#' @export @name repr_*.vector
repr_html.logical <- function(obj, ...) repr_vector_generic(
	obj,
	'\t<li>%s</li>\n',
	'\t<dt>%s</dt>\n\t\t<dd>%s</dd>\n',
	'<strong>%s:</strong> %s',
	'<ol class=list-inline>\n%s</ol>\n',
	'<dl class=dl-horizontal>\n%s</dl>\n')

#' @export @name repr_*.vector
repr_html.integer <- repr_html.logical

#' @export @name repr_*.vector
repr_html.numeric <- repr_html.logical

#' @export @name repr_*.vector
repr_html.factor <- repr_html.logical

#' @export @name repr_*.vector
repr_html.character <- repr_html.logical



# Markdown ----------------------------------------------------------------




#' @export @name repr_*.vector
repr_markdown.logical <- function(obj, ...) repr_vector_generic(
	obj,
	'%s. %s\n',
	'%s\n:   %s',
	'**%s:** %s',
	'%s\n\n',
	item.uses.numbers = TRUE)

#' @export @name repr_*.vector
repr_markdown.integer <- repr_markdown.logical

#' @export @name repr_*.vector
repr_markdown.numeric <- repr_markdown.logical

#' @export @name repr_*.vector
repr_markdown.factor <- repr_markdown.logical

#' @export @name repr_*.vector
repr_markdown.character <- repr_markdown.logical




# LaTeX -------------------------------------------------------------------



#' @export @name repr_*.vector
repr_latex.logical <- function(obj, ...) repr_vector_generic(
	obj,
	'\\item %s\n',
	'\\item[%s] %s\n',
	'\\textbf{%s:} %s',
	enum.wrap  = '\\begin{enumerate*}\n%s\\end{enumerate*}\n',
	named.wrap = '\\begin{description*}\n%s\\end{description*}\n',
	only.named.item = '\\textbf{%s:} %s')

#' @export @name repr_*.vector
repr_latex.integer <- repr_latex.logical

#' @export @name repr_*.vector
repr_latex.numeric <- repr_latex.logical

#' @export @name repr_*.vector
repr_latex.factor <- repr_latex.logical

#' @export @name repr_*.vector
repr_latex.character <- repr_latex.logical
