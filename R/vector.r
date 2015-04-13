#' @include list.r
NULL

# repr_text is defined via print


repr_vector_generic <- function(
	vec, fmt, enum.item, named.item, only.named.item,
	enum.wrap, named.wrap = enum.wrap,
	...,
	numeric.item = named.item,
	item.uses.numbers = FALSE) {
	
	nms <- names(vec)
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
	structure(ret, class = 'repr', repr.format = fmt)
}



# HTML --------------------------------------------------------------------


#' HTML representation of a vector
#' 
#' @export
repr_html.logical <- function(vec, ...) repr_vector_generic(
	vec, 'html',
	'\t<li>%s</li>\n',
	'\t<dt>%s</dt>\n\t\t<dd>%s</dd>\n',
	'<strong>%s:</strong> %s',
	'<ol class=list-inline>\n%s</ol>\n',
	'<dl class=list-inline>\n%s</dl>\n')

#' @export @name repr_html.logical
repr_html.integer <- repr_html.logical

#' @export @name repr_html.logical
repr_html.numeric <- repr_html.logical

#' @export @name repr_html.logical
repr_html.factor <- repr_html.logical

#' @export @name repr_html.logical
repr_html.character <- repr_html.logical



# Markdown ----------------------------------------------------------------




#' Markdown representation of a vector
#' 
#' @export
repr_markdown.logical <- function(vec, ...) repr_vector_generic(
	vec, 'markdown',
	'%s. %s\n',
	'%s\n:   %s',
	'**%s:** %s',
	'%s\n\n',
	item.uses.numbers = TRUE)

#' @export @name repr_markdown.logical
repr_markdown.integer <- repr_markdown.logical

#' @export @name repr_markdown.logical
repr_markdown.numeric <- repr_markdown.logical

#' @export @name repr_markdown.logical
repr_markdown.factor <- repr_markdown.logical

#' @export @name repr_markdown.logical
repr_markdown.character <- repr_markdown.logical




# LaTeX -------------------------------------------------------------------



#' LaTeX representation of a vector
#' 
#' @export
repr_latex.logical <- function(vec, ...) repr_vector_generic(
	vec, 'latex',
	'\\item %s\n',
	'\\item[%s] %s\n',
	'\\textbf{%s:} %s',
	enum.wrap  = '\\begin{inparaenum}\n%s\\end{inparaenum}\n',
	named.wrap = '\\begin{inparadesc}\n%s\\end{inparadesc}\n',
	only.named.item = '\\textbf{%s:} %s')

#' @export @name repr_latex.logical
repr_latex.integer <- repr_latex.logical

#' @export @name repr_latex.logical
repr_latex.numeric <- repr_latex.logical

#' @export @name repr_latex.logical
repr_latex.factor <- repr_latex.logical

#' @export @name repr_latex.logical
repr_latex.character <- repr_latex.logical
