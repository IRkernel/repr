#' @include list.r
NULL

# repr_text is defined via print



# HTML --------------------------------------------------------------------


#' HTML representation of a vector
#' 
#' @export
repr_html.logical <- function(vec, ...) repr_sequence_generic(
	vec, 'html',
	'\t<li>%s</li>\n',
	'\t<dt>%s</dt>\n\t\t<dd>%s</dd>\n',
	'<ol class=vector>\n%s</ol>\n',
	'<dl class=vector>\n%s</dl>\n')

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
repr_markdown.logical <- function(vec, ...) repr_sequence_generic(
	vec, 'markdown',
	'%s. %s\n',
	'%s\n:   %s',
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
repr_latex.logical <- function(vec, ...) repr_sequence_generic(
	vec, 'latex',
	'\\item %s\n',
	'\\item[%s] %s\n',
	'\\begin{inparaenum}\n%s\\end{inparaenum}\n',
	'\\begin{inparadesc}\n%s\\end{inparadesc}\n',
	only.named.item = '\\textbf{%s:} %s')

#' @export @name repr_latex.logical
repr_latex.integer <- repr_latex.logical

#' @export @name repr_latex.logical
repr_latex.numeric <- repr_latex.logical

#' @export @name repr_latex.logical
repr_latex.factor <- repr_latex.logical

#' @export @name repr_latex.logical
repr_latex.character <- repr_latex.logical
