#' Representations of vectors
#' 
#' @param obj  The vector to create a representation for
#' @param ...  ignored
#' 
#' @aliases
#' repr_markdown.logical repr_markdown.integer repr_markdown.complex repr_markdown.numeric repr_markdown.factor repr_markdown.character repr_markdown.Date
#'    repr_latex.logical    repr_latex.integer    repr_latex.complex    repr_latex.numeric    repr_latex.factor    repr_latex.character    repr_latex.Date
#'     repr_html.logical     repr_html.integer     repr_html.complex     repr_html.numeric     repr_html.factor     repr_html.character     repr_html.Date
#' @name repr_*.vector
#' @include repr_list.r
#' @include utils.r
NULL

# repr_text is defined via print


repr_vector_generic <- function(
	vec, enum_item, named_item, only_named_item,
	enum_wrap, named_wrap = enum_wrap,
	...,
	numeric_item = named_item,
	individual_wrap = NULL, # will be passed the vector items twice so needs 2 times %s
	item_uses_numbers = FALSE, escape_fun = identity) {
	
	if (length(vec) == 0) return('')
	
	nms <- names(vec)
	if (!is.null(nms))
		nms <- escape_fun(nms)
	
	qt <- is.character(vec) && getOption('repr.vector.quote')
	# excape_fun is output format specific, encodeString ensures that non-printables come out as \-escapes
	char_vec <- escape_fun(encodeString(as.character(vec), quote = if (qt) "'" else ''))
	
	if (!is.null(individual_wrap)) {
		char_vec <- sprintf(individual_wrap, char_vec, char_vec)
	}
	
	if (length(char_vec) > 1) {
		entries <-
			if (!is.null(nms))
				vapply(seq_along(char_vec), function(i) {
					nm <- nms[[i]]
					if (is.na(nm) || nchar(nm) == 0) {
						sprintf(numeric_item, i, char_vec[[i]])
					} else {
						sprintf(named_item, nms[[i]], char_vec[[i]])
					}
				}, character(1))
			else if (item_uses_numbers)
				sprintf(enum_item, seq_along(char_vec), char_vec)
			else
				sprintf(enum_item, char_vec)
		
		wrap <- if (is.null(nms)) enum_wrap else named_wrap
		
		sprintf(wrap, paste0(entries, collapse = ''))
	} else if (is.null(nms)) {
		char_vec
	} else {
		sprintf(only_named_item, nms, char_vec)
	}
}



# HTML --------------------------------------------------------------------


repr_html_wrapper <- function(obj, individual_wrap, ...) repr_vector_generic(
	obj,
	'\t<li>%s</li>\n',
	'\t<dt>%s</dt>\n\t\t<dd>%s</dd>\n',
	'<strong>%s:</strong> %s',
	'<ol class=list-inline>\n%s</ol>\n',
	'<dl class=dl-horizontal>\n%s</dl>\n',
	escape_fun = html_escape,
	individual_wrap = individual_wrap)


#' @name repr_*.vector
#' @export
repr_html.logical <- function(obj, ...) repr_html_wrapper(obj, NULL, ...)

#' @name repr_*.vector
#' @export
repr_html.integer <- repr_html.logical

#' @name repr_*.vector
#' @export
repr_html.complex <- repr_html.logical

#' @name repr_*.vector
#' @export
repr_html.numeric <- repr_html.logical

#' @name repr_*.vector
#' @export
repr_html.factor <- repr_html.logical

#' @name repr_*.vector
#' @export
repr_html.character <- repr_html.logical

#' @name repr_*.vector
#' @export
repr_html.Date <- function(obj, ...) repr_html_wrapper(obj, '<time datetime="%s">%s</time>', ...)




# Markdown ----------------------------------------------------------------




#' @name repr_*.vector
#' @export
repr_markdown.logical <- function(obj, ...) repr_vector_generic(
	html_escape_names(obj),
	'%s. %s\n',
	'%s\n:   %s',
	'**%s:** %s',
	'%s\n\n',
	item_uses_numbers = TRUE,
	escape_fun = html_escape)

#' @name repr_*.vector
#' @export
repr_markdown.integer <- repr_markdown.logical

#' @name repr_*.vector
#' @export
repr_markdown.complex <- repr_markdown.logical

#' @name repr_*.vector
#' @export
repr_markdown.numeric <- repr_markdown.logical

#' @name repr_*.vector
#' @export
repr_markdown.factor <- repr_markdown.logical

#' @name repr_*.vector
#' @export
repr_markdown.character <- repr_markdown.logical

#' @name repr_*.vector
#' @export
repr_markdown.Date <- repr_markdown.logical




# LaTeX -------------------------------------------------------------------



#' @name repr_*.vector
#' @export
repr_latex.logical <- function(obj, ...) repr_vector_generic(
	latex_escape_names(obj),  # escape vector names, regardless of class
	'\\item %s\n',
	'\\item[%s] %s\n',
	'\\textbf{%s:} %s',
	enum_wrap  = '\\begin{enumerate*}\n%s\\end{enumerate*}\n',
	named_wrap = '\\begin{description*}\n%s\\end{description*}\n',
	only_named_item = '\\textbf{%s:} %s',
	escape_fun = latex_escape)

#' @name repr_*.vector
#' @export
repr_latex.integer <- repr_latex.logical

#' @name repr_*.vector
#' @export
repr_latex.complex <- repr_latex.logical

#' @name repr_*.vector
#' @export
repr_latex.numeric <- repr_latex.logical

#' @name repr_*.vector
#' @export
repr_latex.factor <- repr_latex.logical

#' @name repr_*.vector
#' @export
repr_latex.character <- repr_latex.logical

#' @name repr_*.vector
#' @export
repr_latex.Date <- repr_latex.logical
