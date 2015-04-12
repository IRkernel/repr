repr_sequence_generic <- function(
	vec, fmt, enum.item, named.item, only.named.item, enum.wrap, named.wrap = enum.wrap,
	...,
	numeric.item = named.item,
	item.uses.numbers = FALSE) {
	
	nms <- names(vec)
	char.vec <- as.character(vec)
	
	if (length(char.vec) == 1) {
		if (is.null(nms))
			ret <- char.vec
		else
			ret <- sprintf(only.named.item, nms, char.vec)
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



#' HTML representation of a list
#' 
#' @export
repr_html.list <- function(li, ...) repr_sequence_generic(
	li, 'html',
	'\t<li>%s</li>\n',
	'\t<dt>$%s</dt>\n\t\t<dd>%s</dd>\n',
	'<strong>$%s</strong> = %s',
	'<ol>\n%s</ol>\n',
	'<dl>\n%s</dl>\n',
	numeric.item = '\t<dt>[[%s]]</dt>\n\t\t<dd>%s</dd>\n')



#' Markdown representation of a list
#' 
#' @export
repr_markdown.list <- function(vec, ...) repr_sequence_generic(
	vec, 'markdown',
	'%s. %s\n',
	'$%s\n:   %s\n',
	'**$%s** = %s',
	'%s\n\n',
	numeric.item = '[[%s]]\n:   %s\n',
	item.uses.numbers = TRUE)



#' LaTeX representation of a list
#' 
#' @export
repr_latex.list <- function(vec, ...) repr_sequence_generic(
	vec, 'latex',
	'\\item %s\n',
	'\\item[\\$%s] %s\n',
	'\\textbf{\\$%s} = %s',
	'\\begin{enumerate}\n%s\\end{enumerate}\n',
	'\\begin{description}\n%s\\end{description}\n',
	numeric.item = '\\item[{[[%s]]}] %s\n')
