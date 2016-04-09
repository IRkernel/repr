#' Representations of lists
#' 
#' @param obj  The list to create a representation for
#' @param ...  ignored
#' 
#' @aliases repr_html.list repr_markdown.list repr_latex.list
#' @name repr_*.list
#' @include utils.r
NULL

repr_list_generic <- function(
	vec, fmt,
	enum.item, named.item, only.named.item,
	enum.wrap, named.wrap = enum.wrap,
	...,
	numeric.item = named.item,
	item.uses.numbers = FALSE,
	escape.FUN = identity) {
	
	nms <- names(vec)
	if (!is.null(nms)) {
		nms <- as.character(sapply(nms, as.name, USE.NAMES = FALSE))  # adds `` around special chars
		nms <- escape.FUN(nms)
	}
	
	# This does escaping, so no need to escape the content again
	mapped <- lapply(vec, format2repr[[fmt]])
	
	if (length(mapped) == 1 && !is.null(nms)) {
		ret <- sprintf(only.named.item, nms, mapped[[1]])
	} else {
		if (is.null(nms)) {
			if (item.uses.numbers)
				entries <- sprintf(enum.item, seq_along(mapped), mapped)
			else
				entries <- sprintf(enum.item, mapped)
		} else {
			entries <- vapply(seq_along(mapped), function(i) {
				nm <- nms[[i]]
				if (is.na(nm) || nchar(nm) == 0) {
					sprintf(numeric.item, i, mapped[[i]])
				} else {
					sprintf(named.item, nms[[i]], mapped[[i]])
				}
			}, character(1))
		}
		
		wrap <- if (is.null(nms)) enum.wrap else named.wrap
		
		ret <- sprintf(wrap, paste0(entries, collapse = ''))
	}
	ret
}



#' @name repr_*.list
#' @export
repr_html.list <- function(obj, ...) repr_list_generic(
	obj, 'html',
	'\t<li>%s</li>\n',
	'\t<dt>$%s</dt>\n\t\t<dd>%s</dd>\n',
	'<strong>$%s</strong> = %s',
	'<ol>\n%s</ol>\n',
	'<dl>\n%s</dl>\n',
	numeric.item = '\t<dt>[[%s]]</dt>\n\t\t<dd>%s</dd>\n',
	escape.FUN = html.escape)



#' @name repr_*.list
#' @export
repr_markdown.list <- function(obj, ...) repr_list_generic(
	obj, 'markdown',
	'%s. %s\n',
	'$%s\n:   %s\n',
	'**$%s** = %s',
	'%s\n\n',
	numeric.item = '[[%s]]\n:   %s\n',
	item.uses.numbers = TRUE,
	escape.FUN = html.escape)



#' @name repr_*.list
#' @export
repr_latex.list <- function(obj, ...) repr_list_generic(
	obj, 'latex',
	'\\item %s\n',
	'\\item[\\$%s] %s\n',
	'\\textbf{\\$%s} = %s',
	enum.wrap  = '\\begin{enumerate}\n%s\\end{enumerate}\n',
	named.wrap = '\\begin{description}\n%s\\end{description}\n',
	numeric.item = '\\item[{[[%s]]}] %s\n',
	escape.FUN = latex.escape)
