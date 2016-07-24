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
	enum_item, named_item, only_named_item,
	enum_wrap, named_wrap = enum_wrap,
	...,
	numeric_item = named_item,
	item_uses_numbers = FALSE,
	escape_fun = identity) {
	
	nms <- names(vec)
	if (!is.null(nms)) {
		nms <- as.character(sapply(nms, as.name, USE.NAMES = FALSE))  # adds `` around special chars
		nms <- escape_fun(nms)
	}
	
	# This does escaping, so no need to escape the content again
	mapped <- lapply(vec, format2repr[[fmt]])
	
	# if any elements cannot be represented, return NULL
	if (any(vapply(vec, is.null, logical(1)) != vapply(mapped, is.null, logical(1)))) {
		NULL
	} else if (length(mapped) == 1 && !is.null(nms)) {
		sprintf(only_named_item, nms, mapped[[1]])
	} else {
		entries <- 
			if (!is.null(nms)) {
				vapply(seq_along(mapped), function(i) {
					nm <- nms[[i]]
					if (is.na(nm) || nchar(nm) == 0) {
						sprintf(numeric_item, i, mapped[[i]])
					} else {
						sprintf(named_item, nms[[i]], mapped[[i]])
					}
				}, character(1))
			} else if (item_uses_numbers) {
				sprintf(enum_item, seq_along(mapped), mapped)
			} else {
				sprintf(enum_item, mapped)
			}
		
		wrap <- if (is.null(nms)) enum_wrap else named_wrap
		
		sprintf(wrap, paste0(entries, collapse = ''))
	}
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
	numeric_item = '\t<dt>[[%s]]</dt>\n\t\t<dd>%s</dd>\n',
	escape_fun = html_escape)



#' @name repr_*.list
#' @export
repr_markdown.list <- function(obj, ...) repr_list_generic(
	obj, 'markdown',
	'%s. %s\n',
	'$%s\n:   %s\n',
	'**$%s** = %s',
	'%s\n\n',
	numeric_item = '[[%s]]\n:   %s\n',
	item_uses_numbers = TRUE,
	escape_fun = html_escape)



#' @name repr_*.list
#' @export
repr_latex.list <- function(obj, ...) repr_list_generic(
	obj, 'latex',
	'\\item %s\n',
	'\\item[\\$%s] %s\n',
	'\\textbf{\\$%s} = %s',
	enum_wrap  = '\\begin{enumerate}\n%s\\end{enumerate}\n',
	named_wrap = '\\begin{description}\n%s\\end{description}\n',
	numeric_item = '\\item[{[[%s]]}] %s\n',
	escape_fun = latex_escape)
