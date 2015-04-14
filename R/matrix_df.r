# HTML --------------------------------------------------------------------

repr_matrix_generic <- function(
	x, fmt,
	wrap,
	header.wrap, corner, head,
	body.wrap, row.wrap, row.head,
	cell, last.cell = cell
) {
	has.rownames <- !is.null(rownames(x))
	has.colnames <- !is.null(colnames(x))
	
	header <- ''
	if (has.colnames) {
		headers <- sprintf(head, colnames(x))
		if (has.rownames) headers <- c(corner, headers)
		header <- sprintf(header.wrap, paste(headers, collapse = ''))
	}
	
	rows <- lapply(seq_len(nrow(x)), function(r) {
		row <- x[r, ]
		cells <- sprintf(cell, lapply(row, format))
		if (has.rownames) {
			row.head <- sprintf(row.head, rownames(x)[[r]])
			cells <- c(row.head, cells)
		}
		sprintf(row.wrap, paste(cells, collapse = ''))
	})
	
	body <- sprintf(body.wrap, paste(rows, collapse = ''))
	
	structure(sprintf(wrap, header, body), class = 'repr', repr.format = fmt)
}


#' HTML Representation of a Matrix-like object
#' 
#' @export
repr_html.matrix <- function(x, ...) repr_matrix_generic(
		x, 'html',
		'<table>\n%s%s</table>\n',
		'<thead><tr>%s</tr></thead>\n', '<th></th>',
		'<th scope=col>%s</th>',
		'<tbody>\n%s</tbody>\n', '\t<tr>%s</tr>\n', '<th scope=row>%s</th>',
		'<td>%s</td>')

#' @name repr_html.matrix
#' @export
repr_html.data.frame <- repr_html.matrix



# LaTeX -------------------------------------------------------------------



#' LaTeX Representation of a Matrix-like object
#' 
#' @export
repr_latex.matrix <- function(x, ...) {
	colspec <- getOption('repr.matrix.latex.colspec')
	
	cols <- paste0(paste(rep(colspec$col, ncol(x)), collapse = ''), colspec$end)
	if (!is.null(rownames(x)))
		cols <- paste0(colspec$row.head, cols)
	
	r <- repr_matrix_generic(
		x, 'latex',
		sprintf('\\begin{tabular}{%s}\n%%s%%s\\end{tabular}\n', cols),
		'%s\\\\\n\\hline\n', '  &', ' %s &',
		'%s', '\t%s\\\\\n', '%s &',
		' %s &')
	
	#todo: remove this quick’n’dirty post processing
	gsub(' &\\\\', '\\\\', r)
}

#' @name repr_html.matrix
#' @export
repr_latex.data.frame <- repr_latex.matrix