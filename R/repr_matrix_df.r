#' Tabular data representations
#' 
#' HTML and LaTeX representations of Matrix-like objects
#' 
#' @param obj  The matrix or data.frame to create a representation for
#' @param ...  ignored
#' @param colspec  The colspec for the LaTeX table. The default is given by the option \code{repr.matrix.latex.colspec}
#' 
#' @seealso \link{repr-options} for \code{repr.matrix.latex.colspec}
#' 
#' @aliases repr_html.matrix repr_html.data.frame repr_latex.matrix repr_latex.data.frame
#' @name repr_*.matrix/data.frame
NULL

ellip.h <- '\u22EF'
ellip.v <- '\u22EE'
ellip.d <- '\u22F1'

ellipses <- c(ellip.h, ellip.v, ellip.d)

ellip.limit.vec <- function(v, num, ellip) {
	stopifnot(num >= 2L)
	
	left  <- seq_len(ceiling(num / 2))
	right <- seq.int(length(v) - floor(num / 2) + 1L, length(v))
	
	# fix factors not having the appropriate levels
	if (is.factor(v)) {
		levels(v) <- c(levels(v), ellipses)
	}
	
	c(v[left], ellip, v[right])
}

ellip.limit.arr <- function(
	a,
	rows = getOption('repr.matrix.max.rows'),
	cols = getOption('repr.matrix.max.cols')
) {
	stopifnot(rows >= 2L, cols >= 2L)
	
	left    <- seq_len(ceiling(cols / 2))
	right   <- seq.int(ncol(a) - floor(cols / 2) + 1L, ncol(a))
	top     <- seq_len(ceiling(rows / 2))
	bottom  <- seq.int(nrow(a) - floor(rows / 2) + 1L, nrow(a))
	
	# fix factors not having the appropriate levels
	if (is.data.frame(a)) {
		for (name in names(a)) {
			if (is.factor(a[, name])) {
				levels(a[, name]) <- c(levels(a[, name]), ellipses)
			}
		}
	}
	
	if (rows >= nrow(a) && cols >= ncol(a)) {
		a
	} else if (rows < nrow(a) && cols < ncol(a)) {
		rbind(
			cbind(a[   top, left], ellip.h, a[   top, right], deparse.level = 0),
			ellip.limit.vec(rep(ellip.v, ncol(a)), cols, ellip.d),
			cbind(a[bottom, left], ellip.h, a[bottom, right], deparse.level = 0),
			deparse.level = 0)
	} else if (rows < nrow(a) && cols >= ncol(a)) {
		rbind(a[top, , drop = FALSE], ellip.v, a[bottom, , drop = FALSE], deparse.level = 0)
	} else if (rows >= nrow(a) && cols < ncol(a)) {
		cbind(a[, left, drop = FALSE], ellip.h, a[, right, drop = FALSE], deparse.level = 0)
	}
}

# HTML --------------------------------------------------------------------

repr_matrix_generic <- function(
	x,
	wrap,
	header.wrap, corner, head,
	body.wrap, row.wrap, row.head,
	cell, last.cell = cell
) {
	has.rownames <- !is.null(rownames(x))
	has.colnames <- !is.null(colnames(x))
	
	x <- ellip.limit.arr(x)
	
	header <- ''
	if (has.colnames) {
		headers <- sprintf(head, colnames(x))
		if (has.rownames) headers <- c(corner, headers)
		header <- sprintf(header.wrap, paste(headers, collapse = ''))
	}
	
	rows <- lapply(seq_len(nrow(x)), function(r) {
		row <- x[r, ]
		cells <- sprintf(cell, format(row))
		if (has.rownames) {
			row.head <- sprintf(row.head, rownames(x)[[r]])
			cells <- c(row.head, cells)
		}
		sprintf(row.wrap, paste(cells, collapse = ''))
	})
	
	body <- sprintf(body.wrap, paste(rows, collapse = ''))
	
	sprintf(wrap, header, body)
}


#' @name repr_*.matrix/data.frame
#' @export
repr_html.matrix <- function(obj, ...) repr_matrix_generic(
	obj,
	'<table>\n%s%s</table>\n',
	'<thead><tr>%s</tr></thead>\n', '<th></th>',
	'<th scope=col>%s</th>',
	'<tbody>\n%s</tbody>\n', '\t<tr>%s</tr>\n', '<th scope=row>%s</th>',
	'<td>%s</td>')

#' @name repr_*.matrix/data.frame
#' @export
repr_html.data.frame <- repr_html.matrix



# LaTeX -------------------------------------------------------------------



#' @name repr_*.matrix/data.frame
#' @export
repr_latex.matrix <- function(obj, ..., colspec = getOption('repr.matrix.latex.colspec')) {
	cols <- paste0(paste(rep(colspec$col, ncol(obj)), collapse = ''), colspec$end)
	if (!is.null(rownames(obj)))
		cols <- paste0(colspec$row.head, cols)
	
	r <- repr_matrix_generic(
		obj,
		sprintf('\\begin{tabular}{%s}\n%%s%%s\\end{tabular}\n', cols),
		'%s\\\\\n\\hline\n', '  &', ' %s &',
		'%s', '\t%s\\\\\n', '%s &',
		' %s &')
	
	#todo: remove this quick’n’dirty post processing
	gsub(' &\\\\', '\\\\', r)
}

#' @name repr_*.matrix/data.frame
#' @export
repr_latex.data.frame <- repr_latex.matrix
