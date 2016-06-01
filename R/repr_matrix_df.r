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
#' @include utils.r
NULL

# There is currently a problem on windows which can't display chars in th
# text/plain output, which are not available in the current locale.
# See https://github.com/IRkernel/repr/issues/28#issuecomment-208574856
.char_fallback <- function(char, default) {
  real_len <- nchar(char)
  r_len <- nchar(utils::capture.output(cat(char)))
  if (real_len == r_len) char else default
}
ellip.h <- .char_fallback('\u22EF', '...')
ellip.v <- .char_fallback('\u22EE', '...')
ellip.d <- .char_fallback('\u22F1', '')

# These are used for factor, so make sure they are unique
ellipses <- unique(c(ellip.h, ellip.v, ellip.d))

get.limit.index <- function(obj_dim, limit) {
	stopifnot(obj_dim > limit)  # otherwise this function should not have been run
	left_or_top <- seq_len(ceiling(limit / 2))
	right_or_bottom <- seq.int(obj_dim - floor(limit / 2) + 1L, obj_dim)
	list(begin = left_or_top, end = right_or_bottom)
}

ellip.limit.vec <- function(v, num, ellip) {
	stopifnot(num >= 2L)

	lims <- get.limit.index(length(v), num)
	# fix factors not having the appropriate levels
	if (is.factor(v)) {
		levels(v) <- c(levels(v), ellipses)
	}
	
	c(v[lims$begin], ellip, v[lims$end])
}

# returns a character array with optionally a section of columns and rows in the middle replaced by ellipses
ellip.limit.arr <- function(
	a,
	rows = getOption('repr.matrix.max.rows'),
	cols = getOption('repr.matrix.max.cols')
) {
	stopifnot(rows >= 2L, cols >= 2L)
	
	many_rows <- rows < nrow(a)
	many_cols <- cols < ncol(a)
	
	# create sequences of indices to bisect rows and columns
	if (many_rows) {
		upper <- seq_len(ceiling(rows / 2))
		lower <- seq.int(nrow(a) - floor(rows / 2) + 1L, nrow(a))
	}
	if (many_cols) {
		left  <- seq_len(ceiling(cols / 2))
		right <- seq.int(ncol(a) - floor(cols / 2) + 1L, ncol(a))
	}
	
	# assign a list of parts that can be coerced to strings
	omit <- if (many_rows && many_cols) {
		parts <- list(
			ul = a[upper, left], ur = a[upper, right],
			ll = a[lower, left], lr = a[lower, right])
		'both'
	} else if (many_rows) {
		parts <- list(
			upper = a[upper, , drop = FALSE],
			lower = a[lower, , drop = FALSE])
		'rows'
	} else if (many_cols) {
		parts <- list(
			left  = a[, left,  drop = FALSE],
			right = a[, right, drop = FALSE])
		'cols'
	} else {
		parts <- list(full = a)
		'none'
	} 
	
	# coerce to formatted character matrices; rowwise or colwise
	f_parts <- lapply(parts, function(part) {
		f_part <- if (is.data.frame(part)) {
			vapply(part, format, character(nrow(part)))
		} else {
			# format(part) would work, but e.g. would left-pad *both* rows of matrix(7:10, 2L) instead of one
			apply(part, 2L, format)
		}
		# vapply returns a vector for 1-column dfs
		dim(f_part) <- dim(part)
		dimnames(f_part) <- dimnames(part)
		f_part
	})
	
	# stitch together parts to get a single formatted character matrix
	f_mat <- switch(omit,
		rows = rbind(f_parts$upper, ellip.v, f_parts$lower, deparse.level = 0L),
		cols = cbind(f_parts$left,  ellip.h, f_parts$right, deparse.level = 0L),
		none = f_parts$full,
		both = rbind(
			cbind(f_parts$ul, ellip.h, f_parts$ur, deparse.level = 0L),
			ellip.limit.vec(rep(ellip.v, cols + 1L), cols, ellip.d),
			cbind(f_parts$ll, ellip.h, f_parts$lr, deparse.level = 0L)))
	
	# If there were no dimnames before, as is often true for matrices, don't assign them.
	if (many_rows && !is.null(rownames(a))) {
		rownames(f_mat)[[length(upper) + 1L]] <- ellip.v
		# fix rownames for tbls, which explicitly set them to 1:n when subsetting
		rownames(f_mat)[seq.int(length(upper) + 2L, nrow(f_mat))] <- lower
	}
	if (many_cols && !is.null(colnames(a))) {
		colnames(f_mat)[[length(left)  + 1L]] <- ellip.h
	}

	f_mat
}

# HTML --------------------------------------------------------------------

repr_matrix_generic <- function(
	x,
	wrap,
	header.wrap, corner, head,
	body.wrap, row.wrap, row.head,
	cell, last.cell = cell,
	escape.FUN = identity,
	...,
	rows = getOption('repr.matrix.max.rows'),
	cols = getOption('repr.matrix.max.cols')
) {
	has.rownames <- !is.null(rownames(x))
	has.colnames <- !is.null(colnames(x))
	
	x <- ellip.limit.arr(x, rows, cols)
	
	header <- ''
	if (has.colnames) {
		headers <- sprintf(head, escape.FUN(colnames(x)))
		if (has.rownames) headers <- c(corner, headers)
		header <- sprintf(header.wrap, paste(headers, collapse = ''))
	}
	
	rows <- lapply(seq_len(nrow(x)), function(r) {
		row <- escape.FUN(slice.row(x, r))
		cells <- sprintf(cell, format(row))
		if (has.rownames) {
			row.head <- sprintf(row.head, escape.FUN(rownames(x)[[r]]))
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
	'<td>%s</td>',
	escape.FUN = html.escape.vec,
	...)

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
		' %s &',
		escape.FUN = latex.escape.vec,
		...)

	#TODO: remove this quick’n’dirty post processing
	gsub(' &\\', '\\', r, fixed = TRUE)
}

#' @name repr_*.matrix/data.frame
#' @export
repr_latex.data.frame <- repr_latex.matrix
# Text -------------------------------------------------------------------



#' @name repr_*.matrix/data.frame
#' @export
repr_text.matrix <- function(obj, ...) {
	if (inherits(obj, c('tbl', 'data.table'))) {
		# Coerce to data.frame to avoid special printing in dplyr and data.table.
		obj <- as.data.frame(obj)
	}
	limited_obj <- ellip.limit.arr(obj)
	print_output <- utils::capture.output(print(limited_obj, quote = FALSE))
	paste(print_output, collapse = '\n')
}

#' @name repr_*.matrix/data.frame
#' @export
repr_text.data.frame <- repr_text.matrix
