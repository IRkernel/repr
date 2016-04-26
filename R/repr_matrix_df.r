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

ellip.limit.arr <- function(
	a,
	rows = getOption('repr.matrix.max.rows'),
	cols = getOption('repr.matrix.max.cols')
) {
	stopifnot(rows >= 2L, cols >= 2L)

	# Don't worry about any of the code below if the array is already small.
	if (rows >= nrow(a) && cols >= ncol(a)) {
		return(a)
	}

	left   <- seq_len(ceiling(cols / 2))
	right  <- seq.int(ncol(a) - floor(cols / 2) + 1L, ncol(a))
	top    <- seq_len(ceiling(rows / 2))
	bottom <- seq.int(nrow(a) - floor(rows / 2) + 1L, nrow(a))
	
	# fix columns that won't like ellipsis being inserted
	if (is.data.frame(a)) {
		# data.tables can't be indexed by column number, unless you provide the
		# with=FALSE parameter. To avoid the hassle, just convert to a normal table.
		# dplyr's tbl_* objects don't collapse to a vector when indexed by column,
		# so functions like 'is.factor' always return false. Again, just drop to a
		# basic data.table and avoid the hassle.
		if (inherits(a, c('data.table', 'tbl'))) {
			a <- as.data.frame(a)
		}
		for (c in seq_len(ncol(a))) {
			if (is.factor(a[, c])) {
				# Factors: add ellipses to levels
				levels(a[, c]) <- c(levels(a[, c]), ellipses)
			} else if (inherits(a[, c], 'Date')) {
				# Dates: convert to plain strings
				a[, c] <- as.character(a[, c])
			}
		}
	}

	if (rows < nrow(a) && inherits(a, 'tbl')) {
		# tbl objects from dplyr automatically reset their row names when sliced.
		# we'd like to make it clear that the array has had rows cut out, so that
		# behavior isn't ideal here. If we're row-slicing a tbl object, first 
		# convert to an ordinary data.frame.
		a <- as.data.frame(a)
	}
	
	if (rows < nrow(a) && cols < ncol(a)) {
		if (is.matrix(a)) {
			# If a is a matrix, R will coerce the factor to character and sub in 
			# the factor *value*, not the level.  You end up with a bunch of 1s.
			ehf <- ellip.h
		} else {
			ehf <- factor(ellip.h, levels = ellipses)
		}
		rv <- rbind(
			cbind(a[   top, left], ehf, a[   top, right], deparse.level = 0),
			ellip.limit.vec(rep(ellip.v, ncol(a)), cols, ellip.d),
			cbind(a[bottom, left], ehf, a[bottom, right], deparse.level = 0),
			deparse.level = 0)
	} else if (rows < nrow(a) && cols >= ncol(a)) {
		rv <- rbind(a[top, , drop = FALSE], ellip.v, a[bottom, , drop = FALSE], deparse.level = 0)
	} else if (rows >= nrow(a) && cols < ncol(a)) {
		rv <- cbind(a[, left, drop = FALSE], ellip.h, a[, right, drop = FALSE], deparse.level = 0)
	}

	if (rows < nrow(a) && (! is.null(rownames(rv)))) {
		# If there were no rownames before, as is often true for matrices, don't assign them.
		rownames(rv)[[ top[[length(top) ]] + 1L]] <- ellip.v
	}
	if (cols < ncol(a) && (! is.null(colnames(rv)))) {
		# If there were no colnames before, as is often true for matrices, don't assign them.
		colnames(rv)[[left[[length(left)]] + 1L]] <- ellip.h
	}

	rv
}

# HTML --------------------------------------------------------------------

repr_matrix_generic <- function(
	x,
	wrap,
	header.wrap, corner, head,
	body.wrap, row.wrap, row.head,
	cell, last.cell = cell,
	escape.FUN = identity
) {
	has.rownames <- !is.null(rownames(x))
	has.colnames <- !is.null(colnames(x))
	
	x <- ellip.limit.arr(x)
	
	header <- ''
	if (has.colnames) {
		headers <- sprintf(head, escape.FUN(colnames(x)))
		if (has.rownames) headers <- c(corner, headers)
		header <- sprintf(header.wrap, paste(headers, collapse = ''))
	}
	
	rows <- lapply(seq_len(nrow(x)), function(r) {
		row <- escape.FUN(x[r, ])
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
	escape.FUN = html.escape)

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
		escape.FUN = latex.escape)

	#TODO: remove this quick’n’dirty post processing
	gsub(' &\\', '\\', r, fixed = TRUE)
}

#' @name repr_*.matrix/data.frame
#' @export
repr_latex.data.frame <- repr_latex.matrix
# Text -------------------------------------------------------------------



#' @name repr_*.matrix/data.frame
#' @export
repr_text.matrix <- function(obj, ...)
	paste(utils::capture.output(print(ellip.limit.arr(obj))), collapse = '\n')

#' @name repr_*.matrix/data.frame
#' @export
repr_text.data.frame <- repr_text.matrix
