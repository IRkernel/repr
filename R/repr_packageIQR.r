#' packageIQR representations
#'
#' plain text representations of packageIQR objects like the list of vailable example
#' data or vignettes
#'
#' @param obj  The packageIQR obj to create a representation for
#' @param ...  ignored
#'
#' @name repr_*.packageIQR
#' @export
repr_text.packageIQR <- function(obj, ...)
{
	# this is mostly copied from utils:::print.packageIQR
	db <- obj$results
	out <- if (nrow(db) > 0L) {
		lapply(split(seq_len(nrow(db)), db[, 'Package']),
					 function(ind) {
					 	db[ind, c('Item', 'Title'), drop = FALSE]
					 })
	}
	output <- vector('character')
	outConn = textConnection('output', 'w', local = TRUE)
	
	first <- TRUE
	
	for (pkg in names(out)) {
		writeLines(paste0(
			ifelse(first, '', '\n'),
			obj$title,
			' in package ',
			sQuote(pkg),
			':\n'
		),
		outConn)
		writeLines(formatDL(out[[pkg]][, 'Item'], out[[pkg]][, 'Title']), outConn)
		first <- FALSE
	}
	#  print(first)
	if (first) {
		writeLines(paste('no', tolower(obj$title), 'found'), outConn)
		if (!is.null(obj$footer))
			writeLines(c('', obj$footer), outConn)
	}
	else {
		if (!is.null(obj$footer))
			writeLines(c('\n', obj$footer), outConn)
	}
	close(outConn)
	paste(output, collapse = '\n')
}

#' @name repr_*.packageIQR
#' @export
repr_html.packageIQR <- function(obj, ...)
{
	db <-
		as.data.frame(obj$results, stringsAsFactors = FALSE)[c('Package', 'Item', 'Title')]
	title = sprintf('<h3>%s</h3>', obj$title)
	if (nrow(db) == 0) {
		content = sprintf('<p>No %s found</p>', tolower(obj$title))
	} else {
		oldrows = getOption('repr.matrix.max.rows')
		options(repr.matrix.max.rows = 1000)
		on.exit({
			options(repr.matrix.max.rows = oldrows)
		})
		content <- repr_html(db)
	}
	footer <- ''
	if (!is.null(obj$footer)) {
		footer <- sprintf('<p>%s</p>', obj$footer)
	}
	paste(title, content, footer, sep = '\n')
}
