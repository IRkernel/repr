#' packageIQR representations
#' 
#' Text representations of packageIQR objects like the list of available example data or vignettes
#' 
#' @param obj  The packageIQR obj to create a representation for
#' @param ...  ignored
#' 
#' @examples
#' repr_html(data(package = 'base'))
#' repr_text(vignette(package = 'highr'))
#' 
#' @name repr_*.packageIQR
#' @export
repr_text.packageIQR <- function(obj, ...) {
	# this is mostly copied from utils:::print.packageIQR
	db <- as.data.frame(obj$results, stringsAsFactors = FALSE)
	idx_by_pkg <- split(seq_len(nrow(db)), db$Package)
	db_by_pkg <- lapply(idx_by_pkg, function(ind) db[ind, ])
	
	output <- character(0L)
	
	for (pkg_name in names(db_by_pkg)) {
		package <- db_by_pkg[[pkg_name]]
		output <- c(
			output,
			sprintf('%s in package %s:\n', obj$title, sQuote(pkg_name)),
			formatDL(package$Item, package$Title))
	}
	
	if (length(db_by_pkg) == 0L)
		output <- c(output, sprintf('no %s found', tolower(obj$title)))
	
	if (!is.null(obj$footer))
		output <- c(output, paste0('\n', obj$footer))  # add 2 \n
	
	paste(output, collapse = '\n')
}

#' @name repr_*.packageIQR
#' @export
repr_html.packageIQR <- function(obj, ...) {
	db <- as.data.frame(obj$results, stringsAsFactors = FALSE)[c('Package', 'Item', 'Title')]
	title <- sprintf('<h3>%s</h3>', obj$title)
	content <- if (nrow(db) == 0L) {
		sprintf('<p>No %s found</p>', tolower(obj$title))
	} else {
		repr_html(db, rows = 1000L)
	}
	footer <- sprintf('<p>%s</p>', obj$footer)  # will be character(0L) if is.null(footer)
	paste(title, content, footer, sep = '\n')
}
