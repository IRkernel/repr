html_specials <- list('&' = '&amp;', '<' = '&lt;', '>' = '&gt;')

html_escape <- function(text, do_spaces = TRUE) {
	for (chr in names(html_specials)) {
		text <- gsub(chr, html_specials[[chr]], text, fixed = TRUE)
	}
	if (do_spaces) {
		consec_spaces <- grepl('  ', text)
		text[consec_spaces] <- sprintf('<span style=white-space:pre-wrap>%s</span>', 	text[consec_spaces])
	}
	text
}

latex_specials <- list(
	'\\' = '\\textbackslash{}',
	'{'  = '\\{',
	'}'  = '\\}',
	'$'  = '\\$',
	'^'  = '\\textasciicircum{}',
	'_'  = '\\_',
	'%'  = '\\%',
	'#'  = '\\#',
	'&'  = '\\&',
	'~'  = '\\textasciitilde{}',
	'['  = '{[}',
	']'  = '{]}',
	'|'  = '\\textbar{}')

latex_escape <- function(text) {
	for (chr in names(latex_specials)) {
		text <- gsub(chr, latex_specials[[chr]], text, fixed = TRUE)
	}
	# undo superfluous escape
	gsub('\\textbackslash\\{\\}', '\\textbackslash{}', text, fixed = TRUE)
}


markdown_escape <- function(values) {
	# TODO also replace Markdown
	values <- html_escape(values, do_spaces = FALSE)
	values <- ifelse(grepl('^\\s*$', values), '<!---->', values)
	values
}

.escape_names <- function(obj, escape_type) {
	# Generic function for escaping names.
	# Depending on the object type, names, rownames and colnames may be the same or different
	# Capture all three before changing them.
	# Note that the resulting names may not be valid R syntax.
	# escape_type must be 'latex' or 'html'
	stopifnot(any(escape_type == c('html', 'latex')))

	obj_names <- names(obj)
	obj_rownames <- rownames(obj)
	obj_colnames <- if (length(dim(obj_names)) > 1L) colnames(obj) else NULL

	detect_specials <- match.fun(paste0('any_', escape_type, '_specials'))
	escape_specials <- match.fun(paste0(escape_type, '_escape'))

	if (detect_specials(obj_names))
		names(obj) <- escape_specials(obj_names)
	if (detect_specials(obj_rownames))
		rownames(obj) <- escape_specials(obj_rownames)
	if (length(dim(obj_names)) > 1L && detect_specials(obj_colnames))
		colnames(obj) <- escape_specials(obj_colnames)

	obj
}

.any_specials <- function(char_vec, specials_list) {
	# Use this function to avoid setting names unnecessarily (and thereby copying the object many times).
	if (inherits(char_vec, c('character', 'factor')))
		any(vapply(names(specials_list), grepl, logical(length(char_vec)), char_vec, fixed = TRUE))
	else
		FALSE
}

.escape_vec <- function(vec, escape_type = c('html', 'latex')) {
	escape_type <- match.arg(escape_type)
	# .escape_vec should never change the class of its input.
	# That seems useful, since functions like ellip_limit_arr check class.
	if (!is.vector(vec) && !is.factor(vec)) {
	    stop('expected `vec` to be a vector or factor but it is a ', paste(class(vec), collapse = ', '))
	}

	detect_specials <- match.fun(paste0('any_', escape_type, '_specials'))
	escape_specials <- match.fun(paste0(escape_type, '_escape'))

	if (detect_specials(vec)) {
		if (is.factor(vec)) {
			levels(vec) <- escape_specials(levels(vec))
		} else {
			vec <- escape_specials(vec) # regular character vec
		}
	}

	vec
}

slice_row <- function(df, row) {
	# Slice an array, kind of like unlist(obj[row, ]), but respecting factors and
	# upcasting as necessary.
	slice <- c(df[row, ], recursive = TRUE)
	col_classes <- vapply(df, class, FUN.VALUE = character(1L))
	
	for (col_idx in which(col_classes == 'factor')) {
		# This syntax doesn't work with matrices, but factor matrices are close to
		# impossible.  See: http://stackoverflow.com/a/28724756
		slice[col_idx] <- levels(df[[col_idx]])[df[[row, col_idx]]]
	}
	
	slice
}

strindent <- function(string, indent = '\t') {
	stripped <- gsub('\n$', '', string)
	gsub('\n', paste0('\n', indent), paste0(indent, stripped))
}

as_name_or_na <- function(x) {
	if (is.na(x)) '<NA>'
	else if (is.character(x) && nchar(x) == 0) NA_character_
	else as.name(x)
}
		

# Create the actually-used functions from the shells above.
latex_escape_names <- function(obj) .escape_names(obj, 'latex')
html_escape_names  <- function(obj) .escape_names(obj, 'html')
any_latex_specials <- function(char_vec) .any_specials(char_vec, latex_specials)
any_html_specials  <- function(char_vec) .any_specials(char_vec, html_specials)
latex_escape_vec <- function(vec) .escape_vec(vec, 'latex')
html_escape_vec  <- function(vec) .escape_vec(vec, 'html')

#' @importFrom base64enc dataURI
data_uris <- function(..., mime = '', encoding = 'base64', files) {
	stopifnot(length(list(...)) == 0L)
	vapply(
		files,
		function(f) dataURI(mime = mime, encoding = encoding, file = f),
		character(1L))
}


has_row_names <- function(x) {
	rns <- rownames(x)
	length(dim(x)) != 1 && length(rns) > 0 && !all(rns == seq_len(nrow(x))) && !all(rns == '')
}


flatten <- function(x){
	if (!is.data.frame(x)) return(x)
	dfcolumns <- rle(vapply(x, is.data.frame, logical(1L)))
	if (!any(dfcolumns$values)) return(x)
	
	end <- cumsum(dfcolumns$lengths)
	start <- end - dfcolumns$lengths + 1
	
	parts <- mapply(function(start, end, do_flatten) {
		if (do_flatten) flatten(x[, start:end]) else x[, start:end]
	}, start = start, end = end, do_flatten = dfcolumns$values, SIMPLIFY = FALSE)
	
	names(parts) <- colnames(x)
	do.call(cbind, parts)
}
