html_specials <- list('&' = '&amp;', '<' = '&lt;', '>' = '&gt;')

html_escape <- function(text) {
	for (chr in names(html_specials)) {
		text <- gsub(chr, html_specials[[chr]], text, fixed = TRUE)
	}
	consec_spaces <- grepl('  ', text)
	text[consec_spaces] <- sprintf('<span style=white-space:pre-wrap>%s</span>', 	text[consec_spaces])
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

.escape_names <- function(obj, escape_type) {
	# Generic function for escaping names.
	# Depending on the object type, names, rownames and colnames may be the same or different
	# Capture all three before changing them.
	# Note that the resulting names may not be valid R syntax.
	# escape_type must be 'latex' or 'html'
	stopifnot(any(escape_type == c('html', 'latex')))

	obj_names <- names(obj)
	obj_rownames <- rownames(obj)
	obj_colnames <- colnames(obj)

	detect_specials <- match.fun(paste0('any_', escape_type, '_specials'))
	escape_specials <- match.fun(paste0(escape_type, '_escape'))

	if (detect_specials(obj_names))
		names(obj) <- escape_specials(obj_names)
	if (detect_specials(obj_rownames))
		rownames(obj) <- escape_specials(obj_rownames)
	if (detect_specials(obj_colnames))
		colnames(obj) <- escape_specials(obj_colnames)

	return(obj)
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
	return(vec)
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

r_quote <- function(char_vec) paste0("'", sub("'", "\\'", char_vec, fixed = TRUE), "'")

# Create the actually-used functions from the shells above.
latex_escape_names <- function(obj) .escape_names(obj, 'latex')
html_escape_names  <- function(obj) .escape_names(obj, 'html')
any_latex_specials <- function(char_vec) .any_specials(char_vec, latex_specials)
any_html_specials  <- function(char_vec) .any_specials(char_vec, html_specials)
latex_escape_vec <- function(vec) .escape_vec(vec, 'latex')
html_escape_vec  <- function(vec) .escape_vec(vec, 'html')
