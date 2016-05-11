html.specials <- list('&' = '&amp;', '<' = '&lt;', '>' = '&gt;')

html.escape <- function(text) {
	for (chr in names(html.specials)) {
		text <- gsub(chr, html.specials[[chr]], text, fixed = TRUE)
	}
	text
}

latex.specials <- list(
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

latex.escape <- function(text) {
	for (chr in names(latex.specials)) {
		text <- gsub(chr, latex.specials[[chr]], text, fixed = TRUE)
	}
	# undo superfluous escape
	gsub('\\textbackslash\\{\\}', '\\textbackslash{}', text, fixed = TRUE)
}

.escape.names <- function(obj, escape_type) {
	# Generic function for escaping names.
	# Depending on the object type, names, rownames and colnames may be the same or different
	# Capture all three before changing them.
	# Note that the resulting names may not be valid R syntax.
	# escape_type must be 'latex' or 'html'
	stopifnot(any(escape_type == c('html', 'latex')))

	obj_names <- names(obj)
	obj_rownames <- rownames(obj)
	obj_colnames <- colnames(obj)

	detect.specials  <- match.fun(paste0('any.', escape_type, '.specials'))
	escape.specials <- match.fun(paste0(escape_type, '.escape'))

	if (detect.specials(obj_names))
		names(obj) <- escape.specials(obj_names)
	if (detect.specials(obj_rownames))
		rownames(obj) <- escape.specials(obj_rownames)
	if (detect.specials(obj_colnames))
		colnames(obj) <- escape.specials(obj_colnames)

	return(obj)
}

.any.specials <- function(char_vec, specials_list) {
	# Use this function to avoid setting names unnecessarily (and thereby copying
	# the object many times).
	# This function could also be done in one regex with a bunch of groups and '|'
	# The use of 'any' should make this function play nice with NULL names.
	if (! inherits(char_vec, c('character', 'factor')))
		return(FALSE)

	grepl.one.special <- function(special, char_vec) {
		return(any(grepl(special, char_vec, fixed = TRUE)))
	}
	# Search, using vapply, for each special.
	specials_match <- vapply(names(specials_list), grepl.one.special,
		char_vec = char_vec, FUN.VALUE = FALSE)
	return(any(specials_match))
}

.escape.vec <- function(vec, escape_type) {
	# escape_type must be 'latex' or 'html'
	stopifnot(any(escape_type == c('html', 'latex')))
	# .escape.vec should never change the class of its input.
	# That seems useful, since functions like ellip.limit.arr check class.
	if (!(is.vector(vec) || is.factor(vec))) {
	    stop('expected `vec` to be a vector or factor but it is a ', paste(class(vec), collapse = ', '))
	}

	detect.specials  <- match.fun(paste0('any.', escape_type, '.specials'))
	escape.specials <- match.fun(paste0(escape_type, '.escape'))

	if (detect.specials(vec)) {
		if (is.factor(vec)) {
			levels(vec) <- escape.specials(levels(vec))
		} else {
			vec <- escape.specials(vec) # regular character vec
		}
	}
	return(vec)
}

slice.row <- function(df, row) {
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

# Create the actually-used functions from the shells above.
latex.escape.names <- function(obj) {
	.escape.names(obj, 'latex')
}
html.escape.names <- function(obj) {
	.escape.names(obj, 'html')
}
any.latex.specials <- function(char_vec) {
	.any.specials(char_vec, latex.specials)
}
any.html.specials <- function(char_vec) {
	.any.specials(char_vec, html.specials)
}
latex.escape.vec <- function(vec) {
	.escape.vec(vec, 'latex')
}
html.escape.vec <- function(vec) {
	.escape.vec(vec, 'html')
}
