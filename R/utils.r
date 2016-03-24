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

latex.escape.names <- function(obj) {
	# Depending on the object type, names, rownames and colnames may be the same or different
	# Capture all three before changing them.
	# Note that the resulting names may not be valid R syntax.
	obj_names <- names(obj)
	obj_rownames <- rownames(obj)
	obj_colnames <- colnames(obj)
	if (any.latex.specials(obj_names))
		names(obj) <- latex.escape(obj_names)
	if (any.latex.specials(obj_rownames))
		rownames(obj) <- latex.escape(obj_rownames)
	if (any.latex.specials(obj_colnames))
		colnames(obj) <- latex.escape(obj_colnames)

	return(obj)
}

any.latex.specials <- function(char_vec) {
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
	specials_match <- vapply(names(latex.specials), grepl.one.special,
		char_vec = char_vec, FUN.VALUE = FALSE)
	return(any(specials_match))
}

latex.escape.vec <- function(vec) {
	# latex.escape.vec should never change the class of its input.
	# That seems useful, since functions like ellip.limit.arr check class.
	if (!(is.vector(vec) || is.factor(vec))) {
	    stop('expected `vec` to be a vector or factor but it is a ', paste(class(vec), collapse = ', '))
	}
	if (any.latex.specials(vec)) {
		if (is.factor(vec)) {
			levels(vec) <- latex.escape(levels(vec))
		} else {
			vec <- latex.escape(vec) # regular character vec
		}
	}
	return(vec)
}
