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
