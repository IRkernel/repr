context('LaTeX and HTML escaping')

has_dt <- requireNamespace('data.table', quietly = TRUE)
has_dplyr <- requireNamespace('dplyr', quietly = TRUE)


expect_equivalent_string <- function(result, expectation){
	"Only use ' as a string delimiter in strings."
	expect_identical(gsub('"', "'", x = result), expectation)
}

test_that('simple LaTeX escaping works', {
	expect_identical(latex_escape('\\'), '\\textbackslash{}')
	expect_identical(latex_escape('{}'), '\\{\\}')
	expect_identical(latex_escape('$'), '\\$')
	expect_identical(latex_escape('^'), '\\textasciicircum{}')
	expect_identical(latex_escape('_'), '\\_')
	expect_identical(latex_escape('%'), '\\%')
	expect_identical(latex_escape('#'), '\\#')
	expect_identical(latex_escape('&'), '\\&')
	expect_identical(latex_escape('~'), '\\textasciitilde{}')
	expect_identical(latex_escape('|'), '\\textbar{}')
	expect_identical(latex_escape('[]'), '{[}{]}')
})

test_that('simple HTML escaping works', {
	expect_identical(html_escape('&'), '&amp;')
	expect_identical(html_escape('<'), '&lt;')
	expect_identical(html_escape('>'), '&gt;')
})

test_that('LaTeX escaping in vectors works', {
	expect_equivalent_string(repr_latex('['), "'{[}'")
	expect_equivalent_string(repr_latex(c('[', '|')),
"\\begin{enumerate*}
\\item '{[}'
\\item '\\textbar{}'
\\end{enumerate*}
")
})

test_that('HTML escaping in vectors works', {
	expect_equivalent_string(repr_html('<'), "'&lt;'")
	expect_equivalent_string(repr_html(c('<', '&')),
"<ol class=list-inline>
\t<li>'&lt;'</li>
\t<li>'&amp;'</li>
</ol>
")
})

test_that('LaTeX escaping in matrices works', {
	expect_identical(repr_latex(matrix(c('[', '{', '%', '#'), 2, 2, TRUE)),
'\\begin{tabular}{ll}
\t {[} & \\{\\\\
\t \\% & \\#\\\\
\\end{tabular}
')
	expect_identical(repr_latex(matrix(c(']', '}', '&', '_'), 2, 2, TRUE, list(c('$', '#'), c('%', '|')))),
'\\begin{tabular}{r|ll}
  & \\% & \\textbar{}\\\\
\\hline
\t\\$ & {]} & \\}\\\\
\t\\# & \\& & \\_\\\\
\\end{tabular}
')
})

test_that('HTML escaping in matrices works', {
	expect_identical(repr_html(matrix(c('[', '{', '%', '#'), 2, 2, TRUE)),
'<table>
<tbody>
\t<tr><td>[</td><td>{</td></tr>
\t<tr><td>%</td><td>#</td></tr>
</tbody>
</table>
')
	expect_identical(repr_html(matrix(c(']', '}', '&', '_'), 2, 2, TRUE, list(c('$', '#'), c('%', '|')))),
'<table>
<thead><tr><th></th><th scope=col>%</th><th scope=col>|</th></tr></thead>
<tbody>
\t<tr><th scope=row>$</th><td>]</td><td>}</td></tr>
\t<tr><th scope=row>#</th><td>&amp;</td><td>_    </td></tr>
</tbody>
</table>
')
})

test_that('LaTeX escaping in lists works', {
	expect_equivalent_string(repr_latex(list(lbr = '[')), "\\textbf{\\$lbr} = '{[}'")
	expect_equivalent_string(repr_latex(list(`&` = '%')), "\\textbf{\\$`\\&`} = '\\%'")
})

test_that('HTML escaping in lists works', {
	expect_equivalent_string(repr_html(list(lt = '<')), "<strong>$lt</strong> = '&lt;'")
	expect_equivalent_string(repr_html(list(`&` = '<')), "<strong>$`&amp;`</strong> = '&lt;'")
})

test_that('Factors are maintained in small arrays for text', {
	df <- data.frame(a = 1:4, b = factor(1:4, levels = 1:4, labels = c("A", "B", "C", "D")))
	expected <- "  a b\n1 1 A\n2 2 B\n3 3 C\n4 4 D"
	expect_identical(repr_text(df), expected)
	if (has_dt) {
		dt <- data.table::as.data.table(df)
		answer <- repr_text(dt)
		expect_identical(answer, expected)
	}
	if (has_dplyr) {
		dtbl <- dplyr::as.tbl(df)
		answer <- repr_text(dtbl)
		expect_identical(answer, expected)
	}
})

test_that('Factors are maintained in small arrays for HTML', {
	df <- data.frame(a = 1:4, b = factor(1:4, levels = 1:4, labels = c("A", "B", "C", "D")))
	# Sometimes extra whitespace is added, different than what I expected.
	# That's fine, just strip out all white space.
	expected <- gsub('\\s', '',
'<table>
<thead><tr><th scope=col>a</th><th scope=col>b</th></tr></thead>
<tbody>
\t<tr><td>1</td><td>A</td></tr>
\t<tr><td>2</td><td>B</td></tr>
\t<tr><td>3</td><td>C</td></tr>
\t<tr><td>4</td><td>D</td></tr>
</tbody>
</table>
',
		perl = TRUE)
	answer <- gsub('\\s', '', repr_html(df), perl = TRUE)
	expect_identical(answer, expected)

	if (has_dt) {
		dt <- data.table::as.data.table(df)
		answer <- gsub('\\s', '', repr_html(dt), perl = TRUE)
		expect_identical(answer, expected)
	}
	if (has_dplyr) {
		dtbl <- dplyr::as.tbl(df)
		answer <- gsub('\\s', '', repr_html(dtbl), perl = TRUE)
		expect_identical(answer, expected)
	}
})

test_that('Factors are sanitized in small data.frames for HTML', {
	df <- data.frame(a = 1:4, b = factor(1:4, levels = 1:4, labels = c("A&", "B>", "C", "D")))
	# Sometimes extra whitespace is added, different than what I expected.
	# That's fine, just strip out all white space.
	expected <- gsub('\\s', '',
'<table>
<thead><tr><th scope=col>a</th><th scope=col>b</th></tr></thead>
<tbody>
\t<tr><td>1</td><td>A&amp;</td></tr>
\t<tr><td>2</td><td>B&gt;</td></tr>
\t<tr><td>3</td><td>C</td></tr>
\t<tr><td>4</td><td>D</td></tr>
</tbody>
</table>
',
		perl = TRUE)
	answer <- gsub('\\s', '', repr_html(df), perl = TRUE)
	expect_identical(answer, expected)

	if (has_dt) {
		dt <- data.table::as.data.table(df)
		answer <- gsub('\\s', '', repr_html(df), perl = TRUE)
		expect_identical(answer, expected)
	}
	if (has_dplyr) {
		dtbl <- dplyr::as.tbl(df)
		answer <- gsub('\\s', '', repr_html(dtbl), perl = TRUE)
		expect_identical(answer, expected)
	}
})

test_that('Factors are maintained in small arrays for LaTeX', {
	df <- data.frame(a = 1:4, b = factor(1:4, levels = 1:4, labels = c("A", "B", "C", "D")))
	# Sometimes extra whitespace is added, different than what I expected.
	# That's fine, just strip out all white space.
	expected <- gsub('\\s', '',
'\\begin{tabular}{r|ll}
 a & b\\\\
\\hline
\t1 & A\\\\
\t2 & B\\\\
\t3 & C\\\\
\t4 & D\\\\
\\end{tabular}
',
		perl = TRUE)
	answer <- gsub('\\s', '', repr_latex(df), perl = TRUE)
	expect_identical(answer, expected)

	if (has_dt) {
		dt <- data.table::as.data.table(df)
		answer <- gsub('\\s', '', repr_latex(dt), perl = TRUE)
		expect_identical(answer, expected)
	}
	if (has_dplyr) {
		dtbl <- dplyr::as.tbl(df)
		answer <- gsub('\\s', '', repr_latex(dtbl), perl = TRUE)
		expect_identical(answer, expected)
	}
})

test_that('Factors are sanitized in small data.frames for LaTeX', {
	df <- data.frame(a = 1:4, b = factor(1:4, levels = 1:4, labels = c("A&", "B%", "_C_", "D")))
	# Sometimes extra whitespace is added, different than what I expected.
	# That's fine, just strip out all white space.
	expected <- gsub('\\s', '',
'\\begin{tabular}{r|ll}
 a & b\\\\
\\hline
\t1 & A\\&\\\\
\t2 & B\\%\\\\
\t3 & \\_C\\_\\\\
\t4 &	 D\\\\
\\end{tabular}
',
		perl = TRUE)
	answer <- gsub('\\s', '', repr_latex(df), perl = TRUE)
	expect_identical(answer, expected)

	if (has_dt) {
		dt <- data.table::as.data.table(df)
		answer <- gsub('\\s', '', repr_latex(dt), perl = TRUE)
		expect_identical(answer, expected)
	}
	if (has_dplyr) {
		dtbl <- dplyr::as.tbl(df)
		answer <- gsub('\\s', '', repr_latex(dtbl), perl = TRUE)
		expect_identical(answer, expected)
	}
})

test_that('vector entries with consecutive spaces get wrapped', {
	v <- c('one space', 'two  spaces')
	expect_identical(repr_html(v), "<ol class=list-inline>
\t<li>'one space'</li>
\t<li><span style=white-space:pre-wrap>'two  spaces'</span></li>
</ol>
")
})
