has_dt <- requireNamespace('data.table', quietly = TRUE)
has_tibble <- requireNamespace('tibble', quietly = TRUE)



test_that('unprintables get escaped', {
	expect_identical(repr_html('\001'), "'\\001'")
})

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
	expect_identical(repr_latex('['), "'{[}'")
	expect_identical(repr_latex(c('[', '|')),
"\\begin{enumerate*}
\\item '{[}'
\\item '\\textbar{}'
\\end{enumerate*}
")
})

test_that('HTML escaping in vectors works', {
	expect_identical(repr_html('<'), "'&lt;'")
	expect_identical(
		repr_html(c('<', '&')),
		paste0(list_style, "<ol class=list-inline><li>'&lt;'</li><li>'&amp;'</li></ol>\n")
	)
})

test_that('LaTeX escaping in matrices works', {
	expect_identical(repr_latex(matrix(c('[', '{', '%', '#'), 2, 2, TRUE)),
'A matrix: 2 \u00D7 2 of type chr
\\begin{tabular}{ll}
\t {[} & \\{\\\\
\t \\% & \\#\\\\
\\end{tabular}
')
	expect_identical(repr_latex(matrix(c(']', '}', '&', '_'), 2, 2, TRUE, list(c('$', '#'), c('%', '|')))),
'A matrix: 2 \u00D7 2 of type chr
\\begin{tabular}{r|ll}
  & \\% & \\textbar{}\\\\
\\hline
\t\\$ & {]} & \\}\\\\
\t\\# & \\& & \\_\\\\
\\end{tabular}
')
})

test_that('HTML escaping in matrices works', {
	expect_identical(repr_html(matrix(c('[', '{', '%', '#'), 2, 2, TRUE)),
'<table class="dataframe">
<caption>A matrix: 2 \u00D7 2 of type chr</caption>
<tbody>
\t<tr><td>[</td><td>{</td></tr>
\t<tr><td>%</td><td>#</td></tr>
</tbody>
</table>
')
	expect_identical(repr_html(matrix(c(']', '}', '&', '_'), 2, 2, TRUE, list(c('$', '#'), c('%', '|')))),
'<table class="dataframe">
<caption>A matrix: 2 \u00D7 2 of type chr</caption>
<thead>
\t<tr><th></th><th scope=col>%</th><th scope=col>|</th></tr>
</thead>
<tbody>
\t<tr><th scope=row>$</th><td>]</td><td>}</td></tr>
\t<tr><th scope=row>#</th><td>&amp;</td><td>_</td></tr>
</tbody>
</table>
')
})

test_that('LaTeX escaping in lists works', {
	expect_identical(repr_latex(list(lbr = '[')), "\\textbf{\\$lbr} = '{[}'")
	expect_identical(repr_latex(list(`&` = '%')), "\\textbf{\\$`\\&`} = '\\%'")
})

test_that('HTML escaping in lists works', {
	expect_identical(repr_html(list(lt = '<')), "<strong>$lt</strong> = '&lt;'")
	expect_identical(repr_html(list(`&` = '<')), "<strong>$`&amp;`</strong> = '&lt;'")
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
	if (has_tibble) {
		dtbl <- tibble::as_tibble(df)
		answer <- repr_text(dtbl)
		expect_identical(answer, expected)
	}
})

test_that('Factors are maintained in small arrays for HTML', {
	df <- data.frame(a = 1:4, b = factor(1:4, levels = 1:4, labels = c("A", "B", "C", "D")))
	expected <-
'<table class="dataframe">
<caption>A data.frame: 4 \u00D7 2</caption>
<thead>
\t<tr><th scope=col>a</th><th scope=col>b</th></tr>
\t<tr><th scope=col>&lt;int&gt;</th><th scope=col>&lt;fct&gt;</th></tr>
</thead>
<tbody>
\t<tr><td>1</td><td>A</td></tr>
\t<tr><td>2</td><td>B</td></tr>
\t<tr><td>3</td><td>C</td></tr>
\t<tr><td>4</td><td>D</td></tr>
</tbody>
</table>
'
	expect_identical(repr_html(df), expected)

	if (has_dt) {
		dt <- data.table::as.data.table(df)
		expect_identical(repr_html(dt), sub('data\\.frame', 'data.table', expected))
	}
	if (has_tibble) {
		dtbl <- tibble::as_tibble(df)
		expect_identical(repr_html(dtbl), sub('data\\.frame', 'tibble', expected))
	}
})

test_that('Factors are sanitized in small data.frames for HTML', {
	df <- data.frame(a = 1:4, b = factor(1:4, levels = 1:4, labels = c("A&", "B>", "C", "D")))
	expected <-
'<table class="dataframe">
<caption>A data.frame: 4 \u00D7 2</caption>
<thead>
\t<tr><th scope=col>a</th><th scope=col>b</th></tr>
\t<tr><th scope=col>&lt;int&gt;</th><th scope=col>&lt;fct&gt;</th></tr>
</thead>
<tbody>
\t<tr><td>1</td><td>A&amp;</td></tr>
\t<tr><td>2</td><td>B&gt;</td></tr>
\t<tr><td>3</td><td>C </td></tr>
\t<tr><td>4</td><td>D </td></tr>
</tbody>
</table>
'
	expect_identical(repr_html(df), expected)

	if (has_dt) {
		dt <- data.table::as.data.table(df)
		expect_identical(repr_html(dt), sub('data\\.frame', 'data.table', expected))
	}
	if (has_tibble) {
		dtbl <- tibble::as_tibble(df)
		expect_identical(repr_html(dtbl), sub('data\\.frame', 'tibble', expected))
	}
})

test_that('Factors are maintained in small arrays for LaTeX', {
	df <- data.frame(a = 1:4, b = factor(1:4, levels = 1:4, labels = c("A", "B", "C", "D")))
	expected <-
'A data.frame: 4 \u00D7 2
\\begin{tabular}{ll}
 a & b\\\\
 <int> & <fct>\\\\
\\hline
\t 1 & A\\\\
\t 2 & B\\\\
\t 3 & C\\\\
\t 4 & D\\\\
\\end{tabular}
'
	expect_identical(repr_latex(df), expected)

	if (has_dt) {
		dt <- data.table::as.data.table(df)
		expect_identical(repr_latex(dt), sub('data\\.frame', 'data.table', expected))
	}
	if (has_tibble) {
		dtbl <- tibble::as_tibble(df)
		expect_identical(repr_latex(dtbl), sub('data\\.frame', 'tibble', expected))
	}
})

test_that('Factors are sanitized in small data.frames for LaTeX', {
	df <- data.frame(a = 1:4, b = factor(1:4, levels = 1:4, labels = c("A&", "B%", "_C_", "D")))
	expected <-
'A data.frame: 4 \u00D7 2
\\begin{tabular}{ll}
 a & b\\\\
 <int> & <fct>\\\\
\\hline
\t 1 & A\\& \\\\
\t 2 & B\\% \\\\
\t 3 & \\_C\\_\\\\
\t 4 & D  \\\\
\\end{tabular}
'
	expect_identical(repr_latex(df), expected)

	if (has_dt) {
		dt <- data.table::as.data.table(df)
		expect_identical(repr_latex(dt), sub('data\\.frame', 'data.table', expected))
	}
	if (has_tibble) {
		dtbl <- tibble::as_tibble(df)
		expect_identical(repr_latex(dtbl), sub('data\\.frame', 'tibble', expected))
	}
})

test_that('vector entries with consecutive spaces get wrapped', {
	v <- c('one space', 'two  spaces')
	expect_identical(
		repr_html(v),
		paste0(
			list_style,
			'<ol class=list-inline>',
			"<li>'one space'</li>",
			"<li><span style=white-space:pre-wrap>'two  spaces'</span></li>",
			'</ol>\n'
		)
	)
})
