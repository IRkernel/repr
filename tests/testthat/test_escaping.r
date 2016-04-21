context('LaTeX and HTML escaping')

expect_equivalent_string <- function(result, expectation){
	"Only use ' as a string delimiter in strings."
	expect_equal(gsub('"', "'", x = result), expectation)
}

test_that('simple LaTeX escaping works', {
	expect_equal(latex.escape('\\'), '\\textbackslash{}')
	expect_equal(latex.escape('{}'), '\\{\\}')
	expect_equal(latex.escape('$'), '\\$')
	expect_equal(latex.escape('^'), '\\textasciicircum{}')
	expect_equal(latex.escape('_'), '\\_')
	expect_equal(latex.escape('%'), '\\%')
	expect_equal(latex.escape('#'), '\\#')
	expect_equal(latex.escape('&'), '\\&')
	expect_equal(latex.escape('~'), '\\textasciitilde{}')
	expect_equal(latex.escape('|'), '\\textbar{}')
	expect_equal(latex.escape('[]'), '{[}{]}')
})

test_that('simple HTML escaping works', {
	expect_equal(html.escape('&'), '&amp;')
	expect_equal(html.escape('<'), '&lt;')
	expect_equal(html.escape('>'), '&gt;')
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
	expect_equal(repr_latex(matrix(c('[', '{', '%', '#'), 2, 2, TRUE)),
'\\begin{tabular}{ll}
\t {[} & \\{\\\\
\t \\% & \\#\\\\
\\end{tabular}
')
	expect_equal(repr_latex(matrix(c(']', '}', '&', '_'), 2, 2, TRUE, list(c('$', '#'), c('%', '|')))),
'\\begin{tabular}{r|ll}
  & \\% & \\textbar{}\\\\\n\\hline
\t\\$ & {]} & \\}\\\\
\t\\# & \\& & \\_\\\\
\\end{tabular}
')
})

test_that('HTML escaping in matrices works', {
	expect_equal(repr_html(matrix(c('[', '{', '%', '#'), 2, 2, TRUE)),
'<table>
<tbody>
\t<tr><td>[</td><td>{</td></tr>
\t<tr><td>%</td><td>#</td></tr>
</tbody>
</table>
')
	expect_equal(repr_html(matrix(c(']', '}', '&', '_'), 2, 2, TRUE, list(c('$', '#'), c('%', '|')))),
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
