context('Vectors')

test_that('empty vectors display correctly', {
	expect_identical(repr_html(logical(0L)), '')
})

test_that('1 element vectors display correctly', {
	expect_identical(repr_html(1), '1')
})

test_that('plain vectors display correctly', {
	expect_identical(repr_html(c(1, 2)), '<ol class=list-inline>
\t<li>1</li>
\t<li>2</li>
</ol>
')
})

test_that('named vectors display correctly', {
	expect_identical(repr_html(c(a = 1, b = 2)), '<dl class=dl-horizontal>
\t<dt>a</dt>
\t\t<dd>1</dd>
\t<dt>b</dt>
\t\t<dd>2</dd>
</dl>
')
})

test_that('factors display correctly', {
	expect_identical(repr_html(factor(c('a', 'b'))), '<ol class=list-inline>
\t<li>a</li>
\t<li>b</li>
</ol>
')
})
