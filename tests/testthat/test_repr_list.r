context('reprs of lists')

test_that('plain lists display correctly', {
	expect_identical(repr_html(list(1, 2)), '<ol>
\t<li>1</li>
\t<li>2</li>
</ol>
')
})

test_that('named lists display correctly', {
	expect_identical(repr_html(list(a = 1, b = 2)), '<dl>
\t<dt>$a</dt>
\t\t<dd>1</dd>
\t<dt>$b</dt>
\t\t<dd>2</dd>
</dl>
')
})

test_that('lists with unknown element types don’t display', {
	methods::setClass('__unknown', methods::representation(n = 'character'))
	expect_identical(repr_html(list(1, methods::new('__unknown'))), NULL)
})

test_that('lists with nonexistant names work', {
	expect_identical(repr_html(list(a = 0, 1)), '<dl>
\t<dt>$a</dt>
\t\t<dd>0</dd>
\t<dt>[[2]]</dt>
\t\t<dd>1</dd>
</dl>
')
})
