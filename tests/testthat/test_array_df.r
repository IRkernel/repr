context('Arrays and data.frames')

options(stringsAsFactors = FALSE)

test_that('factors display correctly', {
	df = data.frame(a = factor('iamafactor'))
	expect_equal(repr_html(df), '<table>
<thead><tr><th></th><th scope=col>a</th></tr></thead>
<tbody>
\t<tr><th scope=row>1</th><td>iamafactor</td></tr>
</tbody>
</table>
')
})

test_that('mixed factors and strings display correctly', {
	df = data.frame(a = 'iamastring', b = factor('iamafactor'))
	expect_true(is.factor(df$b))
	expect_equal(repr_html(df), '<table>
<thead><tr><th></th><th scope=col>a</th><th scope=col>b</th></tr></thead>
<tbody>
\t<tr><th scope=row>1</th><td>iamastring</td><td>iamafactor</td></tr>
</tbody>
</table>
')
})
