context('reprs of arrays and data.frames')

options(stringsAsFactors = FALSE)

test_that('empty data.frames work', {
	expect_identical(repr_html(data.frame()), '')
	expect_identical(repr_html(as.data.frame(matrix(integer(0L), 1L, 0L))), '')
	# no data.frame without colnames possible
})

test_that('empty matrices work', {
	expect_identical(repr_html(matrix(integer(0L), 0L, 0L)), '')
	expect_identical(repr_html(matrix(integer(0L), 1L, 0L)), '')
	expect_identical(repr_html(matrix(integer(0L), 0L, 1L)), '')
})

test_that('factors display correctly', {
	df = data.frame(a = factor('iamafactor'))
	expect_identical(repr_html(df),
'<table>
<thead><tr><th scope=col>a</th></tr></thead>
<tbody>
\t<tr><td>iamafactor</td></tr>
</tbody>
</table>
')
})

test_that('mixed factors and strings display correctly', {
	df = data.frame(a = 'iamastring', b = factor('iamafactor'))
	expect_true(is.factor(df$b))
	expect_identical(repr_html(df),
'<table>
<thead><tr><th scope=col>a</th><th scope=col>b</th></tr></thead>
<tbody>
\t<tr><td>iamastring</td><td>iamafactor</td></tr>
</tbody>
</table>
')
})

test_that('date display correctly', {
	df = data.frame(a = as.POSIXct('2016-05-28 10:00:00', tz = 'UTC'))
	expect_identical(repr_html(df),
'<table>
<thead><tr><th scope=col>a</th></tr></thead>
<tbody>
\t<tr><td>2016-05-28 10:00:00</td></tr>
</tbody>
</table>
')
})

test_that('markdown works', {
	df <- data.frame(a = 1:2, b = letters[1:2])
	expect_identical(repr_markdown(df),
'
| a | b |
|---|---|
| 1 | a |
| 2 | b |

')
})

test_that('markdown works with rownames', {
	df <- data.frame(a = 1:2, b = letters[1:2], row.names = LETTERS[1:2])
	expect_identical(repr_markdown(df),
'
| <!--/--> | a | b |
|---|---|---|
| A | 1 | a |
| B | 2 | b |

')
})
