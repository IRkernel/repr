context('reprs of arrays and data.frames')

options(stringsAsFactors = FALSE)

expect_id_text <- function(a, b) {
	expect_identical(strsplit(a, '\n')[[1]], strsplit(b, '\n')[[1]])
}


test_that('empty data.frames work', {
	expect_id_text(repr_html(data.frame()), '')
	expect_id_text(repr_html(as.data.frame(matrix(integer(0L), 1L, 0L))), '')
	# no data.frame without colnames possible
})

test_that('empty matrices work', {
	expect_id_text(repr_html(matrix(integer(0L), 0L, 0L)), '')
	expect_id_text(repr_html(matrix(integer(0L), 1L, 0L)), '')
	expect_id_text(repr_html(matrix(integer(0L), 0L, 1L)), '')
})

test_that('factors display correctly', {
	df = data.frame(a = factor('iamafactor'))
	expect_id_text(repr_html(df),
'<table>
<thead>
\t<tr><th scope=col>a</th></tr>
</thead>
<tbody>
\t<tr><td>iamafactor</td></tr>
</tbody>
</table>
')
})

test_that('mixed factors and strings display correctly', {
	df = data.frame(a = 'iamastring', b = factor('iamafactor'))
	expect_true(is.factor(df$b))
	expect_id_text(repr_html(df),
'<table>
<thead>
\t<tr><th scope=col>a</th><th scope=col>b</th></tr>
</thead>
<tbody>
\t<tr><td>iamastring</td><td>iamafactor</td></tr>
</tbody>
</table>
')
})

test_that('date display correctly', {
	df = data.frame(a = as.POSIXct('2016-05-28 10:00:00', tz = 'UTC'))
	expect_id_text(repr_html(df),
'<table>
<thead>
\t<tr><th scope=col>a</th></tr>
</thead>
<tbody>
\t<tr><td>2016-05-28 10:00:00</td></tr>
</tbody>
</table>
')
})

test_that('markdown works', {
	df <- data.frame(a = 1:2, b = letters[1:2])
	expect_id_text(repr_markdown(df), '
| a | b |
|---|---|
| 1 | a |
| 2 | b |

')
})

test_that('markdown works with rownames', {
	df <- data.frame(a = 1:2, b = letters[1:2], row.names = LETTERS[1:2])
	expect_id_text(repr_markdown(df), '
| <!--/--> | a | b |
|---|---|---|
| A | 1 | a |
| B | 2 | b |

')
})

test_that('nested data.frames work', {
	df <- data.frame(driver = c('Bowser', 'Peach'))
	df$vehicle <- data.frame(model = c('Piranha Prowler', 'Royal Racer'))
	df$vehicle$stats <- data.frame(speed = c(55, 34), weight = c(67, 24), drift = c(35, 32))
	df$occupation <- c('Koopa', 'Princess')
	expect_id_text(repr_markdown(df), '
| driver | vehicle.model | vehicle.stats.speed | vehicle.stats.weight | vehicle.stats.drift | occupation |
|---|---|---|---|---|---|
| Bowser          | Piranha Prowler | 55              | 67              | 35              | Koopa           |
| Peach           | Royal Racer     | 34              | 24              | 32              | Princess        |

')
})

test_that('reprs work on an 1d array', {
	state <- factor(c("tas", "sa",  "qld", "nsw", "nsw", "nt",  "wa",  "wa",  "qld", "vic"))
	incomes <- c(60, 49, 40, 61, 64, 60, 59, 54, 62, 69)
	one_d_arr <- tapply(incomes, state, mean)
	repr_html(one_d_arr)
	repr_latex(one_d_arr)
	repr_markdown(one_d_arr)
	succeed()
})

