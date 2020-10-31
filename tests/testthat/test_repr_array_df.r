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
'<table class="dataframe">
<caption>A data.frame: 1 \u00D7 1</caption>
<thead>
\t<tr><th scope=col>a</th></tr>
\t<tr><th scope=col>&lt;fct&gt;</th></tr>
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
	expect_identical(repr_html(df),
'<table class="dataframe">
<caption>A data.frame: 1 \u00D7 2</caption>
<thead>
\t<tr><th scope=col>a</th><th scope=col>b</th></tr>
\t<tr><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;fct&gt;</th></tr>
</thead>
<tbody>
\t<tr><td>iamastring</td><td>iamafactor</td></tr>
</tbody>
</table>
')
})

test_that('date display correctly', {
	df = data.frame(a = as.POSIXct('2016-05-28 10:00:00', tz = 'UTC'))
	expect_identical(repr_html(df),
'<table class="dataframe">
<caption>A data.frame: 1 \u00D7 1</caption>
<thead>
\t<tr><th scope=col>a</th></tr>
\t<tr><th scope=col>&lt;dttm&gt;</th></tr>
</thead>
<tbody>
\t<tr><td>2016-05-28 10:00:00</td></tr>
</tbody>
</table>
')
})

test_that('markdown works', {
	df <- data.frame(a = 1:2, b = letters[1:2])
	expect_identical(repr_markdown(df), '
A data.frame: 2 \u00D7 2

| a &lt;int&gt; | b &lt;chr&gt; |
|---|---|
| 1 | a |
| 2 | b |

')
})

test_that('markdown works with rownames', {
	df <- data.frame(a = 1:2, b = letters[1:2], row.names = LETTERS[1:2])
	expect_identical(repr_markdown(df), '
A data.frame: 2 \u00D7 2

| <!--/--> | a &lt;int&gt; | b &lt;chr&gt; |
|---|---|---|
| A | 1 | a |
| B | 2 | b |

')
})

test_that('latex works', {
	df <- data.frame(a = 1:2, b = letters[1:2])
	expected <-
'A data.frame: 2 \u00D7 2
\\begin{tabular}{ll}
 a & b\\\\
 <int> & <chr>\\\\
\\hline
\t 1 & a\\\\
\t 2 & b\\\\
\\end{tabular}
'
	expect_identical(repr_latex(df), expected)
})

test_that('latex works with rownames', {
	df <- data.frame(a = 1:2, b = letters[1:2], row.names = LETTERS[1:2])
	expected <-
'A data.frame: 2 \u00D7 2
\\begin{tabular}{r|ll}
  & a & b\\\\
  & <int> & <chr>\\\\
\\hline
\tA & 1 & a\\\\
\tB & 2 & b\\\\
\\end{tabular}
'
	expect_identical(repr_latex(df), expected)
})

test_that('nested data.frames work', {
	df <- data.frame(driver = c('Bowser', 'Peach'))
	df$vehicle <- data.frame(model = c('Piranha Prowler', 'Royal Racer'))
	df$vehicle$stats <- data.frame(speed = c(55, 34), weight = c(67, 24), drift = c(35, 32))
	df$occupation <- c('Koopa', 'Princess')
	expect_identical(repr_markdown(df), '
A data.frame: 2 \u00D7 6

| driver &lt;chr&gt; | vehicle.model &lt;chr&gt; | vehicle.stats.speed &lt;dbl&gt; | vehicle.stats.weight &lt;dbl&gt; | vehicle.stats.drift &lt;dbl&gt; | occupation &lt;chr&gt; |
|---|---|---|---|---|---|
| Bowser | Piranha Prowler | 55 | 67 | 35 | Koopa    |
| Peach  | Royal Racer     | 34 | 24 | 32 | Princess |

')
})

test_that('matrices in data.frames work', {
	df <- aggregate(. ~ Species, iris, range)
	expect_equal(dim(df$Sepal.Width), c(3, 2))
	expect_identical(repr_markdown(df), '
A data.frame: 3 \u00D7 5

| Species &lt;fct&gt; | Sepal.Length &lt;dbl[,2]&gt; | Sepal.Width &lt;dbl[,2]&gt; | Petal.Length &lt;dbl[,2]&gt; | Petal.Width &lt;dbl[,2]&gt; |
|---|---|---|---|---|
| setosa     | 4.3, 5.8 | 2.3, 4.4 | 1.0, 1.9 | 0.1, 0.6 |
| versicolor | 4.9, 7.0 | 2.0, 3.4 | 3.0, 5.1 | 1.0, 1.8 |
| virginica  | 4.9, 7.9 | 2.2, 3.8 | 4.5, 6.9 | 1.4, 2.5 |

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


