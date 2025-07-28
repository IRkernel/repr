options(stringsAsFactors = FALSE)

has_dt <- requireNamespace('data.table', quietly = TRUE)
has_tibble <- requireNamespace('tibble', quietly = TRUE)

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
	repr_text(outer)
	succeed()
})

test_that('nested data.frames can be displayed', {
	# jsonLite creates weird structures: jsonlite::fromJSON('[{"something":{"weird":[1,2]}}]')
	outer <- structure(
		list(
			normal_col = c('c1', 'c2'),
			something = structure(
				list(weird = 1:2, second = c('n1', 'n2')),
				class = "data.frame",
				row.names = 1:2
			)
		),
		class = "data.frame",
		row.names = 1:2
	)
	repr_html(outer)
	repr_latex(outer)
	repr_markdown(outer)
	repr_text(outer)
	succeed()
})

test_that('data.frame with list columns can be displayed', {
	df <- list2DF(list(a=1, b=list(1:2)))
	expected <- '<table class="dataframe">
<caption>A data.frame: 1 × 2</caption>
<thead>
\t<tr><th scope=col>a</th><th scope=col>b</th></tr>
\t<tr><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;list&gt;</th></tr>
</thead>
<tbody>
\t<tr><td>1</td><td>1, 2</td></tr>
</tbody>
</table>
'
	expect_identical(repr_html(df), expected)
	if(has_tibble) {
		expect_identical(repr_html(tibble::as_tibble(df)), sub('data\\.frame','tibble',expected))
	}
	if(has_dt) {
		expect_identical(repr_html(data.table::as.data.table(df)), sub('data\\.frame','data.table',expected))
	}
})

test_that('forced-narrow inputs work', {
	withr::local_options(repr.matrix.max.rows = 2L, repr.matrix.max.cols = 2L)
	df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
	expect_silent(repr_text(df))
	expect_identical(
		# Scrub non-ASCII characters to make the test platform-agnostic.
		gsub("[^a-zA-Z0-9.&;<>= '\"/:\n\t]", "*", repr_html(df)),
		"<table class=\"dataframe\">
<caption>A data.frame: 3 * 3</caption>
<thead>
\t<tr><th scope=col>a</th><th scope=col>*</th><th scope=col>c</th></tr>
\t<tr><th scope=col>&lt;int&gt;</th><th scope=col>*</th><th scope=col>&lt;int&gt;</th></tr>
</thead>
<tbody>
\t<tr><td>1</td><td>*</td><td>7</td></tr>
\t<tr><td>*</td><td>*</td><td>*</td></tr>
\t<tr><td>3</td><td>*</td><td>9</td></tr>
</tbody>
</table>
")
})

test_that('data.table and data.frame elision is the same', {
	skip_if_not_installed('data.table')
	withr::local_options(list(repr.matrix.max.rows = 10L, repr.matrix.max.cols = 10L))
	DF <- data.frame(matrix(rnorm(100L*100L), 100L, 100L))
	expect_identical(repr_text(DF), repr_text(data.table::as.data.table(DF)))
	expect_identical(repr_text(DF[1:10, ]), repr_text(data.table::as.data.table(DF[1:10, ])))
	expect_identical(repr_text(DF[1:10, 1:10]), repr_text(data.table::as.data.table(DF[1:10, 1:10])))
})

test_that('data.table elision works in 1-column and 1-row edge cases', {
	skip_if_not_installed('data.table')
	withr::local_options(list(repr.matrix.max.rows = 2L, repr.matrix.max.cols = 2L))

	DF <- data.frame(a = 1:3)
	expect_identical(repr_text(DF), repr_text(data.table::as.data.table(DF)))

	DF <- data.frame(a = 1L, b = 2L, c = 3L)
	expect_identical(repr_text(DF), repr_text(data.table::as.data.table(DF)))

	DF <- data.frame(a = 1:3, b = 4:6, c = 7:9)
	expect_identical(repr_text(DF), repr_text(data.table::as.data.table(DF)))
})

test_that("partially-empty matrices requiring elision can be displayed", {
  withr::local_options(list(
    repr.matrix.max.rows = 8L,
    repr.matrix.max.cols = 8L
  ))
  m <- matrix(nrow = 0L, ncol = 10L)
  # all on one line
  expect_no_warning(expect_no_match(repr(m), "\n", fixed = TRUE))
  # always [n,] with nothing after it
  expect_no_warning(expect_no_match(repr(t(m)), "\\][^\n]"))

  colnames(m) <- sprintf("A%02d", 1:10)
  # gap from A04 to A07 with \cdots, then only A0n, no newline
  expect_no_warning(expect_no_match(expect_match(repr(m), "A04[^A]*A07"), "\n", fixed = TRUE))
  # gap from A04 to A07 with \vdots, all A0n followed by newline
  expect_no_warning(expect_no_match(expect_match(repr(t(m)), "A04[^A]*A07"), "A0[1-9][^\n]"))
})
