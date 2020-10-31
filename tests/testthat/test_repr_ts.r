test_that('repr_text works with 1D ts object', {
	expect_identical(repr_text(as.ts(1:4)), 'A Time Series:\n[1] 1 2 3 4')
})

test_that('repr_markdown works with 2D ts object', {
	series <- ts(c(
		10.9817,11.5721,11.9866,11.1937,
		11.6959,11.3342,10.8519,10.7861
	), start = c(1984, 03), frequency = 4)
	expect_identical(repr_markdown(series), '
A Time Series: 3 \u00D7 4

| <!--/--> | Qtr1 | Qtr2 | Qtr3 | Qtr4 |
|---|---|---|---|---|
| 1984 | <!----> | <!----> | 10.9817 | 11.5721 |
| 1985 | 11.9866 | 11.1937 | 11.6959 | 11.3342 |
| 1986 | 10.8519 | 10.7861 | <!----> | <!----> |

')
})

test_that('Displaying a single-row time series works', {
	series <- ts(1:4, start=c(2000,1), frequency=12)
	expect_identical(repr_markdown(series), '
A Time Series: 1 \u00D7 4

| <!--/--> | Jan | Feb | Mar | Apr |
|---|---|---|---|---|
| 2000 | 1 | 2 | 3 | 4 |

')
})
