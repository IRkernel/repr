context('lists')

options(stringsAsFactors = FALSE)

test_that('lists must be the only class', {
	inp = structure("something", class=c("list", "A"))
	expect_equal(repr_html(inp), NULL)
	expect_equal(repr_markdown(inp), NULL)
	expect_equal(repr_latex(inp), NULL)
	inp = structure("something", class=c("A", "list"))
	expect_equal(repr_html(inp), NULL)
	expect_equal(repr_markdown(inp), NULL)
	expect_equal(repr_latex(inp), NULL)
})
