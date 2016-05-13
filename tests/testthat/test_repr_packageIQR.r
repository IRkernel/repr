context('reprs of packageIQR')

options(stringsAsFactors = FALSE)

test_that('repr_html outputs the right html code', {
	x = vignette()
	html = repr_html(x)
	# reprs must return length 1 strings
	expect_equal(length(html), 1)
	expect_is(html, 'character')
	# test the content
	no_vignettes_found = '<h3>Vignettes</h3>\n<p>No vignettes found</p>\n'
	expect_true(nchar(html) > nchar(no_vignettes_found))  # there should be at least a few vignettes
	expect_equal(repr_html(vignette(package = 'repr')), no_vignettes_found)
	# we don't want to output the LibPath column
	expect_false(grepl('LibPath', html))
})

test_that('repr_text outputs the right text', {
	x = vignette()
	txt = repr_text(x)
	# reprs must return length 1 strings
	expect_equal(length(txt), 1)
	expect_is(txt, 'character')
	# test the content
	no_vignettes_found = 'no vignettes found'
	expect_true(nchar(txt) > nchar(no_vignettes_found))  # there should be at least a few vignettes
	expect_equal(repr_text(vignette(package = 'repr')), no_vignettes_found)
})
