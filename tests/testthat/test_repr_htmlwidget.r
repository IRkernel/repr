context('reprs of html widgets and tags')

test_that('A basic tag works', {
	r <- repr_html(htmltools::div())
	expect_identical(r, sprintf(repr:::HTML_SKELETON, '', '<div></div>'))
})

test_that('A basic widget works', {
	skip_if_not_installed('stringr')
	
	r <- repr_html(stringr::str_view('xy', 'y'))
	expect_match(r, "x<span class='match'>y<\\/span>", fixed = TRUE, all = FALSE)
})

test_that('Dependencies work', {
	skip_if_not_installed('stringr')
	
	r <- repr_html(stringr::str_view('xy', 'y'))
	expect_match(r, '<script src="data:application/javascript', fixed = TRUE, all = FALSE)
})

test_that('The dependency manager works', {
	skip_if_not_installed('stringr')
	
	o <- options(repr.html.deduplicate = TRUE)
	on.exit(options(o))
	html_dependencies$clear()
	
	r <- repr_html(stringr::str_view('xy', 'y'))
	expect_match(r, '<meta charset="utf-8">\n\t\t<script', fixed = TRUE, all = FALSE)
	
	r <- repr_html(stringr::str_view('xy', 'y'))
	expect_match(r, '<meta charset="utf-8">\n\t\t\n', fixed = TRUE, all = FALSE)  #no deps here
})
