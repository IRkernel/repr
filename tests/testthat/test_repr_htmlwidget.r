context('reprs of html widgets and tags')

test_that('A basic tag works', {
	r <- repr_html(htmltools::div())
	expect_identical(r, sprintf(repr:::HTML_SKELETON, '', '<div></div>'))
})

test_that('A basic widget works', {
	skip_if_not_installed('stringr')
	skip_if_not_installed('htmlwidgets')
	
	r <- repr_html(stringr::str_view('xy', 'y'))
	expect_match(r, "x<span class='match'>y<\\/span>", fixed = TRUE, all = FALSE)
})

test_that('Dependencies work', {
	skip_if_not_installed('stringr')
	skip_if_not_installed('htmlwidgets')
	
	r <- repr_html(stringr::str_view('xy', 'y'))
	expect_match(r, '<script title="htmlwidgets" src="data:application/javascript', fixed = TRUE, all = FALSE)
})

test_that('The dependency manager works', {
	skip_if_not_installed('stringr')
	skip_if_not_installed('htmlwidgets')
	
	o <- options(repr.html.deduplicate = TRUE)
	on.exit(options(o))
	html_dependencies$clear()
	
	r <- repr_html(stringr::str_view('xy', 'y'))
	expect_match(r, '<meta charset="utf-8">\n\t\t<script', fixed = TRUE, all = FALSE)
	
	r <- repr_html(stringr::str_view('xy', 'y'))
	expect_match(r, '<meta charset="utf-8">\n\t\t\n', fixed = TRUE, all = FALSE)  #no deps here
})

test_that('Leaflet HTML and deps can be represented', {
	skip_if_not_installed('leaflet')

	leaf <- leaflet::addTiles(leaflet::leaflet())

	r <- repr_html(leaf)
	expect_match(r, '<script title="htmlwidgets" src="data:')
})
