str_view <- function(...) {
	skip_if_not_installed('stringr')
	skip_if_not_installed('htmlwidgets')
	
	if (packageVersion("stringr") >= "1.4.1.9000") {
		stringr::str_view(..., html = TRUE)
	} else {
		stringr::str_view(...)
	}
}
