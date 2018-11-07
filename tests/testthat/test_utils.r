context('Utility methods')

test_that('Extra arguments to data_uris error', {
	expect_error(
		data_uris('one', mime = 'text/html'),
		'length(list(...)) == 0L is not TRUE',
		fixed = TRUE
	)
})

test_that('data_uris works with one or multiple files', {
	f1 <- tempfile(fileext = '.js')
	f2 <- tempfile(fileext = '.js')

	fh <- file(f1, 'wb')
	writeBin(charToRaw('one'), fh)
	close(fh)

	fh <- file(f2, 'wb')
	writeBin(charToRaw('two'), fh)
  close(fh)

	files <- c(f1, f2)

	expect_equal(
		as.character(data_uris(mime = 'text/plain', files = files[[1L]])),
		'data:text/plain;base64,b25l'
	)

	expect_equal(
		as.character(data_uris(mime = 'text/plain', files = files)),
		paste0('data:text/plain;base64,', c('b25l', 'dHdv'))
	)

	file.remove(f1)
	file.remove(f2)
})

