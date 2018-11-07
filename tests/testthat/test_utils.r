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

	fh <- file(f1)
	writeLines('one', fh)
	close(fh)

	fh <- file(f2)
	writeLines('two', fh)
  close(fh)

	files <- c(f1, f2)

	expect_equal(
		as.character(data_uris(mime = 'application/javascript', files = files[[1L]])),
		'data:application/javascript;base64,b25lCg=='
	)

	expect_equal(
		as.character(data_uris(mime = 'application/javascript', files = files)),
		c(
			'data:application/javascript;base64,b25lCg==',
			'data:application/javascript;base64,dHdvCg=='
		)
	)

	file.remove(f1)
	file.remove(f2)
})

