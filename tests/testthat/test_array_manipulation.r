context('Array and vector truncation')

options(stringsAsFactors = FALSE)

has_dt <- requireNamespace('data.table', quietly = TRUE)
has_dplyr <- requireNamespace('dplyr', quietly = TRUE)

#has_dt <- FALSE

test_that('max rows and cols are reasonable', {
  rows <- getOption('repr.matrix.max.rows')
  cols <- getOption('repr.matrix.max.cols')
  
  expect_identical(length(rows), 1L)
  expect_identical(length(cols), 1L)
  
  expect_false(is.null(rows))
  expect_false(is.na(rows))
  expect_false(is.null(rows))
  expect_false(is.na(rows))
  
  expect_true(rows >= 2L)
  expect_true(cols >= 2L)
  expect_true(rows < .Machine$integer.max)
  expect_true(cols < .Machine$integer.max)
})


test_that('ellip_limit_arr doesn\'t change arrays that are small', {
  # Make sure the limits are reasonable before we test.
  orig_rows_limit <- getOption('repr.matrix.max.rows')
	orig_cols_limit <- getOption('repr.matrix.max.cols')
  if (orig_rows_limit < 6L) {
    options('repr.matrix.max.rows' = 6L)
  } 
  if (orig_cols_limit < 3L) {
    options('repr.matrix.max.cols' = 3L)
  }
  
  # Run some tests.
  test_mat <- matrix(1:10, ncol = 2)
  test_df <- data.frame(V1 = 1:5, V2 = 6:10)
  expected_mat <- matrix(c(format(1:5), format(6:10)), ncol = 2)
  expected_df_mat <- structure(expected_mat, dimnames = list(1:5, c('V1', 'V2')))
  limited_mat <- ellip_limit_arr(test_mat)
  limited_df_mat <- ellip_limit_arr(test_df)
  expect_identical(limited_mat, expected_mat)
  expect_identical(limited_df_mat, expected_df_mat)
  if (has_dt) {
    test_dt <- data.table::as.data.table(test_mat)
    limited_dt_mat <- ellip_limit_arr(test_dt)
    expect_identical(limited_dt_mat, expected_df_mat)
  }
  if (has_dplyr) {
    test_tbl <- dplyr::as.tbl(test_df)
    limited_tbl_mat <- ellip_limit_arr(test_tbl)
    expect_identical(limited_tbl_mat, expected_df_mat)
  }

  # Reset limits
  if (getOption('repr.matrix.max.rows') != orig_rows_limit) {
    options('repr.matrix.max.rows' = orig_rows_limit)
  }
  if (getOption('repr.matrix.max.cols') != orig_cols_limit) {
    options('repr.matrix.max.cols' = orig_cols_limit)
  }
})

test_that('ellip_limit_arr limits arrays that are wide (but not long)', {
  # Make sure the limits are reasonable before we test.
  orig_rows_limit <- getOption('repr.matrix.max.rows')
  orig_cols_limit <- getOption('repr.matrix.max.cols')
  if (orig_rows_limit < 6L) {
    options('repr.matrix.max.rows' = 6L)
  } 
  
  
  test_mat <- matrix(16:1, nrow = 2L)
  test_df <- as.data.frame(test_mat)

  # We'll test even and odd limits, sticking with small numbers to keep things sane.
  options('repr.matrix.max.cols' = 4L)
  limited_mat <- ellip_limit_arr(test_mat)
  limited_df  <- ellip_limit_arr(test_df)
  expected_mat <- matrix(c('16', '15', '14', '13', ellip_h, ellip_h, '4', '3', '2', '1'), nrow = 2L)
  expected_df_mat  <- as.matrix(data.frame(V1 = c(16, 15), V2 = c(14, 13), ellips = rep(ellip_h, 2L), V7 = c(4, 3), V8 = c(2, 1)))
  colnames(expected_df_mat)[[3]] <- ellip_h
  rownames(expected_df_mat) <- 1:2  # TODO: is this correct or should it rather not have those
  expect_identical(limited_mat, expected_mat)
  expect_identical(limited_df,  expected_df_mat)
  if (has_dt) {
    test_dt <- data.table::as.data.table(test_mat)
    limited_dt <- ellip_limit_arr(test_dt)
    expect_identical(limited_dt, expected_df_mat)
  }
  if (has_dplyr) {
    test_tbl <- dplyr::as.tbl(test_df)
    limited_tbl <- ellip_limit_arr(test_tbl)
    expect_identical(limited_tbl, expected_df_mat)
  }

  # Repeat with an odd limit.
  options('repr.matrix.max.cols' = 5L)
  limited_mat <- ellip_limit_arr(test_mat)
  limited_df <- ellip_limit_arr(test_df)
  
  expected_mat <- matrix(c('16', '15', '14', '13', '12', '11', ellip_h, ellip_h, '4', '3', '2', '1'), nrow = 2L)
  expected_df_mat <- as.matrix(data.frame(V1 = c(16, 15), V2 = c(14, 13), V3 = c(12, 11), ellips = rep(ellip_h, 2L), V7 = c(4, 3), V8 = c(2, 1)))
  colnames(expected_df_mat)[[4]] <- ellip_h
  rownames(expected_df_mat) <- 1:2  # TODO: see above

  expect_identical(limited_mat, expected_mat)
  expect_identical(limited_df,  expected_df_mat)
  if (has_dt) {
    limited_dt <- ellip_limit_arr(test_dt)
    expect_identical(limited_dt, expected_df_mat)
  }
  if (has_dplyr) {
    limited_tbl <- ellip_limit_arr(test_tbl)
    expect_identical(limited_tbl, expected_df_mat)
  }
  
  # Reset limits
  if (getOption('repr.matrix.max.rows') != orig_rows_limit) {
    options('repr.matrix.max.rows' = orig_rows_limit)
  }
  if (getOption('repr.matrix.max.cols') != orig_cols_limit) {
    options('repr.matrix.max.cols' = orig_cols_limit)
  }
})

test_that('ellip_limit_arr limits arrays that are long (but not wide)', {
  # Make sure the limits are reasonable before we test.
  orig_rows_limit <- getOption('repr.matrix.max.rows')
  orig_cols_limit <- getOption('repr.matrix.max.cols')
  if (orig_cols_limit < 3L) {
    options('repr.matrix.max.cols' = 3L)
  }
  
  test_mat <- matrix(16:1, ncol = 2L)
  test_df <- as.data.frame(test_mat)

  options('repr.matrix.max.rows' = 4L)
  limited_mat <- ellip_limit_arr(test_mat)
  limited_df <- ellip_limit_arr(test_df)
  expected_mat <- matrix(c(
  	'16', '15', ellip_v, '10', ' 9',
  	'8', '7', ellip_v, '2', '1'), ncol = 2L)
  expected_df_mat <- as.matrix(data.frame(
  	V1 = c('16', '15', ellip_v, '10', ' 9'),
  	V2 = c('8', '7', ellip_v, '2', '1')))
  rownames(expected_df_mat) <- c('1', '2', ellip_v, '7', '8')
  expect_identical(limited_mat, expected_mat)
  expect_identical(limited_df,  expected_df_mat)
  if (has_dt) {
    test_dt <- data.table::as.data.table(test_mat)
    limited_dt <- ellip_limit_arr(test_dt)
    expect_identical(limited_dt, expected_df_mat)
  }
  if (has_dplyr) {
    test_tbl <- dplyr::as.tbl(test_df)
    limited_tbl <- ellip_limit_arr(test_tbl)
    expect_identical(limited_tbl, expected_df_mat)
  }
  
  # Repeat with an odd limit.
  options('repr.matrix.max.rows' = 5L)
  limited_mat <- ellip_limit_arr(test_mat)
  limited_df <- ellip_limit_arr(test_df)
  expected_mat <- matrix(c(
  	'16', '15', '14', ellip_v, '10', ' 9',
  	'8', '7', '6', ellip_v, '2', '1'), ncol = 2L)
  expected_df_mat <- as.matrix(data.frame(
  	V1 = c('16', '15', '14', ellip_v, '10', ' 9'),
  	V2 = c('8', '7', '6', ellip_v, '2', '1')))
  rownames(expected_df_mat) <- c('1', '2', '3', ellip_v, '7', '8')
  expect_identical(limited_mat, expected_mat)
  expect_identical(limited_df,  expected_df_mat)
  if (has_dt) {
    limited_dt <- ellip_limit_arr(test_dt)
    expect_identical(limited_dt,  expected_df_mat)
  }
  if (has_dplyr) {
    limited_tbl <- ellip_limit_arr(test_tbl)
    expect_identical(limited_tbl, expected_df_mat)
  }
  
  # Reset limits
  if (getOption('repr.matrix.max.rows') != orig_rows_limit) {
    options('repr.matrix.max.rows' = orig_rows_limit)
  }
  if (getOption('repr.matrix.max.cols') != orig_cols_limit) {
    options('repr.matrix.max.cols' = orig_cols_limit)
  }
})

test_that('ellip_limit_arr preserves rownames when limiting rows', {
	# Make sure the limits are reasonable before we test.
	orig_rows_limit <- getOption('repr.matrix.max.rows')
	orig_cols_limit <- getOption('repr.matrix.max.cols')
	if (orig_cols_limit < 3L) {
		options('repr.matrix.max.cols' = 3L)
	}
	
	test_mat <- matrix(16:1, ncol = 2L, dimnames = list(letters[1:8], NULL))
	test_df <- as.data.frame(test_mat)
	
	options('repr.matrix.max.rows' = 4L)
	limited_mat <- ellip_limit_arr(test_mat)
	limited_df <- ellip_limit_arr(test_df)
	expected_rownames <- c(letters[1:2], ellip_v, letters[7:8])
	expected_mat <- matrix(c(
		'16', '15', ellip_v, '10', ' 9',
		'8', '7', ellip_v, '2', '1'), ncol = 2L, dimnames = list(expected_rownames, NULL))
	expected_df_mat <- as.matrix(data.frame(
		V1 = c('16', '15', ellip_v, '10', ' 9'),
		V2 = c('8', '7', ellip_v, '2', '1'), row.names = expected_rownames))
	expect_identical(limited_mat, expected_mat)
	expect_identical(limited_df,  expected_df_mat)
	if (has_dt) {
		test_dt <- data.table::as.data.table(test_mat)
		rownames(test_dt) <- rownames(test_mat)  # force keeping rownames
		limited_dt <- ellip_limit_arr(test_dt)
		expect_identical(limited_dt, expected_df_mat)
	}
	if (has_dplyr) {
		test_tbl <- dplyr::as.tbl(test_df)
		# tbl removes rownames, so we have to reset them
		rownames(test_tbl) <- rownames(test_df)
		limited_tbl <- ellip_limit_arr(test_tbl)
		expect_identical(limited_tbl, expected_df_mat)
	}
	
	# Repeat with an odd limit.
	options('repr.matrix.max.rows' = 5L)
	limited_mat <- ellip_limit_arr(test_mat)
	limited_df <- ellip_limit_arr(test_df)
	expected_rownames <- c(letters[1:3], ellip_v, letters[7:8])
	expected_mat <- matrix(c(
		'16', '15', '14', ellip_v, '10', ' 9',
		'8', '7', '6', ellip_v, '2', '1'), ncol = 2L, dimnames = list(expected_rownames, NULL))
	expected_df_mat <- as.matrix(data.frame(
		V1 = c('16', '15', '14', ellip_v, '10', ' 9'),
		V2 = c('8', '7', '6', ellip_v, '2', '1'), row.names = expected_rownames))
	expect_identical(limited_mat, expected_mat)
	expect_identical(limited_df,  expected_df_mat)
	if (has_dt) {
		limited_dt <- ellip_limit_arr(test_dt)
		expect_identical(limited_dt,  expected_df_mat)
	}
	if (has_dplyr) {
		limited_tbl <- ellip_limit_arr(test_tbl)
		expect_identical(limited_tbl, expected_df_mat)
	}
	
	# Reset limits
	if (getOption('repr.matrix.max.rows') != orig_rows_limit) {
		options('repr.matrix.max.rows' = orig_rows_limit)
	}
	if (getOption('repr.matrix.max.cols') != orig_cols_limit) {
		options('repr.matrix.max.cols' = orig_cols_limit)
	}
})

test_that('ellip_limit_arr limits arrays that are long and wide', {

  # Make sure the limits are reasonable before we test.
  orig_rows_limit <- getOption('repr.matrix.max.rows')
  orig_cols_limit <- getOption('repr.matrix.max.cols')
  
  
  # Make a 7x7 because I want to test with limits of 4, 5 and 6. I want to test
  # both the normal cases and the weird case where a dimension is one less than
  # the limit (and therefore the 'smaller' output array is actually the same dim
  # as the original)
  test_mat <- matrix(1:49, ncol = 7)
  test_df <- as.data.frame(test_mat)

  # We'll test with even and odd limits (but not all combinations of the two)
  # Test with small numbers to keep things reasonable.
  options('repr.matrix.max.rows' = 4L)
  options('repr.matrix.max.cols' = 4L)
  limited_mat <- ellip_limit_arr(test_mat)
  limited_df <- ellip_limit_arr(test_df)
  expected_mat <- matrix(c(
  	'1', '2', ellip_v, '6', '7',
  	'8', '9', ellip_v, '13', '14',
  	ellip_h, ellip_h, ellip_d, ellip_h, ellip_h,
  	'36', '37', ellip_v, '41', '42',
  	'43', '44', ellip_v, '48', '49'), nrow = 5L)
  expected_df_mat <- as.matrix(as.data.frame(expected_mat))
  colnames(expected_df_mat) <- c('V1', 'V2', ellip_h, 'V6', 'V7')
  rownames(expected_df_mat) <- c('1', '2', ellip_v, '6', '7')

  expect_identical(limited_mat, expected_mat)
  expect_identical(limited_df,  expected_df_mat)
  if (has_dt) {
    test_dt <- data.table::as.data.table(test_mat)
    limited_dt <- ellip_limit_arr(test_dt)
    expect_identical(limited_dt,  expected_df_mat)
  }
  if (has_dplyr) {
    test_tbl <- dplyr::as.tbl(test_df)
    limited_tbl <- ellip_limit_arr(test_tbl)
    expect_identical(limited_tbl, expected_df_mat)
  }
  options('repr.matrix.max.rows' = 5L)
  options('repr.matrix.max.cols' = 5L)
  limited_mat <- ellip_limit_arr(test_mat)
  limited_df <- ellip_limit_arr(test_df)
  expected_mat <- matrix(c(
  	'1', '2', '3', ellip_v, '6', '7',
  	' 8', ' 9', '10', ellip_v, '13', '14',
  	'15', '16', '17', ellip_v, '20', '21',
  	ellip_h, ellip_h, ellip_h, ellip_d, ellip_h, ellip_h,
  	'36', '37', '38', ellip_v,'41', '42',
  	'43', '44', '45', ellip_v, '48', '49'), nrow = 6L)
  expected_df_mat <- as.matrix(as.data.frame(expected_mat))
  colnames(expected_df_mat) <- c('V1', 'V2', 'V3', ellip_h, 'V6', 'V7')
  rownames(expected_df_mat) <- c('1', '2', '3', ellip_v, '6', '7')

  expect_identical(limited_mat, expected_mat)
  expect_identical(limited_df,  expected_df_mat)
  if (has_dt) {
    limited_dt <- ellip_limit_arr(test_dt)
    expect_identical(limited_dt,  expected_df_mat)
  }
  if (has_dplyr) {
    limited_tbl <- ellip_limit_arr(test_tbl)
    expect_identical(limited_tbl, expected_df_mat)
  }
  options('repr.matrix.max.rows' = 6L)
  options('repr.matrix.max.cols' = 6L)
  limited_mat <- ellip_limit_arr(test_mat)
  limited_df <- ellip_limit_arr(test_df)

  expected_mat <- matrix(c(
  	'1', '2', '3', ellip_v, '5', '6', '7',
  	' 8', ' 9', '10', ellip_v, '12', '13', '14',
  	'15', '16', '17', ellip_v, '19', '20', '21',
  	ellip_h,  ellip_h, ellip_h, ellip_d, ellip_h, ellip_h, ellip_h,
  	'29', '30', '31', ellip_v, '33', '34', '35',
  	'36', '37', '38', ellip_v, '40', '41', '42',
  	'43', '44', '45', ellip_v, '47', '48', '49'), nrow = 7L)
  expected_df_mat <- as.matrix(as.data.frame(expected_mat, stringsAsFactors = FALSE))
  colnames(expected_df_mat) <- c('V1', 'V2', 'V3', ellip_h, 'V5', 'V6', 'V7')
  rownames(expected_df_mat) <- c('1', '2', '3', ellip_v, '5', '6', '7')

  expect_identical(limited_mat, expected_mat)
  expect_identical(limited_df,  expected_df_mat)
  if (has_dt) {
    limited_dt <- ellip_limit_arr(test_dt)
    expect_identical(limited_dt,  expected_df_mat)
  }
  if (has_dplyr) {
    limited_tbl <- ellip_limit_arr(test_tbl)
    expect_identical(limited_tbl, expected_df_mat)
  }

  # Reset limits
  if (getOption('repr.matrix.max.rows') != orig_rows_limit) {
    options('repr.matrix.max.rows' = orig_rows_limit)
  }
  if (getOption('repr.matrix.max.cols') != orig_cols_limit) {
    options('repr.matrix.max.cols' = orig_cols_limit)
  }
})
