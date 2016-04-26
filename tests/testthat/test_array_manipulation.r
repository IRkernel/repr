
context('Array and vector truncation')

options('stringsAsFactors' = FALSE)

has_dt <- requireNamespace('data.table', quietly = TRUE)
has_dplyr <- requireNamespace('dplyr', quietly = TRUE)

#has_dt <- FALSE

test_that('max rows and cols are reasonable', {
  rows <- getOption('repr.matrix.max.rows')
  cols <- getOption('repr.matrix.max.cols')
  
  expect_equal(length(rows), 1L)
  expect_equal(length(cols), 1L)
  
  expect_false(is.null(rows))
  expect_false(is.na(rows))
  expect_false(is.null(rows))
  expect_false(is.na(rows))
  
  expect_true(rows >= 2L)
  expect_true(cols >= 2L)
  expect_true(rows < .Machine$integer.max)
  expect_true(cols < .Machine$integer.max)
})

test_that('get.limit.index sets the right indexes', {
  # Limit a hypothetical length-10 vector to 2.
  lim_index <- get.limit.index(10, 2)
  expect_equal(lim_index$begin, 1)
  expect_equal(lim_index$end, 10)
  
  # Limit a hypothetical length-10 vector to 3.
  lim_index <- get.limit.index(10, 3)
  expect_equal(lim_index$begin, c(1, 2))
  expect_equal(lim_index$end, 10)
  
  
  # Limit a hypothetical length-10 vector to 4.
  lim_index <- get.limit.index(10, 4)
  expect_equal(lim_index$begin, c(1, 2))
  expect_equal(lim_index$end, c(9, 10))
  
  # Limit a hypothetical length-10 vector to 9.
  lim_index <- get.limit.index(10, 9)
  expect_equal(lim_index$begin, c(1, 2, 3, 4, 5))
  expect_equal(lim_index$end, c(7, 8, 9, 10))  
})


test_that('ellip.limit.vec returns correctly for numerics', {

  test_vec <- 10:1
  lim <- 2
  limited_vec <- ellip.limit.vec(test_vec, lim, ellip.h)
  expect_equal(limited_vec, c('10', ellip.h, '1'))

  lim <- 5
  limited_vec <- ellip.limit.vec(test_vec, lim, ellip.h)
  expect_equal(limited_vec, c('10', '9', '8', ellip.h, '2', '1'))
  
  lim <- 6
  limited_vec <- ellip.limit.vec(test_vec, lim, ellip.h)
  expect_equal(limited_vec, c('10', '9', '8', ellip.h, '3', '2', '1'))
  
  
  lim <- 9
  limited_vec <- ellip.limit.vec(test_vec, lim, ellip.h)
  expect_equal(limited_vec, c('10', '9', '8', '7', '6', ellip.h, '4', '3', '2', '1'))
})


test_that('ellip.limit.vec returns correctly for factors', {
  test_vec <- factor(10:1)
  lim <- 2
  limited_vec <- ellip.limit.vec(test_vec, lim, ellip.h)
  expected_vec <- c('10', ellip.h, '1')
  expect_equal(as.character(limited_vec), expected_vec)

  lim <- 5
  limited_vec <- ellip.limit.vec(test_vec, lim, ellip.h)
  expected_vec <- c(10, 9, 8, ellip.h, 2, 1)
  expect_equal(as.character(limited_vec), expected_vec)
  
  lim <- 6
  limited_vec <- ellip.limit.vec(test_vec, lim, ellip.h)
  expected_vec <- c(10, 9, 8, ellip.h, 3, 2, 1)
  expect_equal(as.character(limited_vec), expected_vec)
  
  lim <- 9
  limited_vec <- ellip.limit.vec(test_vec, lim, ellip.h)
  expected_vec <- c(10, 9, 8, 7, 6, ellip.h, 4, 3, 2, 1)
  expect_equal(as.character(limited_vec), expected_vec)
})


test_that('ellip.limit.arr doesn\'t change arrays that are small', {
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
  test_df <- as.data.frame(test_mat)
  limited_mat <- ellip.limit.arr(test_mat)
  limited_df <- ellip.limit.arr(test_df)
  expect_equal(test_mat, limited_mat)
  expect_equal(test_df,  limited_df)
  if (has_dt) {
    test_dt <- data.table::as.data.table(test_mat)
    limited_dt <- ellip.limit.arr(test_dt)
    expect_equal(test_dt,  limited_dt)
  }
  if (has_dplyr) {
    test_tbl <- dplyr::as.tbl(test_df)
    limited_tbl <- ellip.limit.arr(test_tbl)
    expect_equal(test_tbl,  limited_tbl)
  }

  # Reset limits
  if (getOption('repr.matrix.max.rows') != orig_rows_limit) {
    options('repr.matrix.max.rows' = orig_rows_limit)
  }
  if (getOption('repr.matrix.max.cols') != orig_cols_limit) {
    options('repr.matrix.max.cols' = orig_cols_limit)
  }
})

test_that('ellip.limit.arr limits arrays that are wide (but not long)', {
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
  limited_mat <- ellip.limit.arr(test_mat)
  limited_df <- ellip.limit.arr(test_df)
  expected_mat <- matrix(c('16', '15', '14', '13', ellip.h, ellip.h, '4', '3', 
    '2', '1'), nrow = 2L)
  expected_df <- data.frame(V1 = c(16, 15), V2 = c(14, 13), ellips = rep(ellip.h, 2L),
    V7 = c(4, 3), V8 = c(2, 1))
  names(expected_df) <- c('V1', 'V2', ellip.h, 'V7', 'V8')
  expect_equal(limited_mat, expected_mat)
  expect_equal(limited_df,  expected_df)
  if (has_dt) {
    test_dt <- data.table::as.data.table(test_mat)
    limited_dt <- ellip.limit.arr(test_dt)
    # The code, as a shortcut, just converts data.tables to data.frames.
    #expected_dt <- data.table::as.data.table(expected_df)
    expected_dt <- expected_df
    expect_equal(limited_dt,  expected_dt)
  }
  if (has_dplyr) {
    test_tbl <- dplyr::as.tbl(test_df)
    limited_tbl <- ellip.limit.arr(test_tbl)
    expected_tbl <- expected_df  # curtailed tbl's get converted to data.frames.
    expect_equal(limited_tbl, expected_tbl)
  }

  # Repeat with an odd limit.
  options('repr.matrix.max.cols' = 5L)
  limited_mat <- ellip.limit.arr(test_mat)
  limited_df <- ellip.limit.arr(test_df)
  
  expected_mat <- matrix(c('16', '15', '14', '13', '12', '11', ellip.h, ellip.h, 
    '4', '3', '2', '1'), nrow = 2L)
  expected_df <- data.frame(V1 = c(16, 15), V2 = c(14, 13), V3 = c(12, 11), 
    ellips = rep(ellip.h, 2L), V7 = c(4, 3), V8 = c(2, 1))
  names(expected_df) <- c('V1', 'V2', 'V3', ellip.h, 'V7', 'V8')

  expect_equal(limited_mat, expected_mat)
  expect_equal(limited_df,  expected_df)
  if (has_dt) {
    limited_dt <- ellip.limit.arr(test_dt)
    # The code, as a shortcut, just converts data.tables to data.frames.
    #expected_dt <- data.table::as.data.table(expected_df)
    expected_dt <- expected_df
    expect_equal(limited_dt,  expected_dt)
  }
  if (has_dplyr) {
    limited_tbl <- ellip.limit.arr(test_tbl)
    expected_tbl <- expected_df  # curtailed tbl's get converted to data.frames.
    expect_equal(limited_tbl, expected_tbl)
  }
  
  # Reset limits
  if (getOption('repr.matrix.max.rows') != orig_rows_limit) {
    options('repr.matrix.max.rows' = orig_rows_limit)
  }
  if (getOption('repr.matrix.max.cols') != orig_cols_limit) {
    options('repr.matrix.max.cols' = orig_cols_limit)
  }
})

test_that('ellip.limit.arr limits arrays that are long (but not wide)', {
  # Make sure the limits are reasonable before we test.
  orig_rows_limit <- getOption('repr.matrix.max.rows')
  orig_cols_limit <- getOption('repr.matrix.max.cols')
  if (orig_cols_limit < 3L) {
    options('repr.matrix.max.cols' = 3L)
  }
  
  test_mat <- matrix(16:1, ncol = 2L)
  test_df <- as.data.frame(test_mat)

  options('repr.matrix.max.rows' = 4L)
  limited_mat <- ellip.limit.arr(test_mat)
  limited_df <- ellip.limit.arr(test_df)
  expected_mat <- matrix(c('16', '15', ellip.v, '10', '9', '8', '7', ellip.v, 
    '2', '1'), ncol = 2L)
  expected_df <- data.frame(V1 = c('16', '15', ellip.v, '10', '9'), 
    V2 = c('8', '7', ellip.v, '2', '1'))
  rownames(expected_df) <- c('1', '2', ellip.v, '7', '8')
  expect_equal(limited_mat, expected_mat)
  expect_equal(limited_df,  expected_df)
  if (has_dt) {
    test_dt <- data.table::as.data.table(test_mat)
    limited_dt <- ellip.limit.arr(test_dt)
    expected_dt <- expected_df
    expect_equal(limited_dt,  expected_dt)
  }
  if (has_dplyr) {
    test_tbl <- dplyr::as.tbl(test_df)
    limited_tbl <- ellip.limit.arr(test_tbl)
    expected_tbl <- expected_df  # curtailed tbl's get converted to data.frames.
    expect_equal(limited_tbl, expected_tbl)
  }
  
  # Repeat with an odd limit.
  options('repr.matrix.max.rows' = 5L)
  limited_mat <- ellip.limit.arr(test_mat)
  limited_df <- ellip.limit.arr(test_df)
  expected_mat <- matrix(c('16', '15', '14', ellip.v, '10', '9', '8', '7', '6', 
    ellip.v, '2', '1'), ncol = 2L)
  expected_df <- as.data.frame(expected_mat)
  rownames(expected_df) <- c('1', '2', '3', ellip.v, '7', '8')
  expected_tbl <- expected_df  # long tbl's get converted to data.frames.
  expect_equal(limited_mat, expected_mat)
  expect_equal(limited_df,  expected_df)
  if (has_dt) {
    # The code, as a shortcut, just converts data.tables to data.frames.
    #expected_dt <- data.table::as.data.table(expected_df)
    expected_dt <- expected_df
    limited_dt <- ellip.limit.arr(test_dt)
    expect_equal(limited_dt,  expected_dt)
  }
  if (has_dplyr) {
    limited_tbl <- ellip.limit.arr(test_tbl)
    expected_tbl <- expected_df  # curtailed tbl's get converted to data.frames.
    expect_equal(limited_tbl, expected_tbl)
  }
  
  # Reset limits
  if (getOption('repr.matrix.max.rows') != orig_rows_limit) {
    options('repr.matrix.max.rows' = orig_rows_limit)
  }
  if (getOption('repr.matrix.max.cols') != orig_cols_limit) {
    options('repr.matrix.max.cols' = orig_cols_limit)
  }
})


test_that('ellip.limit.arr limits arrays that are long and wide', {

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
  limited_mat <- ellip.limit.arr(test_mat)
  limited_df <- ellip.limit.arr(test_df)
  expected_mat <- matrix(c('1', '2', ellip.v, '6', '7', '8', '9', ellip.v, '13',
    '14', ellip.h, ellip.h, ellip.d, ellip.h, ellip.h, '36', '37',
    ellip.v, '41', '42', '43', '44', ellip.v, '48', '49'), nrow = 5L)
  expected_df <- as.data.frame(expected_mat)
  expected_df[, 3] <- factor(expected_df[, 3], levels = ellipses)
  names(expected_df) <- c('V1', 'V2', ellip.h, 'V6', 'V7')
  rownames(expected_df) <- c('1', '2', ellip.v, '6', '7')

  expect_equal(limited_mat, expected_mat)
  expect_equal(limited_df,  expected_df)
  if (has_dt) {
    test_dt <- data.table::as.data.table(test_mat)
    limited_dt <- ellip.limit.arr(test_dt)
    # The code, as a shortcut, just converts data.tables to data.frames.
    #expected_dt <- data.table::as.data.table(expected_df)
    expected_dt <- expected_df
    expect_equal(limited_dt,  expected_dt)
  }
  if (has_dplyr) {
    test_tbl <- dplyr::as.tbl(test_df)
    limited_tbl <- ellip.limit.arr(test_tbl)
    expected_tbl <- expected_df  # curtailed tbl's get converted to data.frames.
    expect_equal(limited_tbl, expected_tbl)
  }
  options('repr.matrix.max.rows' = 5L)
  options('repr.matrix.max.cols' = 5L)
  limited_mat <- ellip.limit.arr(test_mat)
  limited_df <- ellip.limit.arr(test_df)
  expected_mat <- matrix(c('1', '2', '3', ellip.v, '6', '7', '8', '9', '10',
    ellip.v, '13', '14', '15', '16', '17', ellip.v, '20', '21', ellip.h,
    ellip.h, ellip.h, ellip.d, ellip.h, ellip.h, '36', '37', '38', ellip.v,'41',
    '42', '43', '44', '45', ellip.v, '48', '49'), nrow = 6L)
  expected_df <- as.data.frame(expected_mat)
  expected_df[, 4] <- factor(expected_df[, 4], levels = ellipses)
  names(expected_df) <- c('V1', 'V2', 'V3', ellip.h, 'V6', 'V7')
  rownames(expected_df) <- c('1', '2', '3', ellip.v, '6', '7')

  expect_equal(limited_mat, expected_mat)
  expect_equal(limited_df,  expected_df)
  if (has_dt) {
    limited_dt <- ellip.limit.arr(test_dt)
    # The code, as a shortcut, just converts data.tables to data.frames.
    #expected_dt <- data.table::as.data.table(expected_df)
    expected_dt <- expected_df
    expect_equal(limited_dt,  expected_dt)
  }
  if (has_dplyr) {
    limited_tbl <- ellip.limit.arr(test_tbl)
    expected_tbl <- expected_df  # curtailed tbl's get converted to data.frames.
    expect_equal(limited_tbl, expected_tbl)
  }
  options('repr.matrix.max.rows' = 6L)
  options('repr.matrix.max.cols' = 6L)
  limited_mat <- ellip.limit.arr(test_mat)
  limited_df <- ellip.limit.arr(test_df)

  expected_mat <- matrix(c('1', '2', '3', ellip.v, '5', '6', '7', '8', '9',
    '10', ellip.v, '12', '13', '14', '15', '16', '17', ellip.v, '19', '20',
    '21', ellip.h,  ellip.h, ellip.h, ellip.d, ellip.h, ellip.h, ellip.h, '29', '30', '31', ellip.v, '33', '34', '35', '36', '37', '38', ellip.v, '40',
    '41', '42', '43', '44', '45', ellip.v, '47', '48', '49'), nrow = 7L)
  expected_df <- as.data.frame(expected_mat, stringsAsFactors = FALSE)
  expected_df[, 4] <- factor(expected_df[, 4], levels = ellipses)
  names(expected_df) <- c('V1', 'V2', 'V3', ellip.h, 'V5', 'V6', 'V7')
  rownames(expected_df) <- c('1', '2', '3', ellip.v, '5', '6', '7')

  expect_equal(limited_mat, expected_mat)
  expect_equal(limited_df,  expected_df)
  if (has_dt) {
    limited_dt <- ellip.limit.arr(test_dt)
    # The code, as a shortcut, just converts data.tables to data.frames.
    #expected_dt <- data.table::as.data.table(expected_df)
    expected_dt <- expected_df
    expect_equal(limited_dt,  expected_dt)
  }
  if (has_dplyr) {
    limited_tbl <- ellip.limit.arr(test_tbl)
    expected_tbl <- expected_df  # curtailed tbl's get converted to data.frames.
    expect_equal(limited_tbl, expected_tbl)
  }

  # Reset limits
  if (getOption('repr.matrix.max.rows') != orig_rows_limit) {
    options('repr.matrix.max.rows' = orig_rows_limit)
  }
  if (getOption('repr.matrix.max.cols') != orig_cols_limit) {
    options('repr.matrix.max.cols' = orig_cols_limit)
  }
})
