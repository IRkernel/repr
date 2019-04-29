# all files called helper*.r are sourced before the tests

expect_id_text <- function(object, expected) {
	#obj_orig <- object
	#exp_orig <- expected
	one_element <- length(object) == 1L && length(expected) == 1L
	if (one_element) {
		object   <- strsplit(object,   '\n')[[1]]
		expected <- strsplit(expected, '\n')[[1]]
	}
	if (!identical(object, expected)) {
		dif <- diffobj::diffChr(object, expected, format = 'ansi256', ignore.white.space = FALSE, interactive = FALSE)
		fail_msg <- as.character(dif)
		if (!one_element) fail_msg <- paste(format(seq_along(fail_msg)), fail_msg)
		expect(!any(dif), fail_msg)
	}
}

expect_equivalent_string <- function(result, expectation){
	"Only use ' as a string delimiter in strings."
	expect_id_text(gsub('"', "'", x = result), expectation)
}
