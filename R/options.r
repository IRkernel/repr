#' These options are used to control the behavior of repr when not calling it directly.
#' 
#' Once this package is loaded, all options are set to defaults which weren’t set beforehand.
#' 
#' Setting all options set to \code{NULL} are reset to defaults when reloading the package (or calling \code{repr:::onload()}).
#'
#' @name options
NULL

condopt <- function(...) {
	opt.defaults <- list(...)
	for (opt.name in names(opt.defaults)) {
		if (is.null(getOption(opt.name)))
			do.call(options, opt.defaults[opt.name])  # single []: name stays
	}
}

.onLoad <- function(libname = NULL, pkgname = NULL) {
	condopt(
		repr.plot.width     = 7,
		repr.plot.height    = 7,
		repr.plot.pointsize = 12,
		repr.plot.bg        = 'white',
		repr.plot.antialias = 'gray',
		#nice medium-res DPI
		repr.plot.res       = 120,
		#jpeg quality bumped from default
		repr.plot.quality   = 90,
		#vector font family
		repr.plot.family    = 'sans')
	
	condopt(
		repr.vector.quote = TRUE,
		repr.matrix.latex.colspec = list(row.head = 'r|', col = 'l', end = ''))
}