#' These options are used to control the behavior of repr when not calling it directly.
#' 
#' Once this package is loaded, all options are set to defaults which weren’t set beforehand.
#' 
#' Setting all options set to \code{NULL} are reset to defaults when reloading the package (or calling \code{repr:::.onload()}).
#' 
#' @section Options:
#' 
#' \describe{
#' 
#' \item{\code{repr.plot.*}}{
#' 	Those are for representations of \code{recordedplot} instances:
#' 	\describe{
#' 		\item{\code{repr.plot.width}}{Plotting area width in inches (default: 7)}
#' 		\item{\code{repr.plot.height}}{Plotting area height in inches (default: 7)}
#' 		\item{\code{repr.plot.pointsize}}{Text height in pt (default: 12)}
#' 		\item{\code{repr.plot.bg}}{Background color (default: white)}
#' 		\item{\code{repr.plot.antialias}}{Which kind of antialiasing to use for for lines and text? 'gray', 'subpixel' or 'none'? (default: gray)}
#' 		\item{\code{repr.plot.res}}{PPI for rasterization (default: 120)}
#' 		\item{\code{repr.plot.quality}}{Quality of JPEG format in \% (default: 90)}
#' 		\item{\code{repr.plot.family}}{Vector font family. 'sans', 'serif', 'mono' or a specific one (default: sans)}
#' 	}
#' }
#' \item{\code{repr.vector.quote}}{Output quotation marks for character vectors? (default: TRUE)}
#' \item{\code{repr.matrix.latex.colspec}}{How to layout LaTeX tables when representing matrices or data.frames. List of \code{row.head}, other \code{col}, and \code{end} strings. \code{end} mainly exists for when you want a vertical line there (default: 'r|', 'l', and '')}
#' \item{\code{repr.function.highlight}}{Use the \code{highr} package to insert highlighting instructions into the code? Needs that package to be installed. (default: FALSE)}
#' 
#' }
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
		repr.matrix.latex.colspec = list(row.head = 'r|', col = 'l', end = ''),
		repr.function.highlight = FALSE)
}