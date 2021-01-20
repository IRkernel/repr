#' repr options
#' 
#' These options are used to control the behavior of repr when not calling it directly. Use \code{\link[base]{options}(repr.* = ...)} and \code{\link[base]{getOption}('repr.*')} to set and get them, respectively.
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
#' 		\item{\code{repr.plot.width}}{Plotting area width in inches (default: \code{7})}
#' 		\item{\code{repr.plot.height}}{Plotting area height in inches (default: \code{7})}
#' 		\item{\code{repr.plot.pointsize}}{Text height in pt (default: \code{12})}
#' 		\item{\code{repr.plot.bg}}{Background color (default: \code{'white'})}
#' 		\item{\code{repr.plot.antialias}}{
#' 		  Which kind of antialiasing to use for for lines and text?
#' 		  'gray', 'subpixel' or 'none'?
#' 		  (default: \code{'gray'})
#' 		}
#' 		\item{\code{repr.plot.res}}{PPI for rasterization (default: \code{120})}
#' 		\item{\code{repr.plot.quality}}{Quality of JPEG format in \% (default: \code{90})}
#' 		\item{\code{repr.plot.family}}{Font family. 'sans', 'serif', 'mono' or a specific one (default: \code{'sans'})}
#' 		\item{\code{repr.plot.backends}}{
#' 		  The order in which backends are tried.
#' 		  \describe{
#' 		    \item{\link[ragg:ragg]{ragg}}{Can emit only raster formats (png, jpeg), good quality}
#' 		    \item{\link[Cairo:Cairo]{Cairo}}{Cannot handle per-element fonts, excellent quality}
#' 		    \item{\link[grDevices:Devices]{grDevices}}{OK quality}
#' 		  }
#' 		  (default: \code{c('ragg', 'Cairo', 'grDevices')})
#' 		}
#' 	}
#' }
#' \item{\code{repr.vector.quote}}{
#' 	Output quotation marks for character vectors? (default: TRUE)
#' }
#' \item{\code{repr.vector.max.items}}{
#' 	How many items to display at max. Will insert an item with a horizontal ellipsis to show elision. (default: 400)
#' }
#' \item{\code{repr.matrix.max.rows}}{
#' 	How many rows to display at max. Will insert a row with vertical ellipses to show elision. (default: 60)
#' }
#' \item{\code{repr.matrix.max.cols}}{
#' 	How many cols to display at max. Will insert a column with horizontal ellipses to show elision. (default: 20)
#' }
#' \item{\code{repr.matrix.latex.colspec}}{
#' 	How to layout LaTeX tables when representing matrices or data.frames.
#' 	List of \code{row.head}, other \code{col}, and \code{end} strings.
#' 	\code{end} mainly exists for when you want a vertical line there (default: 'r|', 'l', and '')
#' }
#' \item{\code{repr.function.highlight}}{
#'  Use the \code{highr} package to insert highlighting instructions into the code? Needs that package to be installed. (default: FALSE)
#' }
#' \item{\code{repr.html.deduplicate}}{
#'  Use the \link{html_dependencies} manager to only include dependencies once? This can greatly reduce notebook size, but fails if e.g. iframes are used (default: FALSE)
#' }
#' 
#' }
#'
#' @name repr-options
NULL

plot_defaults <- list(
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

class_defaults <- list(
	repr.vector.quote = TRUE,
	repr.vector.max.items = 400L,
	repr.matrix.max.rows = 60L,
	repr.matrix.max.cols = 20L,
	repr.matrix.latex.colspec = list(row_head = 'r|', col = 'l', end = ''),
	repr.function.highlight = FALSE,
	repr.html.deduplicate = FALSE)

#' @name repr-options
#' @export
repr_option_defaults <- c(plot_defaults, class_defaults)

.onLoad <- function(libname = NULL, pkgname = NULL) {
	for (opt_name in names(repr_option_defaults)) {
		if (is.null(getOption(opt_name)))
			do.call(options, repr_option_defaults[opt_name])  # single []: name stays
	}
}
