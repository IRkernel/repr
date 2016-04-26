CAIRO_INSTALLED <- requireNamespace('Cairo')

plot_title <- function(p, default = NULL) {
	for (call in rev(p[[1]])) {
		args <- call[[2]]
		if (isTRUE(args[[1]]$name == 'C_title') && !is.null(args[[2]])) {
			return(args[[2]])
		}
	}
	default
}

#' Plot representations
#' 
#' \code{repr_text.recordedplot} only returns a small info string containing the title (if any)
#' while the others return a character vector (SVG) a raw vector (the rest) containing the image data.
#' 
#' All parameters can also be specified using the eponymous \code{repr.plot.*} \link{repr-options}.
#' 
#' @param obj  The plot to create a representation for
#' @param width  Plot area width in inches (default: 7)
#' @param height  Plot area height in inches (default: 7)
#' @param bg  Background color (default: white)
#' @param pointsize  Text height in pt (default: 12)
#' @param antialias  Which kind of antialiasing to use for for lines and text? 'gray', 'subpixel' or 'none'? (default: gray)
#' @param res  For PNG and JPEG, specifies the PPI for rasterization (default: 120)
#' @param quality  For JPEG, determines the compression quality in \% (default: 90)
#' @param family  Font family for SVG and PDF. 'sans', 'serif', 'mono' or a specific one (default: sans)
#' @param ...  ignored
#' 
#' @examples
#' dev.new()
#' dev.control(displaylist = 'enable')
#' plot(sqrt, main = 'Square root')
#' p <- recordPlot()
#' dev.off()
#' 
#' repr_text(p)
#' 
#' @aliases repr_text.recordedplot repr_png.recordedplot repr_jpg.recordedplot repr_svg.recordedplot repr_pdf.recordedplot
#' @name repr_*.recordedplot
#' @export
repr_text.recordedplot <- function(obj, ...) {
	title <- plot_title(obj)
	if (is.null(title)) {
		'plot without title'
	} else {
		sprintf('Plot with title %s', dQuote(title))
	}
}

repr_recordedplot_generic <- function(obj, ext, binary, dev.cb) {
	tf <- tempfile(fileext = ext)
	dev.cb(tf)
	grDevices::replayPlot(obj)
	grDevices::dev.off()
	if (binary)
		readBin(tf, raw(), file.info(tf)$size)
	else
		readChar(tf, file.info(tf)$size, useBytes = TRUE)
}


### BITMAPS ###


#' @name repr_*.recordedplot
#' @export
repr_png.recordedplot <- function(obj,
	width     = getOption('repr.plot.width'),
	height    = getOption('repr.plot.height'),
	bg        = getOption('repr.plot.bg'),
	pointsize = getOption('repr.plot.pointsize'),
	antialias = getOption('repr.plot.antialias'),
	#special
	res       = getOption('repr.plot.res'),
...) {
	if (!CAIRO_INSTALLED && !any(capabilities(c('aqua', 'cairo', 'X11', 'png')))) return(NULL)
	
	dev.cb <- function(tf)
		if (CAIRO_INSTALLED)
			Cairo::Cairo(width, height, tf, 'png', pointsize, bg, 'transparent', 'in', res)
		else
			grDevices::png(tf, width, height, 'in', pointsize, bg, res, antialias = antialias)
	
	repr_recordedplot_generic(obj, '.png', TRUE, dev.cb)
}

#' @name repr_*.recordedplot
#' @export
repr_jpg.recordedplot <- function(obj,
	width     = getOption('repr.plot.width'),
	height    = getOption('repr.plot.height'),
	bg        = getOption('repr.plot.bg'),
	pointsize = getOption('repr.plot.pointsize'),
	antialias = getOption('repr.plot.antialias'),
	#special
	res       = getOption('repr.plot.res'),
	quality   = getOption('repr.plot.quality'),
...) {
	if (!CAIRO_INSTALLED && !any(capabilities(c('aqua', 'cairo', 'X11', 'jpeg')))) return(NULL)
	
	dev.cb <- function(tf)
		if (CAIRO_INSTALLED)
			Cairo::Cairo(width, height, tf, 'jpeg', pointsize, bg, 'transparent', 'in', res, quality = quality)
		else
			grDevices::jpeg(tf, width, height, 'in', pointsize, quality, bg, res, antialias = antialias)
	
	repr_recordedplot_generic(obj, '.jpg', TRUE, dev.cb)
}


### VECTOR ###


#' @name repr_*.recordedplot
#' @export
repr_svg.recordedplot <- function(obj,
	width     = getOption('repr.plot.width'),
	height    = getOption('repr.plot.height'),
	bg        = getOption('repr.plot.bg'),
	pointsize = getOption('repr.plot.pointsize'),
	antialias = getOption('repr.plot.antialias'),
	#special
	family    = getOption('repr.plot.family'),
...) {
	if (!CAIRO_INSTALLED && !capabilities('cairo')) return(NULL) #only cairo can do SVG
	
	dev.cb <- function(tf)
		if (CAIRO_INSTALLED)
			Cairo::Cairo(width, height, tf, 'svg', pointsize, bg, 'transparent', 'in')
		else
			grDevices::svg(tf, width, height, pointsize, FALSE, family, bg, antialias)
	
	repr_recordedplot_generic(obj, '.svg', FALSE, dev.cb)
}

#' @name repr_*.recordedplot
#' @export
repr_pdf.recordedplot <- function(obj,
	width     = getOption('repr.plot.width'),
	height    = getOption('repr.plot.height'),
	bg        = getOption('repr.plot.bg'),
	pointsize = getOption('repr.plot.pointsize'),
	antialias = getOption('repr.plot.antialias'),
	#special
	family    = getOption('repr.plot.family'),
...) repr_recordedplot_generic(obj, '.pdf', TRUE, function(tf) {
	title <- plot_title(obj, 'Untitled plot')
	
	if (capabilities('aqua'))
		grDevices::quartz(title, width, height, pointsize, family, antialias, 'pdf', tf, bg)
	else if (CAIRO_INSTALLED)
		Cairo::Cairo(width, height, tf, 'pdf', pointsize, bg, 'transparent', 'in')
	else if (capabilities('cairo'))
		grDevices::cairo_pdf(tf, width, height, pointsize, FALSE, family, bg, antialias)
	else
		grDevices::pdf(tf, width, height, FALSE, family, title, bg = bg, pointsize = pointsize)
})
