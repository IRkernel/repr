#' Get plot title
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
#' @export
repr_text.recordedplot <- function(obj, ...) {
	for (call in rev(obj[[1]])) {
		args <- call[[2]]
		if(isTRUE(args[[1]]$name == 'C_title') && !is.null(args[[2]])) {
			return(sprintf('Plot with title %s', dQuote(args[[2]])))
		}
	}
	'plot without title'
}

repr_recordedplot_generic <- function(obj, ext, binary, dev.cb) {
	tf <- tempfile(fileext = ext)
	dev.cb(tf)
 	replayPlot(obj)
	dev.off()
	if(binary)
		readBin(tf, raw(), file.info(tf)$size)
	else
		readChar(tf, file.info(tf)$size, useBytes = TRUE)
}


### BITMAPS ###


#' Get PNG plot
#' 
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
	if (!any(capabilities(c('aqua', 'cairo', 'X11')))) return(NULL)
	
	repr_recordedplot_generic(obj, '.png', TRUE, function(tf)
		png(tf, width, height, 'in', pointsize, bg, res, antialias = antialias))
}

#' Get JPEG plot
#' 
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
	if (!any(capabilities(c('aqua', 'cairo', 'X11')))) return(NULL)
	
	repr_recordedplot_generic(obj, '.jpg', TRUE, function(tf)
		jpeg(tf, width, height, 'in', pointsize, quality, bg, res, antialias = antialias))
}


### VECTOR ###


#' Get SVG plot
#' 
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
	if (!capabilities('cairo')) return(NULL) #only cairo can do SVG
	
	repr_recordedplot_generic(obj, '.svg', FALSE, function(tf)
		svg(tf, width, height, pointsize, FALSE, family, bg, antialias))
}

#' Get PDF plot
#' 
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
	if (capabilities('aqua'))
		quartz('Quartz %d', width, height, pointsize, family, antialias, 'pdf', tf, bg)
	else if (capabilities('cairo'))
		cairo_pdf(tf, width, height, pointsize, FALSE, family, bg, antialias)
	else
		pdf(tf, width, height, FALSE, family, bg = bg, pointsize = pointsize)
})
