#' Get plot title
#' 
#' @export
repr_text.recordedplot <- function(p, ...) {
	for (call in rev(p[[1]])) {
		args <- call[[2]]
		if(args[[1]]$name == 'C_title' && !is.null(args[[2]])) {
			return(sprintf('Plot with title “%s”', args[[2]]))
		}
	}
	'plot without title'
}


### BITMAPS ###


#' Get PNG plot
#' 
#' @export
repr_png.recordedplot <- function(p,
	width     = getOption('repr.plot.width'),
	height    = getOption('repr.plot.height'),
	bg        = getOption('repr.plot.bg'),
	pointsize = getOption('repr.plot.pointsize'),
	antialias = getOption('repr.plot.antialias'),
	#special
	res       = getOption('repr.plot.res'),
...) {
	if (!capabilities('png')) return(NULL)
	tf <- tempfile(fileext = '.png')
	png(tf, width, height, 'in', pointsize, bg, res, type = 'cairo', antialias = antialias)
	replayPlot(p)
	dev.off()
	readBin(tf, raw(), file.info(tf)$size)
}

#' Get JPEG plot
#' 
#' @export
repr_jpg.recordedplot <- function(p,
	width     = getOption('repr.plot.width'),
	height    = getOption('repr.plot.height'),
	bg        = getOption('repr.plot.bg'),
	pointsize = getOption('repr.plot.pointsize'),
	antialias = getOption('repr.plot.antialias'),
	#special
	res       = getOption('repr.plot.res'),
	quality   = getOption('repr.plot.quality'),
...) {
	if (!capabilities('jpeg')) return(NULL)
	tf <- tempfile(fileext = '.jpg')
	jpeg(tf, width, height, 'in', pointsize, quality, bg, res, type = 'cairo', antialias = antialias)
	replayPlot(p)
	dev.off()
	readBin(tf, raw(), file.info(tf)$size)
}


### VECTOR ###


#' Get SVG plot
#' 
#' @export
repr_svg.recordedplot <- function(p,
	width     = getOption('repr.plot.width'),
	height    = getOption('repr.plot.height'),
	bg        = getOption('repr.plot.bg'),
	pointsize = getOption('repr.plot.pointsize'),
	antialias = getOption('repr.plot.antialias'),
	#special
	family    = getOption('repr.plot.family'),
...) {
	if (!capabilities('cairo')) return(NULL)
	tf <- tempfile(fileext = '.svg')
	svg(tf, width, height, pointsize, FALSE, family, bg, antialias)
	replayPlot(p)
	dev.off()
	readChar(tf, file.info(tf)$size, TRUE)
}

#' Get PDF plot
#' 
#' @export
repr_pdf.recordedplot <- function(p,
	width     = getOption('repr.plot.width'),
	height    = getOption('repr.plot.height'),
	bg        = getOption('repr.plot.bg'),
	pointsize = getOption('repr.plot.pointsize'),
	antialias = getOption('repr.plot.antialias'),
	#special
	family    = getOption('repr.plot.family'),
...) {
	if (!capabilities('cairo')) return(NULL)
	tf <- tempfile(fileext = '.pdf')
	cairo_pdf(tf, width, height, pointsize, FALSE, family, bg, antialias)
	replayPlot(p)
	dev.off()
	readBin(tf, raw(), file.info(tf)$size)
}
