# override utils:::print.help_files_with_topic

#' Representations of help
#' 
#' @aliases repr_text.help_files_with_topic repr_html.help_files_with_topic repr_latex.help_files_with_topic
#' @name repr_*.help_files_with_topic
NULL

repr_help_files_with_topic_generic <- function(obj, Rd2_) {
    topic <- attr(obj, 'topic')
    type <- attr(obj, 'type') #should we make this html by setting some option?
    tried_all_packages <- attr(obj, 'tried_all_packages')
    #TODO: handle tried_all_packages
    
    paths <- as.character(obj)
    
    if(length(paths) == 0) {
    	return(paste(gettextf('No documentation for %s in specified packages and libraries:', sQuote(topic)),
    							 gettextf('you could try %s', sQuote(paste0('??', topic))), sep = '\n'))
    }
    
    #TODO: handle multiple
    file <- paths[[1]]
    
    pkgname <- basename(dirname(dirname(file)))
    
    rd <- utils:::.getHelpFile(file)
    
    output <- capture.output(Rd2_(rd, package = pkgname, outputEncoding = 'UTF-8'))
    
    if (identical(Rd2_, tools::Rd2HTML)) {
    	head.end.idx <- which(output == '</head><body>')
    	body.end.idx <- which(output == '</body></html>')
    	rm.idx <- c(seq_len(head.end.idx), body.end.idx)
    	
    	output <- output[-rm.idx]
    }
    
    #TODO: replace all the Rd-specific envs in latex
    
    paste(output, collapse = '\n')
}

#' @export @name repr_*.help_files_with_topic
repr_text.help_files_with_topic <- function(obj, ...)
	repr_help_files_with_topic_generic(obj, tools::Rd2txt)

#' @export @name repr_*.help_files_with_topic
repr_html.help_files_with_topic <- function(obj, ...)
	repr_help_files_with_topic_generic(obj, tools::Rd2HTML)

#TODO: markdown

#' @export @name repr_*.help_files_with_topic
repr_latex.help_files_with_topic <- function(obj, ...)
	repr_help_files_with_topic_generic(obj, tools::Rd2latex)