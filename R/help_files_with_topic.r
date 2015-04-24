# override utils:::print.help_files_with_topic

#' Representation of help
#' 
#' @export
repr_help_files_with_topic_generic <- function(helps, Rd2_) {
    topic <- attr(helps, 'topic')
    type <- attr(helps, 'type') #should we make this html by setting some option?
    tried_all_packages <- attr(helps, 'tried_all_packages')
    #TODO: handle tried_all_packages
    
    paths <- as.character(helps)
    
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

#' @export
repr_text.help_files_with_topic <- function(helps, ...)
	repr_help_files_with_topic_generic(helps, tools::Rd2txt)

#' @export
repr_html.help_files_with_topic <- function(helps, ...)
	repr_help_files_with_topic_generic(helps, tools::Rd2HTML)

#TODO: markdown

#' @export
repr_latex.help_files_with_topic <- function(helps, ...)
	repr_help_files_with_topic_generic(helps, tools::Rd2latex)