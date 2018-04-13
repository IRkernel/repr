#' @importFrom htmltools renderTags copyDependencyToDir makeDependencyRelative renderDependencies
#' @importFrom base64enc dataURI
embed_tags <- function(obj, ...) {
	
	obj <- htmltools::renderTags(obj)
	
	if (nchar(obj$head) > 0) {
		# TODO: 
		# (1) can this be done?
		# (2) what about singletons?
		warning("Inserting HTML strings into <head> currently isn't supported")
	}
	
	# ignore dependencies that already exist in the notebook
	obj$dependencies <- setdiff(obj$dependencies, .dependencies$get())
	
	# add these (new) dependencies to the dependency manager
	.dependencies$add(obj$dependencies)
	
	# render dependencies as data URIs (for standalone HTML)
	depHTML <- lapply(obj$dependencies, function(dep) {
		
		html <- c()
		
		if (length(dep$script) > 0) {
			f <- file.path(dep$src$file, dep$script)
			# TODO: is this *always* the correct mime type?
			html <- c(html, sprintf(
				'<script src="%s"></script>', 
				base64enc::dataURI(mime = "application/javascript", file = f)
			))
		}
		
		if (length(dep$stylesheet) > 0) {
			f <- file.path(dep$src$file, dep$stylesheet)
			# TODO: is this *always* the correct mime type? Use base64enc::checkUTF8() to ensure UTF-8 is OK?
			html <- c(html, sprintf(
				'<link href="%s" rel="stylesheet" />', 
				base64enc::dataURI(mime = "text/css;charset-utf-8", file = f)
			))
		}
		
		paste(html, collapse = "\n")
	})
	
	html <- sprintf(
		'<!DOCTYPE html>
		  <html>
		    <head>
		     <meta charset="utf-8" />
         %s
        <head>
        <body>
          %s
        </body>
      </html>
		', unlist(depHTML), obj$html
	)
	
	paste(html, collapse = "\n")
}

# find a new folder name under the working directory 
new_dir <- function() {
	dirCandidate <- new_id()
	while (dir.exists(dirCandidate)) {
		dirCandidate <- new_id()
	}
	dirCandidate
}

new_id <- function() {
	basename(tempfile(""))
}


# keep track of what dependencies have been included and where they are located
dependency_manager <- function() {
	deps <- NULL
	depDir <- new_dir()
	
	as.environment(list(
		get = function() deps,
		add = function(dep) deps <<- unique(c(deps, dep)),
		dir = function() depDir
	))
}

.dependencies <- dependency_manager()

destroy <- function(.dep) {
	unlink(.dep$dir(), recursive = TRUE)
}

# delete the dependency files that have been copied to the ipython notebook
# webserver location (when this object is garbage collected or upon exiting R)
reg.finalizer(.dependencies, destroy, onexit = TRUE)



#' HTML widget representations
#'
#' Standalone HTML representation and dummy text representation
#'
#' @param obj  The htmlwidget to create a representation for
#' @param ...  ignored
#'
#' @name repr_*.htmlwidget
#' @export
repr_text.htmlwidget <- function(obj, ...) 'HTML widgets cannot be represented in plain text (need html)'

#' @name repr_*.htmlwidget
#' @export
repr_html.htmlwidget <- embed_tags

#' Shiny tag representations
#'
#' Standalone HTML representation and dummy text representation
#'
#' @param obj  The shiny tags to create a representation for
#' @param ...  ignored
#'
#' @name repr_*.shiny.tag
#' @export
repr_text.shiny.tag <- function(obj, ...) 'Shiny tags cannot be represented in plain text (need html)'

#' @name repr_*.shiny.tag
#' @export
repr_html.shiny.tag <- embed_tags

#' Standalone HTML representation and dummy text representation
#'
#' @param obj  The shiny tags to create a representation for
#' @param ...  ignored
#'
#' @name repr_*.shiny.tag.list
#' @export
repr_text.shiny.tag.list <- function(obj, ...) 'Shiny tags cannot be represented in plain text (need html)'

#' @name repr_*.htmlwidget
#' @export
repr_html.shiny.tag.list <- embed_tags

