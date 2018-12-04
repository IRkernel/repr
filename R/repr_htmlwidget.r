#' @importFrom htmltools renderTags
embed_tags <- function(obj, ...) {
	obj <- renderTags(obj)
	
	if (nchar(obj$head) > 0) {
		# TODO: 
		# (1) can this be done?
		# (2) what about singletons?
		warning('Inserting HTML strings into <head> is currently not supported')
	}
	
	if (getOption('repr.html.deduplicate')) {
		# ignore dependencies that already exist in the notebook
		obj$dependencies <- setdiff(obj$dependencies, html_dependencies$get())
		
		# add these (new) dependencies to the dependency manager
		html_dependencies$add(obj$dependencies)
	}
	
	# render dependencies as data URIs (for standalone HTML)
	html_deps <- lapply(obj$dependencies, function(dep) {
		html <- c()
		
		if (length(dep$script) > 0) {
			f <- file.path(dep$src$file, dep$script)
			# TODO: is this *always* the correct mime type?
			html <- c(html, sprintf(
				'<script title="%s" src="%s"></script>',
				sub('"', '', dep$name),
				data_uris(mime = 'application/javascript', files = f)
			))
		}
		
		if (length(dep$stylesheet) > 0) {
			f <- file.path(dep$src$file, dep$stylesheet)
			# TODO: is this *always* the correct mime type? Use base64enc::checkUTF8() to ensure UTF-8 is OK?
			html <- c(html, sprintf(
				'<link href="%s" rel="stylesheet" />',
				data_uris(mime = 'text/css;charset-utf-8', files = f)
			))
		}
		
		paste(html, collapse = '\n')
	})
	
	sprintf(HTML_SKELETON, paste(html_deps, collapse = '\n'), obj$html)
}

HTML_SKELETON <-
'<!doctype html>
<html>
	<head>
		<meta charset="utf-8">
		%s
	</head>
	<body>
		%s
	</body>
</html>
'

# find a new folder name under the working directory 
new_dir <- function() {
	dir_candidate <- new_id()
	while (dir.exists(dir_candidate)) {
		dir_candidate <- new_id()
	}
	dir_candidate
}

new_id <- function() basename(tempfile(''))


# keep track of what dependencies have been included and where they are located
dependency_manager <- function() {
	deps <- NULL
	dep_dir <- new_dir()
	
	as.environment(list(
		get = function() deps,
		add = function(dep) deps <<- unique(c(deps, dep)),
		clear = function() {
			deps <<- NULL
			unlink(dep_dir, recursive = TRUE)
		},
		dir = function() dep_dir
	))
}

#' @name repr_*.htmlwidget
#' @export
html_dependencies <- dependency_manager()

# delete the dependency files that have been copied to the jupyter notebook
# webserver location (when this object is garbage collected or upon exiting R)
reg.finalizer(html_dependencies, function(deps) deps$clear(), onexit = TRUE)



#' HTML widget representations
#' 
#' Standalone HTML representation and dummy text representation.
#' 
#' \code{html_dependencies} is an \link[base]{environment} containing the following functions.
#' \code{getOption(\link[=repr-options]{'repr.html.deduplicate'})}
#' \describe{
#'  \item{\code{get()}}{Get the list of added dependencies}
#'  \item{\code{add(dep)}}{Marks a dependency as added. Call this e.g. after appending a script tag with the dependency.}
#'  \item{\code{clear()}}{Clear the list as seen dependencies. Now everything will be added again when encountered.}
#'  \item{\code{dir()}}{Returns the directory in which the dependencies reside.}
#' }
#' 
#' @param obj  The htmlwidget, shiny.tag, or shiny.tag.list to create a representation for
#' @param ...  ignored
#' 
#' @name repr_*.htmlwidget
#' @export
repr_text.htmlwidget <- function(obj, ...) 'HTML widgets cannot be represented in plain text (need html)'

#' @name repr_*.htmlwidget
#' @export
repr_html.htmlwidget <- embed_tags


#' @aliases repr_*.shiny.tag
#' @name repr_*.htmlwidget
#' @export
repr_text.shiny.tag <- function(obj, ...) 'Shiny tags cannot be represented in plain text (need html)'

#' @name repr_*.htmlwidget
#' @export
repr_html.shiny.tag <- embed_tags


#' @aliases repr_*.shiny.tag.list
#' @name repr_*.htmlwidget
#' @export
repr_text.shiny.tag.list <- function(obj, ...) 'Shiny tags cannot be represented in plain text (need html)'

#' @name repr_*.htmlwidget
#' @export
repr_html.shiny.tag.list <- embed_tags
