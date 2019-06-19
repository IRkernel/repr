repr_vignette_generic <- function(v, ext, isbinary) {
    path <- file.path(v$Dir, 'doc', v$PDF)
    if (tolower(tools::file_ext(path)) == ext) {
        s <- file.info(file)$size
        if (isbinary)
            readBin(path, 'raw', s)
        else
            readChar(path, s)
    } else NULL
}

repr_html.vignette <- function(obj, ...) repr_vignette_generic(obj, 'html', FALSE)
repr_pdf.vignette <- function(obj, ...) repr_vignette_generic(obj, 'pdf', FALSE)
