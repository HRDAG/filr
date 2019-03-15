#' Create a path object
#' 
#' @param location A path to a file, relative to the project's root
#' @param is_dep Is the file a dependency of the current script?
#'
#' @export
pth <- function(location, is_dep = TRUE) {
    if (!inherits(location, "character") || length(location) !=  1L)
        stop("location must be a string")

    full_path <- here::here(location)
    if (is_dep && !file.exists(full_path))
        stop("missing dependency: ", location)

    if (!is_dep && !dir.exists(dirname(full_path)))
        stop("missing directory: ", dirname(full_path))

    structure(here::here(location), 
              class = "pth")
}

#' @rdname pth
#' @export
as.pth <- function(location, ...) UseMethod("as.pth")

#' @rdname pth
#' @export
as.pth.pth <- function(location, ...) location

#' @rdname pth
#' @export
as.pth.character <- function(location, ...) pth(location, ...)

#' Create a list of files used in your script
#'
#' @param ... \code{pth} or \code{pth}-like objects
#'
#' @export
filr <- function(...) {
    files <- list(...)
    filenames <- names(files)
    if (is.null(filenames) || any(filenames == ""))
        stop("all filr paths must be named")
    files <- Map(f = as.pth, files)
    structure(files, class = "filr")
}

`$.filr` <- function(files, name) as.character(files[[name]])
