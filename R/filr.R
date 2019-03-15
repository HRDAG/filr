pth <- function(location, is_dep = TRUE) {
    if (!inherits(location, "character") || length(location) !=  1L)
        stop("location must be a string")

    full_path <- here::here(location)
    if (is_dep && !file.exists(full_path))
        stop("missing dependency: ", location)

    if (!!is_dep && !dir.exists(dirname(full_path)))
        stop("missing directory: ", dirname(full_path))

    structure(here::here(location), 
              is_dep = is_dep,
              class = "pth")
}

as.pth <- function(location, ...) UseMethod("as.pth")
as.pth.pth <- function(location, ...) location
as.pth.character <- function(location, ...) pth(location, ...)

filr <- function(...) {
    files <- list(...)
    filenames <- names(files)
    if (is.null(filenames) || any(filenames == ""))
        stop("all filr paths must be named")
    files <- Map(f = as.pth, files)
    structure(files, class = "filr")
}

`$.filr` <- function(files, name) as.character(files[[name]])
