


.nhanesOptions <- new.env(parent = emptyenv())

##' Set and retrieve global options controlling the behaviour of
##' certain functions in the package.
##'
##' The 'nhanesOptions()' function can be used in two forms, to set or
##' get options. Options can be set using 'nhanesOptions(key1 =
##' value1, key2 = value2)'. Options can be retrieved (one at a time)
##' using 'nhanesOptions("key")'. When called with no arguments, all
##' currently set options are returned as a list.
##'
##' Options currently used in the package are 'use.db' (logical flag
##' controlling whether a database should be used if available),
##' 'use.cache' (logical flag controlling whether the BiocFileCache
##' (Bioconductor) package should be used, if available, to cache
##' downloaded files), and 'log.access', a logical flag that logs any
##' attempted URL access by printing the URL).
##' @title Options for the nhanesA package
##' @param ... either one or more named arguments giving options to be
##'     set (in the form \code{key = value}), or a single unnamed
##'     character string to retrieve a setting.
##' @return When retrieving an option, the value of the option, or
##'     \code{NULL} if the option has not been set. When setting one
##'     or more options, a list (invisibly) containing the previous
##'     values (possibly \code{NULL}) of the options being set.
##' @author Deepayan Sarkar <deepayan.sarkar@gmail.com>
##' @examples
##'  nhanesOptions(foo = "bar")
##'  nhanesOptions()
##'  print(nhanesOptions(foo = NULL))
##' @export
nhanesOptions <- function(...) {
    args <- list(...)
    nm <- names(args)
    if (length(args) == 0L) return(as.list(.nhanesOptions))

    if (is.null(nm) && length(args) == 1L)
        return(.nhanesOptions[[ args[[1]] ]])
    if (is.null(nm)) stop("invalid use: only one option can be retrieved at a time")
    retval <- mget(nm, .nhanesOptions, ifnotfound = list(NULL))
    for (name in nm) {
        .nhanesOptions[[ name ]] <- args[[ name ]]
    }
    invisible(retval)
}



