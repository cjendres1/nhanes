##notes

.dbEnv <- new.env(parent = emptyenv())

cn <- function() .dbEnv$cn
translatedTables <- function() .dbEnv$translatedTables
validTables <- function() .dbEnv$validTables

## For use by internal functions to decide whether the DB should be
## used.  Returns TRUE iff DB is available AND nhanesOptions("use.db")
## is not FALSE

.useDB <- function() {
  !isFALSE(nhanesOptions("use.db")) && isTRUE(.dbEnv$ok)
}



##' @importFrom utils globalVariables

.onLoad = function(libname, pkgname)
{
  nhanesOptions(use.db = .init_db())
  makeActiveBinding(sym = "nhanesManifestPrefix",
                    fun = ab_nhanesManifestPrefix,
                    env = environment(ab_nhanesManifestPrefix))
  makeActiveBinding(sym = "nhanesTableURL",
                    fun = ab_nhanesTableURL,
                    env = environment(ab_nhanesTableURL))
  ## declare 'global' variables used in subset() to make codetools happy 
  utils::globalVariables(c("DataURL", "Begin.Year", "Component", "Data.File",
                           "Data.File.Name", "Date.Published", "Doc.File",
                           "EndYear", "Use.Constraints",
                           "Variable.Description", "Value.Description"),
                         package = "nhanesA", add = FALSE)
}

.onUnload <- function(libpath)
{
  if (is(cn(), "DBIConnection")) {
    DBI::dbDisconnect(cn())
  }
}

