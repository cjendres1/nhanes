##notes

.dbEnv <- new.env(parent = emptyenv())
.nhanesCacheEnv <- new.env(parent = emptyenv())

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
  NSE.globals <-
      c("Begin.Year", "BeginYear", "CodeOrValue", "Component",
        "concat", "Count", "Cumulative", "Data.File",
        "Data.File.Description", "Data.File.Name", "DataGroup",
        "DataURL", "Date.Published", "DatePublished", "Description",
        "Description.x", "Description.y", "Doc.File", "EndYear",
        "SasLabel", "SkipToItem", "TableName", "Target",
        "Use.Constraints", "Value.Description", "ValueDescription",
        "Variable", "Variable.Description", "Variable.Name", "Years", "UseConstraints")
  utils::globalVariables(NSE.globals,
                         package = "nhanesA", add = FALSE)
}

.onUnload <- function(libpath)
{
  if (is(cn(), "DBIConnection")) {
    DBI::dbDisconnect(cn())
  }
}



