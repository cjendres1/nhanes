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


##' @importFrom methods is

.connect_db <- function()
{
  if (is(cn(), "DBIConnection")) return(TRUE) # connection already set up
  .dbEnv$container_version <- Sys.getenv("EPICONDUCTOR_CONTAINER_VERSION")
  .dbEnv$collection_date <- as.Date(Sys.getenv("EPICONDUCTOR_COLLECTION_DATE"))
  ## message("EpiConductor Container Version: ", .container_version)
  ## message("Data Collection Date: ", .collection_date)
  if (!nzchar(.dbEnv$container_version) || is.na(.dbEnv$collection_date)) {
    return(FALSE) # no DB available for use 
  }
  if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("odbc", quietly = TRUE)) {
    ## can't use DB because required packages not available
    warning("Packages 'DBI' and 'odbc' unavailable but required to use database")
    return(FALSE)
  }
  ## suppress warning from DBI::dbConnect()
  before <- getTaskCallbackNames()
  .dbEnv$cn <-
    DBI::dbConnect(
      odbc::odbc(),
      uid = Sys.getenv("EPICONDUCTOR_DB_UID", unset = "sa"),
      pwd = Sys.getenv("SA_PASSWORD", unset = "yourStrong(!)Password"),
      server = Sys.getenv("EPICONDUCTOR_DB_SERVER", unset = "localhost"),
      port = as.integer(Sys.getenv("EPICONDUCTOR_DB_PORT", unset = "1433")),
      database = Sys.getenv("EPICONDUCTOR_DB_DATABASE", unset = "NhanesLandingZone"),
      driver = Sys.getenv("EPICONDUCTOR_DB_DRIVER", unset = "ODBC Driver 17 for SQL Server")
    )
    after <- getTaskCallbackNames()
    removeTaskCallback(which(!after %in% before))
  return(TRUE)
}

.init_db <- function()
{
  if (isTRUE(.dbEnv$ok)) return(TRUE) # already set up
  ## otherwise try to set it up
  if (.dbEnv$ok <- .connect_db()) {
    .dbEnv$translatedTables <-
      .nhanesQuery(
        "SELECT DISTINCT TABLE_NAME
         FROM INFORMATION_SCHEMA.TABLES
         WHERE TABLE_TYPE = 'BASE TABLE'
         AND TABLE_CATALOG='NhanesLandingZone'
         AND TABLE_SCHEMA = 'Translated'")$TABLE_NAME
    .dbEnv$validTables <- 
      .nhanesQuery(
        "SELECT DISTINCT TableName FROM Metadata.QuestionnaireVariables;")$TableName
  }
  return(.dbEnv$ok)
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

