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
      uid = "sa", 
      pwd = "yourStrong(!)Password",
      server = "localhost", 
      database = "NhanesLandingZone",
      port = 1433, 
      driver = "ODBC Driver 17 for SQL Server"
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

.onLoad = function(libname, pkgname)
{
  nhanesOptions(use.db = .init_db())
}

.onUnload <- function(libpath)
{
  if (is(cn(), "DBIConnection")) {
    DBI::dbDisconnect(cn())
  }
}

