

## Abstraction for DB access to hide backend-specific implementation
## details.

## We assume that we have three types of tables (in three schemas when
## schemas are supported): Metadata, Raw, Translated. Naming
## conventions may be different for different backends. We use
## constructor functions to determine suitably quoted identifiers.

.constructId <- function(conn, schema, table)
{
    backend <- class(conn) |> attr("package")
    switch(backend,
           RPostgres = sprintf('"%s.%s"', schema, table),
           stop("Unsupported DB backend: ", backend))
}

MetadataTable <- function(x, conn = cn()) .constructId(conn, "Metadata", x)
RawTable <- function(x, conn = cn()) .constructId(conn, "Raw", x)
TranslatedTable <- function(x, conn = cn()) .constructId(conn, "Translated", x)



## Query data from the Docker database
## examples: nhanesQuery("SELECT TOP(50) * FROM QuestionnaireVariables;")

## Query data from the Docker database
## examples: nhanesQuery("SELECT TOP(50) * FROM Metadata.QuestionnaireVariables;")


.nhanesQuery <- function(sql)
{
  if(!.dbEnv$ok) stop("no database available for use")
  return(DBI::dbGetQuery(cn(), sql))
}


## check if the table names are valid

.checkTableNames <- function(table_name)
{
  if(!.dbEnv$ok) stop("no database available for use")
  if(is.null(table_name)) stop("Table name cannot be NULL!")
  ok <- (table_name %in% validTables())
  if (any(!ok)) 
    stop("Table(s) ", paste(table_name[!ok], collapse = ", "),
         " missing from database")
  invisible()
}


## choose translated or Raw table.

.convertTranslatedTable <- function(table_name, translated)
{
  if (!.dbEnv$ok) stop("no database available for use")
  if (translated)
  {
    ok <- (table_name %in% translatedTables()) # whether translated tables exist
    if (any(!ok))
    {
      warning("Table(s) ", paste(table_name[!ok], collapse = ", "),
              " missing from Translated schema, using Raw schema instead.")
    }
    ifelse(ok, TranslatedTable(table_name), RawTable(table_name))
  }
  else RawTable(table_name)
}


##' @importFrom methods is

.connect_db_postgres = function()
{
  if (is(cn(), "DBIConnection")) return(TRUE) # connection already set up
  .dbEnv$container_version <- Sys.getenv("EPICONDUCTOR_CONTAINER_VERSION")
  .dbEnv$collection_date <- as.Date(Sys.getenv("EPICONDUCTOR_COLLECTION_DATE"))
  ## message("EpiConductor Container Version: ", .container_version)
  ## message("Data Collection Date: ", .collection_date)
  if (!nzchar(.dbEnv$container_version) || is.na(.dbEnv$collection_date)) {
    return(FALSE) # no DB available for use 
  }
  if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("RPostgres", quietly = TRUE)) {
    ## can't use DB because required packages not available
    warning("Packages 'DBI' and 'RPostgres' unavailable but required to use Postgres DB")
    return(FALSE)
  }
  ## suppress warning from DBI::dbConnect()
  before <- getTaskCallbackNames()
  .dbEnv$cn <-
    try(
      DBI::dbConnect(
        RPostgres::Postgres(),
        user = Sys.getenv("EPICONDUCTOR_DB_UID", unset = "sa"),
        password = Sys.getenv("SA_PASSWORD", unset = "NHAN35"),
        host = Sys.getenv("EPICONDUCTOR_DB_SERVER", unset = "localhost"),
        port = as.integer(Sys.getenv("EPICONDUCTOR_DB_PORT", unset = "5432")),
        dbname = Sys.getenv("EPICONDUCTOR_DB_NAME", unset = "NhanesLandingZone")
      ),
      silent = TRUE)
  after <- getTaskCallbackNames()
  removeTaskCallback(which(!after %in% before))
  return(is(.dbEnv$cn, "DBIConnection"))
}


.init_db <- function()
{
  if (isTRUE(.dbEnv$ok)) return(TRUE) # already set up
  .dbEnv$ok <- .connect_db_postgres()
  .dbEnv$validTables <- 
      .nhanesQuery(
        "SELECT DISTINCT \"TableName\" FROM \"Metadata.QuestionnaireDescriptions\"")$TableName
  
  if (inherits(.dbEnv$cn, "try-error"))
    warning("Unable to connect to DB, falling back to online downloads")
  
  return(.dbEnv$ok)
}

