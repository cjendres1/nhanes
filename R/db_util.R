
## Abstraction for DB access to hide backend-specific implementation
## details.

## We assume that we have three types of tables (in three schemas when
## schemas are supported): Metadata, Raw, Translated. Naming
## conventions may be different for different backends. We use
## constructor functions to determine suitably quoted identifiers.

.constructId <- function(conn, schema, table)
{
  backend <- class(conn) |> attr("package")
  ## cat(paste("---> backend = ", backend, " in constructId() <---\n"))
  switch(backend,
         odbc = sprintf('"%s"."%s"', schema, table),
         RPostgres = sprintf('"%s"."%s"', schema, table),
         ## RPostgres = DBI::Id(schema, table),
         RMariaDB = sprintf('Nhanes%s.%s', schema, table),
         stop("Unsupported DB backend: ", backend))
}

MetadataTable <- function(x, conn = cn()) .constructId(conn, "Metadata", x)
RawTable <- function(x, conn = cn()) .constructId(conn, "Raw", x)
TranslatedTable <- function(x, conn = cn()) .constructId(conn, "Translated", x)


## When connecting to a DB, we need to initialize a list of available
## tables / translated tables. There is no nice backward compatible
## way to do this, and we need backend-specific hacks

.getValidTables <- function(conn = cn(), type = c("Raw", "Translated"))
{
  type <- match.arg(type)
  backend <- class(conn) |> attr("package")
  if (length(backend) == 1L) {
    switch(backend,
           odbc =,
           RPostgres =
             {
               sql <- paste("SELECT DISTINCT TABLE_NAME", 
                            "FROM INFORMATION_SCHEMA.TABLES",
                            "WHERE TABLE_TYPE = 'BASE TABLE'",
                            "AND TABLE_CATALOG = 'NhanesLandingZone'",
                            sprintf("AND TABLE_SCHEMA = '%s'", type))
               DBI::dbGetQuery(conn, sql)[[1]]
             },
           ## RPostgres = 
           ##   {
           ##     sql <- paste("SELECT DISTINCT TABLE_NAME",
           ##                  "FROM INFORMATION_SCHEMA.TABLES", 
           ##                  "WHERE TABLE_TYPE = 'BASE TABLE' AND ", 
           ##                  "TABLE_CATALOG = 'NhanesLandingZone'")
           ##     ## schema doesn't work properly yet, so work around
           ##     alltabs <- DBI::dbGetQuery(conn, sql)[[1]]
           ##     alltabs <- alltabs[startsWith(alltabs, paste0(type, "."))]
           ##     gsub(paste0(type, "."), "", alltabs, fixed = TRUE)
           ##   },
           RMariaDB =
             {
               sql <- sprintf("SHOW TABLES FROM Nhanes%s", type)
               DBI::dbGetQuery(conn, sql)[[1]]
             },
           stop("Unsupported DB backend: ", backend))
  }
  else stop("Unexpected backend: ", backend)
}

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


## Functions to connect to various DB backends

##' @importFrom methods is

.connect_db <- function(backend = NULL, ...)
{
  if (is(cn(), "DBIConnection")) return(TRUE) # connection already set up
  ## Need DBI and backend package to proceed
  if (!requireNamespace("DBI", quietly = TRUE)) return (FALSE)
  if (is.null(backend) || !requireNamespace(backend, quietly = TRUE)) return(FALSE)
  driver <- switch(backend,
                   odbc = odbc::odbc(),
                   RMariaDB = RMariaDB::MariaDB(),
                   RPostgres = RPostgres::Postgres())
  ## suppress warning from DBI::dbConnect()
  before <- getTaskCallbackNames()
  .dbEnv$cn <- try(DBI::dbConnect(driver, ...), silent = TRUE)
  after <- getTaskCallbackNames()
  removeTaskCallback(which(!after %in% before))
  return(is(.dbEnv$cn, "DBIConnection"))
}

.connect_db_mssql <- function()
{
  .connect_db("odbc", 
              uid = Sys.getenv("EPICONDUCTOR_DB_UID", unset = "sa"),
              pwd = Sys.getenv("SA_PASSWORD", unset = "yourStrong(!)Password"),
              server = Sys.getenv("EPICONDUCTOR_DB_SERVER", unset = "localhost"),
              port = as.integer(Sys.getenv("EPICONDUCTOR_DB_PORT", unset = "1433")),
              database = Sys.getenv("EPICONDUCTOR_DB_DATABASE",
                                    unset = "NhanesLandingZone"),
              driver = Sys.getenv("EPICONDUCTOR_DB_DRIVER",
                                  unset = "ODBC Driver 17 for SQL Server"))
}

.connect_db_mariadb <- function()
{
  .connect_db("RMariaDB",
              username = Sys.getenv("EPICONDUCTOR_DB_UID", unset = "admin"),
              password = Sys.getenv("SA_PASSWORD", unset = "C0lumnStore!"),
              host = Sys.getenv("EPICONDUCTOR_DB_SERVER", unset = "localhost"),
              port = as.integer(Sys.getenv("EPICONDUCTOR_DB_PORT", unset = "3306")),
              mysql = FALSE)
}

.connect_db_postgres <- function()
{
  .connect_db("RPostgres",
              user = Sys.getenv("EPICONDUCTOR_DB_UID", unset = "sa"),
              password = Sys.getenv("SA_PASSWORD", unset = "NHAN35"),
              host = Sys.getenv("EPICONDUCTOR_DB_SERVER", unset = "localhost"),
              ## host = Sys.getenv("EPICONDUCTOR_DB_SERVER", unset = "0.0.0.0"),
              port = as.integer(Sys.getenv("EPICONDUCTOR_DB_PORT", unset = "5432")),
              dbname = Sys.getenv("EPICONDUCTOR_DB_DATABASE", unset = "NhanesLandingZone"))
}

.init_db <- function()
{
  if (isTRUE(.dbEnv$ok)) return(TRUE) # already set up
  ## otherwise try to set it up. Try to guess from environment variables
  container_backend <- Sys.getenv("EPICONDUCTOR_CONTAINER_DB") # preferred
  container_version <- Sys.getenv("EPICONDUCTOR_CONTAINER_VERSION")
  .dbEnv$ok <- if (nzchar(container_backend)) {
    if (container_backend == "postgres") .connect_db_postgres()
    else if (container_backend == "mariadb") .connect_db_mariadb()
  }
  else if (nzchar(container_version)) { ## update for new container releases
    if (container_version == "v0.4.1") .connect_db_mssql()
    else if (container_version == "v0.5.0") .connect_db_mariadb()
    else .connect_db_postgres()
  }
  else FALSE
  if (isFALSE(.dbEnv$ok) &&
        (nzchar(container_version) || nzchar(container_backend)))
    warning("Unable to connect to DB, falling back to online downloads")
  else {
    .dbEnv$validTables <- .getValidTables(.dbEnv$cn, type = "Raw")
    .dbEnv$translatedTables <- .getValidTables(.dbEnv$cn, type = "Translated")
  }
  .dbEnv$ok
}


