
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
  prefix <-
    if (translated)
    {
      ok <- (table_name %in% translatedTables()) # whether translated tables exist
      if (any(!ok))
      {
        warning("Table(s) ", paste(table_name[!ok], collapse = ", "),
                " missing from Translated schema, using Raw schema instead.")
      }
      ifelse(ok, "Translated.", "Raw.")
    }
    else "Raw."
  paste0(prefix, table_name)
}



