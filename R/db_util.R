
## Query data from the Docker database
## examples: nhanesQuery("SELECT TOP(50) * FROM QuestionnaireVariables;")

## Query data from the Docker database
## examples: nhanesQuery("SELECT TOP(50) * FROM Metadata.QuestionnaireVariables;")
.nhanesQuery = function(sql){

  # suppress warining from DBI::dbConnect()
  before <- getTaskCallbackNames()
  cn  <- MsSqlTools::connectMsSqlSqlLogin(
    server = "localhost",
    user ="sa",
    password="yourStrong(!)Password",
    database="NhanesLandingZone")
  after <- getTaskCallbackNames()
  removeTaskCallback(which(!after %in% before))

  df <- DBI::dbGetQuery(cn, sql)
  DBI::dbDisconnect(cn)

  df
}

# query table names from Metadata.QuestionnaireVariables
.validTables = .nhanesQuery("SELECT DISTINCT TableName FROM Metadata.QuestionnaireVariables;")$TableName

# check if the table names are valid
.checkTableNames = function(table_name){
  if(is.null(table_name)){
    stop("Table name cannot be null!")
  }
  validIndx = (table_name %in% .validTables)
  if(sum(!validIndx)>0){
    stop("Table ",paste(table_name[!validIndx],collapse = ", "),
         " does/do not exist in database, please check the table names.")
  }
}

# is translated tables exist
.tranlsatedTables = .nhanesQuery("SELECT DISTINCT TABLE_NAME
                                FROM INFORMATION_SCHEMA.TABLES
                                WHERE TABLE_TYPE = 'BASE TABLE'
                                      AND TABLE_CATALOG='NhanesLandingZone' AND TABLE_SCHEMA = 'Translated'")$TABLE_NAME
# choose the to query translated or Raw table.
.convertTranslatedTable = function(table_name,translated){
  if(translated ){
    translatedIndx = (table_name %in% .tranlsatedTables)
    table_name[translatedIndx] = paste0("Translated.",table_name[translatedIndx])
    if(sum(!translatedIndx)>0){

      warning("Table ",paste(table_name[!translatedIndx],collapse = ", "),
              " does/do not exist in Translated schema, using Raw schema instead.")
      table_name[!translatedIndx] = paste0("Raw.",table_name[!translatedIndx])
    }
  }else{
    table_name = paste0("Raw.",table_name)

  }
  table_name
}
