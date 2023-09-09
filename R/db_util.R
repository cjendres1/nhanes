
## Query data from the Docker database
## examples: nhanesQuery("SELECT TOP(50) * FROM QuestionnaireVariables;")

## Query data from the Docker database
## examples: nhanesQuery("SELECT TOP(50) * FROM Metadata.QuestionnaireVariables;")
.collection_date = NA
.container_version = NA

.nhanesQuery = function(sql){
  if(is.na(.collection_date) | is.na(.container_version)) 
    stop("can't run .nhanesQuery - no DB detected")
 return(DBI::dbGetQuery(cn(), sql))
}


# check if the table names are valid

.checkTableNames = function(table_name){
  if(is.na(.collection_date) | is.na(.container_version))
   stop("no DB detected")

  if(is.null(table_name)){
    stop("Table name cannot be null!")
  }
  validIndx = (table_name %in% .validTables)
  if(sum(!validIndx)>0){
    stop("Table ",paste(table_name[!validIndx],collapse = ", "),
         " does/do not exist in database, please check the table names.")
  }
}


# choose the to query translated or Raw table.
.convertTranslatedTable = function(table_name,translated){
  if(is.na(.collection_date) | is.na(.container_version))
    stop("no DB detected")
  # is translated tables exist
  if(translated ){
    translatedIndx = (table_name %in% .translatedTables)
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
