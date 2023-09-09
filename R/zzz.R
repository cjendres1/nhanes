##notes
.datacache = new.env(parent=emptyenv())
.container_version = NA
.collection_date = NA 
.translatedTables = NA
.validTables = NA


cn = function() .datacache$cn

.onLoad = function(libname, pkgname){
  .container_version <<- Sys.getenv("EPICONDUCTOR_CONTAINER_VERSION",unset=NA)
  .collection_date <<- as.Date(Sys.getenv("EPICONDUCTOR_COLLECTION_DATE"),unset=NA)

  if(!is.na(.container_version) & !is.na(.collection_date)){
    #message("EpiConductor Container Version: ", .container_version)
    #message("Data Collection Date: ", .collection_date)
    # suppress warining from DBI::dbConnect()
    before <- getTaskCallbackNames()
    cn  <- MsSqlTools::connectMsSqlSqlLogin(
       server = "localhost",
       user ="sa",
       password="yourStrong(!)Password",
       database="NhanesLandingZone")
       after <- getTaskCallbackNames()
       removeTaskCallback(which(!after %in% before))
   assign("cn", cn, envir = .datacache) 
   ##set up a couple of global variables
   .translatedTables <<- .nhanesQuery("SELECT DISTINCT TABLE_NAME
                                FROM INFORMATION_SCHEMA.TABLES
                                WHERE TABLE_TYPE = 'BASE TABLE'
                                      AND TABLE_CATALOG='NhanesLandingZone' AND TABLE_SCHEMA = 'Translated'")$TABLE_NAME

   # query table names from Metadata.QuestionnaireVariables
   .validTables <<- .nhanesQuery("SELECT DISTINCT TableName FROM Metadata.QuestionnaireVariables;")$TableName

  }

}

.onUnload <- function(libpath)
{
  DBI::dbDisconnect(cn)
}