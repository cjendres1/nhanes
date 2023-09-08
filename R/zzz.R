##notes

.onLoad = function(libname, pkgname){
  .container_version <<- Sys.getenv("EPICONDUCTOR_CONTAINER_VERSION",unset=NA)
  .collection_date <<- as.Date(Sys.getenv("EPICONDUCTOR_COLLECTION_DATE"),unset=NA)

  if(!is.null(.container_version) & !is.null(.collection_date)){
    message("EpiConductor Container Version: ", .container_version)
    message("Data Collection Date: ", .collection_date)
  }else{
    message("No Container is detected")
  }

}

