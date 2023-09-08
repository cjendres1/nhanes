# nhanesDB - retrieve nhanes data from the local container
# Laha Ale and Robert Gentleman 06/08/2023


.nhanesTablesDB = function( data_group, year,
                         nchar = 128,  details = FALSE,
                         namesonly = FALSE, includerdc = FALSE ) {
  .checkDataGroupDB(data_group)
  # ##check if they are using the short name
  if( data_group %in% names(nhanes_group) )
    data_group = nhanes_group[data_group]



  if (is.numeric(year))
    EVEN = .is.even(year)
  else stop("Invalid year")
  ##construct SQL queries


  tables = paste0("SELECT TableName AS 'Data.File.Name',
                Description as 'Data.File.Description',
                CONCAT(SUBSTRING(DataGroup,1,1),LOWER(SUBSTRING(DataGroup,2,20))) AS Component,
                Year AS 'Begin.Year',
                (Year+1) AS EndYear
                FROM
                Metadata.QuestionnaireDescriptions where DataGroup='",
                  data_group, "' and Year=",ifelse(EVEN, year-1, year))


  if(details==FALSE){
    tables = paste0("SELECT TableName AS 'Data.File.Name',
                Description as 'Data.File.Description'
                FROM
                Metadata.QuestionnaireDescriptions where DataGroup='",
                    data_group, "' and Year=",ifelse(EVEN, year-1, year))
  }

  df =.nhanesQuery(tables)
  if(namesonly){
    return(unique(df$Data.File.Name))
  } else {
    return(df)
  }
}

.checkDataGroupDB = function(data_group){
  if (!(data_group %in% names(nhanes_group)))
    stop("Invalid survey group!")

}

.nhanesTableVarsDB = function(data_group, nh_table, details = FALSE, nchar=128, namesonly = FALSE) {

  # FIXME: We need to add Use.Constraints when DB is updated
  param = match.call()
  if(is.null(param$nh_table)){
    nh_table = param$data_group
    data_group = NULL
  }
  if(!is.null(data_group)){
    .checkDataGroupDB(data_group)
  }

  .checkTableNames(nh_table)

  sql = paste0("SELECT DISTINCT V.Variable AS 'Variable.Name',
                       SUBSTRING(V.Description,1,",nchar,") AS 'Variable.Description',
                       V.TableName AS 'Data.File.Name',
                       SUBSTRING(Q.[Description],1,",nchar,") AS 'Data.File.Description',
                       Year AS 'Begin.Year',
                       (Year+1) AS EndYear,
                       CONCAT(SUBSTRING(DataGroup,1,1),LOWER(SUBSTRING(DataGroup,2,20))) AS Component
                  FROM Metadata.QuestionnaireDescriptions Q
                  JOIN Metadata.QuestionnaireVariables V ON V.TableName = Q.TableName
                  WHERE V.TableName = '",nh_table,"'")
  if(!is.null(data_group)){
    sql = paste0(sql," AND DataGroup LIKE '",data_group,"%'")
  }

  df =.nhanesQuery(sql)
  if(namesonly){
    return(unique(df$Variable.Name))
  }else if(!details){
    return(df[,c('Variable.Name','Variable.Description')])
  }else{
    return(df)
  }
}


.nhanesDB = function(nh_table,translated=TRUE){
  .checkTableNames(nh_table)
  nh_table = .convertTranslatedTable(nh_table,translated)
  sql = paste0("SELECT * FROM ",nh_table)
 .nhanesQuery(sql)
}


.nhanesSearchVarNameDB = function(varnames = NULL,
                                ystart = NULL,
                                ystop = NULL,
                                includerdc = FALSE,
                                nchar = 128,
                                namesonly = TRUE){

  sql = paste0("SELECT DISTINCT V.Variable AS 'Variable.Name',
                       SUBSTRING(V.Description,1,",nchar,") AS 'Variable.Description',
                       V.TableName AS 'Data.File.Name',
                       SUBSTRING(Q.[Description],1,",nchar,") AS 'Data.File.Description',
                       Year AS 'Begin.Year',
                       (Year+1) AS EndYear,
                       CONCAT(SUBSTRING(DataGroup,1,1),LOWER(SUBSTRING(DataGroup,2,20))) AS Component
                  FROM Metadata.QuestionnaireDescriptions Q
                  JOIN Metadata.QuestionnaireVariables V ON V.TableName = Q.TableName
                  WHERE V.Variable IN (", toString(sprintf("'%s'", varnames)),")")



  if(!is.null(ystart)){
    sql <- paste(sql,"AND Q.Year >=",ystart)
  }
  if(!is.null(ystop)){
    sql <- paste(sql,"AND Q.Year <",ystop)
  }


  df =.nhanesQuery(sql)
  if(is.null(df)){
    warning(paste("Variable ",v, "is not found in the database!"))
  }


  if(namesonly){
    df = unique(df$Data.File.Name)
  }

  df

}


.nhanesSearchTableNamesDB = function(pattern = NULL,
                                    ystart = NULL,
                                    ystop = NULL,
                                    includerdc = FALSE,
                                    includewithdrawn=FALSE,
                                    nchar = 128,
                                    details = FALSE){

  sql <- paste0("SELECT DISTINCT TableName,
                        CONCAT(Q.Year, '-', (Q.Year+1)) AS Years
                      FROM Metadata.QuestionnaireDescriptions Q
                  WHERE TableName LIKE '%",pattern,"%'"
  )
  if(!is.null(ystart)){
    sql = paste(sql,"AND Q.Year >=",ystart)
  }
  if(!is.null(ystop)){
    sql <- paste(sql,"AND Q.Year <",ystop)
  }

  if( includerdc ) warning("The DB has no restricted data")

  df =.nhanesQuery(sql)

  # Fixme: we are still missing the published date in the DB
  # if(!includewithdrawn) {
  #   df = df[!(df$Date.Published=='Withdrawn'),]
  # }

  if(is.null(df) | nrow(df)==0){
    warning(paste("Cannot find any table name like:",pattern,"!"))
  }

  if(details)
    return(df)
  else
    return(unique(df$TableName))
}




.nhanesTranslateDB = function( nh_table, colnames = NULL, data = FALSE, nchar = 32,
                            mincategories = 2, details = FALSE, dxa = FALSE){
  .checkTableNames(nh_table)
  if(data){
    return(.nhanesDB(nh_table))
  }

  if(length(nh_table) > 1 ) stop("you can only select one table")
  if(details){
    sql = "SELECT Variable,CodeOrValue AS 'Code.or.Value',ValueDescription AS 'Value.Description',
            Count,Cumulative,SkipToItem AS 'Skip.to.Item'
            FROM Metadata.VariableCodebook WHERE TableName='"
  } else {
    sql = "SELECT Variable,CodeOrValue AS 'Code.or.Value',ValueDescription AS 'Value.Description'
             FROM Metadata.VariableCodebook WHERE TableName='"
  }
  sql = paste0(sql,nh_table,"'")
  if(!is.null(colnames))
    sql = paste0(sql," AND Variable IN (", toString(sprintf("'%s'", colnames)),")")

  df =.nhanesQuery(sql)
  ans=split(df[-which(names(df)=="Variable")], df$Variable)
  ans=lapply(ans,function(x){row.names(x)=NULL;x}) # reset row names
  ans
}


.nhanesSearchDB = function( search_terms = NULL,
                         exclude_terms = NULL,
                         data_group = NULL,
                         ignore.case = FALSE,
                         ystart = NULL,
                         ystop = NULL,
                         includerdc = FALSE,
                         nchar = 128,
                         namesonly = FALSE){


  sql = paste0("SELECT V.Variable AS 'Variable.Name',
                       SUBSTRING(V.Description,1,",nchar,") AS 'Variable.Description',
                       V.TableName AS 'Data.File.Name',
                       SUBSTRING(Q.[Description],1,",nchar,") AS 'Data.File.Description',
                       Year AS 'Begin.Year',
                       (Year+1) AS EndYear,
                       CONCAT(SUBSTRING(DataGroup,1,1),LOWER(SUBSTRING(DataGroup,2,20))) AS Component
                  FROM Metadata.QuestionnaireDescriptions Q
                  JOIN Metadata.QuestionnaireVariables V ON V.TableName = Q.TableName
                  WHERE (V.Description COLLATE SQL_Latin1_General_CP1_CS_AS LIKE '%")

  # COLLATE SQL_Latin1_General_CP1_CS_AS  : is to make case sensitive pattern match

  sql = paste0(sql,search_terms[1],"%'")
  # match multiple patterns
  if (length(search_terms)>=2){
    for (term in search_terms[2:length(search_terms)]){
      sql = paste0(sql," OR V.Description COLLATE SQL_Latin1_General_CP1_CS_AS LIKE '%",term,"%'")
    }
  }
  sql = paste0(sql,")")



  if(!is.null(exclude_terms)){
    for (term in exclude_terms){
      sql = paste0(sql," AND V.Description COLLATE SQL_Latin1_General_CP1_CS_AS NOT LIKE '%",term,"%'")
    }
  }

  if(ignore.case){
    sql = gsub("COLLATE SQL_Latin1_General_CP1_CS_AS", "", sql)
  }


  if(!is.null(data_group)){
    if(length(data_group)>1){
      sql = paste0(sql," AND (DataGroup LIKE '%",data_group[1],"%'")
      for (term in data_group[2:length(data_group)]){
        sql = paste0(sql," OR DataGroup LIKE '%",term,"%'")
      }
      sql = paste0(sql,")")
    }else{
      sql = paste0(sql," AND DataGroup LIKE '%",data_group,"%'")
    }
  }



  sql = gsub("%\\^", "", sql) # address start with ..

  if(!is.null(ystart)){
    sql <- paste(sql,"AND Q.Year >=",ystart)
  }
  if(!is.null(ystop)){
    sql <- paste(sql,"AND Q.Year <=",ystop)
  }
  df =.nhanesQuery(sql)
  if(namesonly){
    df = unique(df$Data.File.Name)
  }

  df
}


.nhanesCodebookDB = function(nh_table, colname){
  # FIXME: we need handle multiple targets once DB is updated!
  .checkTableNames(nh_table)
  if(length(colname) > 1){
    stop("colname not accepts a list, please provide one colunm name only!")
  }

  sql = paste0("SELECT Variable AS 'Variable Name:',
                       SasLabel AS 'SAS Label:',
                       Description AS 'English Text:',
                       Target AS 'Target:'
                       FROM Metadata.QuestionnaireVariables WHERE TableName='",nh_table,"' AND Variable='",colname,"'")
  res = as.list(.nhanesQuery(sql))
  if(length(res[[1]])==0){
    stop(paste0("The variable \"",colname,"\" is not found in the data file/table \"",nh_table,"\".
                Please check the table and variable name!"))
  }

  res[colname]= .nhanesTranslateDB(nh_table, colname,details = TRUE)

  res
}
