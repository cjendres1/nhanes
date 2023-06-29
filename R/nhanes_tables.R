# FUNCTIONS:
#   nhanesTables
#   nhanesTableVars
#
#------------------------------------------------------------------------------
#' Returns a list of table names for the specified survey group.
#' 
#' Enables quick display of all available tables in the survey group.
#' 
#' @importFrom stringr str_replace str_c str_match str_to_title str_sub str_split str_remove str_detect
#' @importFrom rvest html_elements html_table
#' @importFrom xml2 read_html
#' @importFrom magrittr %>%
#' @param data_group The type of survey (DEMOGRAPHICS, DIETARY, EXAMINATION, LABORATORY, QUESTIONNAIRE).
#' Abbreviated terms may also be used: (DEMO, DIET, EXAM, LAB, Q).
#' @param year The year in yyyy format where 1999 <= yyyy.
#' @param nchar Truncates the table description to a max length of nchar.
#' @param details If TRUE then a more detailed description of the tables is returned (default=FALSE).
#' @param namesonly If TRUE then only the table names are returned (default=FALSE).
#' @param includerdc If TRUE then RDC only tables are included in list (default=FALSE).
#' @return Returns a data frame that contains table attributes. If namesonly=TRUE,
#' then a character vector of table names is returned.
#' @details Function nhanesTables retrieves a list of tables and a 
#' description of their contents from the NHANES website. This provides
#' a convenient way to browse the available tables. NULL is returned when an
#' HTML read error is encountered.
#' @examples
#' nhanesTables('EXAM', 2007)
#' \donttest{nhanesTables('LAB', 2009, details=TRUE, includerdc=TRUE)}
#' \donttest{nhanesTables('Q', 2005, namesonly=TRUE)}
#' \donttest{nhanesTables('DIET', 'P')}
#' \donttest{nhanesTables('EXAM', 'Y')}
#' @export
#'

nhanesTables <- function(data_group, year, nchar=128, details = FALSE, namesonly=FALSE, includerdc=FALSE) {
  if( !(data_group %in% names(nhanes_group)) ) {
    stop("Invalid survey group")
    return(NULL)
  }
  
  if(year == 'P' | year == 'p') {
    turl <- str_c(nhanesURL, 'search/datapage.aspx?Component=', 
                  str_to_title(as.character(nhanes_group[data_group])), 
                  '&CycleBeginYear=', '2017-2020', sep='')
  } else if(year == 'Y' | year == 'y') {
    turl <- str_c(nhanesURL, 'search/NnyfsData.aspx?Component=', 
                  str_to_title(as.character(nhanes_group[data_group])), 
                  '&CycleBeginYear=', '2012', sep='')
  } else {
    nh_year <- .get_nh_survey_years(year)
    turl <- str_c(nhanesURL, 'search/variablelist.aspx?Component=', 
                  str_to_title(as.character(nhanes_group[data_group])), 
                  '&CycleBeginYear=', unlist(str_split(as.character(nh_year), '-'))[[1]] , sep='')
  }
  
  # At this point df contains every table for the specified survey & year
  hurl <- .checkHtml(turl)
  if(is.null(hurl)) {
    message("Error occurred during read. No tables returned")
    return(NULL)
  }
  df <- as.data.frame(hurl %>% html_elements(xpath=xpath) %>% html_table())
  #  df <- as.data.frame(turl %>% read_html() %>% html_elements(xpath=xpath) %>% html_table())
  # By default we exclude RDC Only tables as those cannot be downloaded
  
  if(nrow(df)==0) {
    if(year %in% c(2019,2020)) {
      message("No tables found. Please set year='P' for Pre-Pandemic data.")
    } else {
      message("No tables found")
    }
    return(NULL)
  }
  
  if(year %in% c('P', 'p', 'Y', 'y')) {
    if(year %in% c('P','p')) {
      df <- df[str_detect(df$Data.File,'^P_'),]
    }
    if(details) {
      df <- unique(df) 
    } else {
      df <- unique(df[,c('Doc.File', 'Data.File.Name')])
    }
    #    if(!includerdc) {
    #      df <- df[(df$Data.File != "RDC Only"),]
    #    }
    
    if(namesonly == TRUE) {
      df$Doc.File <- str_remove(df$Doc.File, ' Doc')
      return(as.character(df$Doc.File))
    } else {
      row.names(df) <- NULL
      return(df)
    }
  }
  
  if(!(nhanes_group[data_group]=='NON-PUBLIC')){
    if(!includerdc) {
      df <- df[(df$Use.Constraints != "RDC Only"),]
    }
  }
  
  if(details) {
    df <- unique(df[,3:length(df)])
  } else {
    df <- unique(df[,c('Data.File.Name', 'Data.File.Description')])
  }
  #  df <- rename(df, c("Data.File.Name"="FileName","Data.File.Description"="Description"))
  
  #Here we exclude tables that overlap from earlier surveys
  # Get possible table suffixes for the specified year
  if(nh_year != "1999-2000") { ## No exclusion needed for first survey
    suffix <- names(data_idx[which(data_idx == nh_year)])
    suffix <- unlist(lapply(suffix, function(x) {str_c('_', x, sep='')}))
    if(nh_year == '2005-2006') {suffix <- c(suffix, anomalytables2005)}
    matches <- unique(grep(paste(suffix,collapse="|"), df[['Data.File.Name']], value=TRUE))  
    df <- df[(df$Data.File.Name %in% matches),]
  }
  if(namesonly) {
    return(as.character(df[[1]]))
  }
  df$Data.File.Description <- str_sub(df$Data.File.Description, 1, nchar)
  row.names(df) <- NULL
  return(df)  
}

#------------------------------------------------------------------------------
#' Displays a list of variables in the specified NHANES table.
#' 
#' Enables quick display of table variables and their definitions.
#' 
#' @importFrom stringr str_replace str_c str_sub str_split
#' @importFrom rvest html_elements html_table
#' @importFrom xml2 read_html
#' @importFrom magrittr %>%
#' @param data_group The type of survey (DEMOGRAPHICS, DIETARY, EXAMINATION, LABORATORY, QUESTIONNAIRE).
#' Abbreviated terms may also be used: (DEMO, DIET, EXAM, LAB, Q).
#' @param nh_table The name of the specific table to retrieve.
#' @param details If TRUE then all columns in the variable description are returned (default=FALSE).
#' @param nchar The number of characters in the Variable Description to print. Default length is 128,
#' which is set to enhance readability cause variable descriptions can be very long.
#' @param namesonly If TRUE then only the variable names are returned (default=FALSE).
#' @return Returns a data frame that describes variable attributes for the specified table. If namesonly=TRUE,
#' then a character vector of the variable names is returned.
#' @details NHANES tables may contain more than 100 variables. Function nhanesTableVars provides a concise display
#' of variables for a specified table, which helps to ascertain quickly if the table is of interest. 
#' NULL is returned when an HTML read error is encountered.
#' @examples
#' \donttest{nhanesTableVars('LAB', 'CBC_E')}
#' \donttest{nhanesTableVars('EXAM', 'OHX_E', details=TRUE, nchar=50)}
#' \donttest{nhanesTableVars('DEMO', 'DEMO_F', namesonly = TRUE)}
#' @export
#' 
nhanesTableVars <- function(data_group, nh_table, details = FALSE, nchar=128, namesonly = FALSE) {
  if( !(data_group %in% names(nhanes_group)) ) {
    stop("Invalid survey group")
    return(NULL)
  }
  
  if(length(grep('^P_', nh_table))>0){
    nh_year <- '2017-2020'
    turl <- str_c(nhanesURL, 'search/variablelist.aspx?Component=', 
                  str_to_title(as.character(nhanes_group[data_group])), 
                  '&Cycle=', nh_year , sep='')
    
  } else {
    nh_year <- .get_year_from_nh_table(nh_table)
    turl <- str_c(nhanesURL, 'search/variablelist.aspx?Component=', 
                  str_to_title(as.character(nhanes_group[data_group])), 
                  '&CycleBeginYear=', unlist(str_split(as.character(nh_year), '-'))[[1]] , sep='') 
  }
  
  hurl <- .checkHtml(turl) 
  if(is.null(hurl)) {
    message("Error occurred during read. No table variables returned")
    return(NULL)
  }
  df <- as.data.frame(hurl %>% html_elements(xpath=xpath) %>% html_table())
  #  df <- as.data.frame(turl %>% read_html() %>% html_elements(xpath=xpath) %>% html_table())
  
  if(!(nh_table %in% df$Data.File.Name)) {
    stop('Table ', nh_table, ' not present in the ', data_group, ' survey' )
    return(NULL)
  }
  
  #nchar_max <- 128
  if(nchar > nchar_max) {
    nchar <- nchar_max
  }
  if( details == FALSE ) { # If TRUE then only return the variable name and description
    df <- df[df$Data.File.Name == nh_table,1:2]
  } else {
    df <- df[df$Data.File.Name == nh_table,]
  }
  df[[2]] <- str_sub(df[[2]],1,nchar)
  if( namesonly == TRUE ) {
    return(as.character(unique(df[[1]])))
  }
  row.names(df) <- NULL
  return(unique(df))
}

