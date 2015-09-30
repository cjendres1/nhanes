#nhaneS - retrieve data from the CDC NHANES repository
nhanesURL <- 'http://wwwn.cdc.gov/Nchs/Nhanes/'

# Create a list of nhanes groups
# Include convenient aliases
nhanes_group <- list()
nhanes_group['DEMO']          <- "DEMOGRAPHICS"
nhanes_group['DEMOGRAPHICS']  <- "DEMOGRAPHICS"
nhanes_group['DIETARY']       <- "DIETARY"
nhanes_group['DIET']          <- "DIETARY"
nhanes_group['EXAMINATION']   <- "EXAMINATION"
nhanes_group['EXAM']          <- "EXAMINATION"
nhanes_group['LABORATORY']    <- "LABORATORY"
nhanes_group['LAB']           <- "LABORATORY"
nhanes_group['QUESTIONNAIRE'] <- "QUESTIONNAIRE"
nhanes_group['Q']             <- "QUESTIONNAIRE"
nhanes_survey_groups <- unlist(unique(nhanes_group))

# Although continuous NHANES is grouped in 2-year intervals,
# for convenience we want to specify using a single year
nh_years <- list()
nh_years['1999'] <- "1999-2000"
nh_years['2000'] <- "1999-2000"
nh_years['2001'] <- "2001-2002"
nh_years['2002'] <- "2001-2002"
nh_years['2003'] <- "2003-2004"
nh_years['2004'] <- "2003-2004"
nh_years['2005'] <- "2005-2006"
nh_years['2006'] <- "2005-2006"
nh_years['2007'] <- "2007-2008"
nh_years['2008'] <- "2007-2008"
nh_years['2009'] <- "2009-2010"
nh_years['2010'] <- "2009-2010"
nh_years['2011'] <- "2011-2012"
nh_years['2012'] <- "2011-2012"
nh_years['2013'] <- "2013-2014"
nh_years['2014'] <- "2013-2014"
nh_years['2015'] <- "2015-2016"
nh_years['2016'] <- "2015-2016"

# Continuous NHANES table names have a letter suffix that indicates the collection interval
data_idx <- list()
data_idx["A"] <- '1999-2000'
data_idx["a"] <- '1999-2000'
data_idx["B"] <- '2001-2002'
data_idx["b"] <- '2001-2002'
data_idx["C"] <- '2003-2004'
data_idx["c"] <- '2003-2004'
data_idx["D"] <- '2005-2006'
data_idx["E"] <- '2007-2008'
data_idx["F"] <- '2009-2010'
data_idx["G"] <- '2011-2012'
data_idx["H"] <- '2013-2014'
data_idx["I"] <- '2015-2016'

anomalytables2005 <- c('CHLMD_DR', 'SSUECD_R', 'HSV_DR')

# An internal function that converts a year into the nhanes interval.
# E.g. 2003 is converted to '2003-2004'
# @param year where year is numeric in yyyy format
# @return The 2-year interval that includes the year, e.g. 2001-2002
# 
#------------------------------------------------------------------------------
get_year_from_nh_table <- function(nh_table) {
if(nh_table %in% anomalytables2005) {return('2005-2006')}
nhloc <- data.frame(stringr::str_locate_all(nh_table, '_'))
nn <- nrow(nhloc)
if(nn!=0){
  if((nhloc$start[nn]+1) == nchar(nh_table)) {
    idx <- str_sub(nh_table, -1, -1)
    if(idx=='r'||idx=='R') {
      if(nn > 1) {
        idx <- str_sub(nh_table, nhloc$start[nn-1]+1, nhloc$start[nn-1]+1)
      } else {stop('Invalid table name')}
    }
    return(nh_year <- data_idx[idx])
  } else { ## Assume it's from the first set
    return("1999-2000")}
} else 
  nh_year <- "1999-2000"
}
#------------------------------------------------------------------------------

get_nh_survey_years <- function(year) {
  if(as.character(year) %in% names(nh_years)) {
    return( as.character(nh_years[as.character(year)]) )
  }
  else {
    stop('Data for year ', year, ' are not available')
    return(NULL)
  }
}

xpath <- '//*[@id="ContentPlaceHolder1_GridView1"]'

#------------------------------------------------------------------------------

#' Returns a list of table names for the specified survey group.
#' 
#' Enables quick display of all available tables in the survey group.
#' 
#' @importFrom stringr str_replace str_c str_match str_to_title
#' @importFrom rvest xml_nodes html_table
#' @importFrom xml2 read_html
#' @importFrom magrittr %>%
#' @param nh_surveygroup The type of survey (DEMOGRAPHICS, DIETARY, EXAMINATION, LABORATORY, QUESTIONNAIRE).
#' Abbreviated terms may also be used: (DEMO, DIET, EXAM, LAB, Q).
#' @param year The year in yyyy format where 1999 <= yyyy <= 2012.
#' @param nchar Truncates the table description to a max length of nchar.
#' @param details If TRUE then a more detailed description of the tables is returned.
#' @param namesonly If TRUE then only the table names are returned.
#' @param includerdc If TRUE then RDC only tables are included in list.
#' @return The names of the tables in the specified survey group.
#' @details Data are retrieved via web scraping using html wrappers from package rvest.
#' It is often useful to display the table names in an NHANES survey. In effect this
#' is a convenient way to browse the available NHANES tables.
#' @examples
#' \donttest{nhanesTables('EXAM', 2007)}
#' nhanesTables('LAB', 2009, details=TRUE, includerdc=TRUE)
#' nhanesTables('Q', 2005, namesonly=TRUE)
#' @export
#' 
nhanesTables <- function(nh_surveygroup, year, nchar=100, details = FALSE, namesonly=FALSE, includerdc=FALSE) {
  if( !(nh_surveygroup %in% names(nhanes_group)) ) {
    stop("Invalid survey group")
    return(NULL)
  }
  
  nh_year <- get_nh_survey_years(year)
  
  turl <- str_c(nhanesURL, 'search/variablelist.aspx?Component=', 
                str_to_title(as.character(nhanes_group[nh_surveygroup])), 
                '&CycleBeginYear=', unlist(strsplit(as.character(nh_year), '-'))[[1]] , sep='')
  # At this point df contains every table
  df <- as.data.frame(turl %>% read_html() %>% xml_nodes(xpath=xpath) %>% html_table())
  # By default we exclude RDC Only tables as those cannot be downloaded
  if( includerdc == FALSE) {
    df <- df[(df$Use.Constraints != "RDC Only"),]
  }
  
  if(details == TRUE) {
    df <- unique(df[,3:length(df)])
    colnames(df)[colnames(df)=="Data.File.Name"] <- "FileName"
    colnames(df)[colnames(df)=="Data.File.Description"] <- "Description"
  }
  else {
    df <- unique(df[,c('Data.File.Name', 'Data.File.Description')])
    names(df) <- c('FileName', 'Description')
  }
  
  #Here we exclude tables that overlap from earlier surveys
  # Get possible table suffixes for the specified year
  if( nh_year != "1999-2000") { ## No exclusion needed for first survey
    suffix <- names(data_idx[which(data_idx == nh_year)])
    suffix <- unlist(lapply(suffix, function(x) {str_c('_', x, sep='')}))
    if(nh_year == '2005-2006') {suffix <- c(suffix, anomalytables2005)}
    matches <- unique(grep(paste(suffix,collapse="|"), df[['FileName']], value=TRUE))  
    df <- df[(df$FileName %in% matches),]
  }
  row.names(df) <- c(1:nrow(df))
  if( namesonly == TRUE ) {
    return(as.character(df[[1]]))
  }
  df$Description <- strtrim(df$Description, nchar)
  return(df)  
}

#------------------------------------------------------------------------------

#' Displays a list of variables in the specified NHANES table.
#' 
#' Enables quick display of table variables and their definitions.
#' 
#' @importFrom stringr str_replace str_c str_sub
#' @importFrom rvest xml_nodes html_table
#' @importFrom xml2 read_html
#' @importFrom magrittr %>%
#' @param nh_surveygroup The type of survey (DEMOGRAPHICS, DIETARY, EXAMINATION, LABORATORY, QUESTIONNAIRE).
#' Abbreviated terms may also be used: (DEMO, DIET, EXAM, LAB, Q).
#' @param nh_table The name of the specific table to retrieve.
#' @param details If TRUE then only the variable names and descriptions are returned, which is often sufficient.
#' @param nchar The number of characters in the Variable Description to print. Values are limited to 0<=nchar<=128.
#' This is used to enhance readability, cause variable descriptions can be very long.
#' @param namesonly If TRUE then only the variable names are returned.
#' @return The names of the tables in the specified survey group
#' @details Data are retrieved via web scraping using html wrappers from package rvest.
#' Each data table contains multiple, sometimes more than 100, fields. It is helpful to list the field
#' descriptions to ascertain quickly if a data table is of interest.
#' @examples
#' nhanesTableVars('LAB', 'CBC_E')
#' nhanesTableVars('EXAM', 'OHX_E', details=TRUE, nchar=50)
#' nhanesTableVars('DEMO', 'DEMO_F', namesonly = TRUE)
#' @export
#' 
nhanesTableVars <- function(nh_surveygroup, nh_table, details = FALSE, nchar=100, namesonly = FALSE) {
  if( !(nh_surveygroup %in% names(nhanes_group)) ) {
    stop("Invalid survey group")
    return(NULL)
  }
  
  nh_year <- get_year_from_nh_table(nh_table)
  turl <- str_c(nhanesURL, 'search/variablelist.aspx?Component=', 
                str_to_title(as.character(nhanes_group[nh_surveygroup])), 
                '&CycleBeginYear=', unlist(strsplit(as.character(nh_year), '-'))[[1]] , sep='')
  df <- as.data.frame(turl %>% read_html() %>% xml_nodes(xpath=xpath) %>% html_table())

  if(!(nh_table %in% df$Data.File.Name)) {
    stop('Table ', nh_table, ' not present in the ', nh_surveygroup, ' survey' )
    return(NULL)
  }
  
  nchar_max <- 128
  if(nchar > nchar_max) {
    nchar <- nchar_max
  }
  if( details == FALSE ) { # If TRUE then only return the variable name and description
    df <- df[df$Data.File.Name == nh_table,1:2]
  } else {
    df <- df[df$Data.File.Name == nh_table,]
  }
  df[[2]] <- str_sub(df[[2]],1,nchar)
  row.names(df) <- NULL
  if( namesonly == TRUE ) {
    return(as.character(unique(df[[1]])))
  }
  return(unique(df))
}

#------------------------------------------------------------------------------

#' Download an NHANES table and return as a data frame.
#' 
#' Use to download NHANES data tables that are in SAS format.
#' 
#' @importFrom Hmisc sasxport.get
#' @importFrom stringr str_c
#' @param nh_table The name of the specific table to retrieve.
#' @return The table is returned as a data frame.
#' @details Downloads a table from the NHANES website in its entirety. NHANES tables 
#' are stored in SAS '.XPT' format. Function nhanes uses sasxport.get from package H
#' misc to retrieve the data.
#' @examples 
#' nhanes('BPX_E')
#' nhanes('FOLATE_F')
#' @export
#' 
nhanes <- function(nh_table) {
  nht <- tryCatch({    
    nh_year <- get_year_from_nh_table(nh_table)
    url <- str_c(nhanesURL, nh_year, '/', nh_table, '.XPT', collapse='')
    return(sasxport.get(url))
  },
  error = function(cond) {
    message(paste("Data set ", nh_table,  " is not available"), collapse='')
#    message(url)
    return(NULL)
  },
  warning = function(cond) {
    message(cond, '\n')    
  }  
  )
  return(nht)
}

#------------------------------------------------------------------------------

#' Returns the attributes of an NHANES data table.
#' 
#' Returns attributes such as number of rows, columns, and memory size,
#' but does not reutrn the table itself.
#' 
#' @importFrom Hmisc sasxport.get
#' @importFrom stringr str_c
#' @importFrom utils object.size
#' @param nh_table The name of the specific table to retrieve
#' @return The following attributes are returned as a list \cr
#' nrow = number of rows \cr
#' ncol = number of columns \cr
#' names = name of each column \cr
#' unique = true if all SEQN values are unique \cr
#' na = number of 'NA' cells in the table \cr
#' size = total size of table in bytes \cr
#' types = data types of each column
#' @details nhanesAttr allows one to check the size and other charactersistics of a data table 
#' before importing into R. To retrieve these characteristics, the specified table is downloaded,
#' characteristics are determined, then the table is deleted.
#' @examples 
#' nhanesAttr('BPX_E')
#' nhanesAttr('FOLATE_F')
#' @export
#' 
nhanesAttr <- function(nh_table) {
  nht <- tryCatch({    
    nh_year <- get_year_from_nh_table(nh_table)
    url <- str_c(nhanesURL, nh_year, '/', nh_table, '.XPT', collapse='')
    tmp <- sasxport.get(url)
    nhtatt <- attributes(tmp)
    nhtatt$row.names <- NULL
    nhtatt$nrow <- nrow(tmp)
    nhtatt$ncol <- ncol(tmp)
    nhtatt$unique <- (length(unique(tmp$SEQN)) == nhtatt$nrow)
    nhtatt$na <- sum(is.na(tmp))
    nhtatt$size <- object.size(tmp)
    nhtatt$types <- sapply(tmp,class)
    rm(tmp)
    return(nhtatt)
  },
  error = function(cond) {
    message(paste("Data from ", nh_table, " are not available"))
    return(NA)
  },
  warning = function(cond) {
    message(cond, '\n')    
  }  
  )
  return(nht)  
}

#------------------------------------------------------------------------------

#' Display code translation information for the specified table.
#' 
#' Returns code translations which is especially useful for categorical tables, 
#' which includes most NHANES tables. 
#' 
#' @importFrom stringr str_c str_locate str_sub 
#' @importFrom rvest xml_nodes html_table
#' @importFrom xml2 read_html
#' @importFrom plyr mapvalues
#' @param nh_table The name of the NHANES table to retrieve.
#' @param colnames The names of the columns to translate.
#' @param data If a data frame is passed, then code translation will be applied directly to the data frame. \cr
#' In that case the return argument is the code-translated data frame.
#' @param nchar Applies only when data is defined. Code translations can be very long. \cr
#' Truncate the length by setting nchar. Default is nchar = 32.
#' @param details If TRUE. then all available table translation information is displayed.
#' @return The code translation table (or translated data frame when data is defined).
#' @details Code translation tables are retrieved via webscraping using rvest. 
#' Many of the NHANES data tables have encoded values. E.g. 1 = 'Male', 2 = 'Female'.
#' Thus it is often helpful to view the code translations and perhaps insert the translated values
#' in a data frame. Note that Hmisc supports "labelled" fields. When a translation is applied directly
#' to a column in a data frame, the column class is first converted to 'factor' and then the coded
#' values are replaced with the code translations.
#' @examples
#' nhanesTranslate('DEMO_B', c('DMDBORN','DMDCITZN'))
#' nhanesTranslate('BPX_F', 'BPACSZ', details=TRUE)
#' @export
#' 
nhanesTranslate <- function(nh_table, colnames, data = NULL, nchar = 32, details=FALSE) {
  if(is.null(colnames)) {
    message('Column name is required')
    return(0)
  }
  
  # Parse nh_table to find the suffix, e.g. for table 'BPX_E', the suffix is '_E'
  # If there is no suffix, then we are likely dealing with data from 1999-2000

  nh_year <- get_year_from_nh_table(nh_table)
  if(is.null(nh_year)) {
    return(NULL)
  }  
  
  get_translation_table <- function(colname) {
    xpt <- str_c('//*[h3[a[@name="', colname, '"]]]', sep='')
    tabletree <- url %>% read_html() %>% xml_nodes(xpath=xpt)
    if(length(tabletree)>0) {
      tabletrans <- as.data.frame(xml_nodes(tabletree, 'table') %>% html_table())
    } else {
      warning(c('Column "', colname, '" not found'), collapse='')
      return(NULL)
    }
    
    if(length(tabletrans) > 0) {
      if(details == FALSE) {
        tabletrans <- tabletrans[,c('Code.or.Value', 'Value.Description')]
      }
      return(tabletrans)
    } else { 
      warning(c('No translation table is available for ', colname), collapse='')
      return(NULL)
    }
  }
  
  url <- str_c(nhanesURL, nh_year, '/', nh_table, '.htm', sep='')
  translations <- lapply(colnames, get_translation_table)
  names(translations) <- colnames
  
  nchar_max <- 128
  if(nchar > nchar_max) {
    nchar <- nchar_max
  }
  
  if(is.null(data)) { ## If no data to translate then just return the translation table
    return(Filter(Negate(function(x) is.null(unlist(x))), translations))
  } else {
    #    message("Need to decide what to do when data are passed in")
    translations <- Filter(Negate(function(x) is.null(unlist(x))), translations)
    colnames     <- as.list(names(translations))
    
    translated <- c() ## Let's keep track of columns that were translated
    notfound   <- c() ## Keep track of columns that were not found
    nskip <- grep('Range', translations)
    for( i in 1:length(colnames) ) {
      if(!(i %in% nskip)) {
        cname <- unlist(colnames[i])
        sstr <- str_c('^', cname, '$') # Construct the search string
        idx <- grep(sstr, names(data), ignore.case=TRUE) # Fields are lower case in data
        if(idx>0) { ## The column is present. Next we need to decide if it should be translated.
          if(length(levels(as.factor(data[[idx]]))) > 1) {
            data[[idx]] <- as.factor(data[[idx]])
            data[[idx]] <- suppressMessages(mapvalues(data[[idx]], from = translations[[cname]][['Code.or.Value']], 
                                                      to = strtrim(translations[[cname]][['Value.Description']], nchar)))
            translated <- c(translated, cname) }
        } else {
          notfound <- c(notfound, cname)
        }
      }
    }
    
    if(length(translated) > 0) {
      message(paste(c("Translated columns:", translated), collapse = ' '))
      if(length(notfound) > 0)
        message(paste(c("Columns not found:", notfound), collapse = ' '))
    } else {
      warning("No columns were translated")
    }
    return(data)
  }
}

#------------------------------------------------------------------------------
