#nhanesA - retrieve data from the CDC NHANES repository
nhanesURL <- 'http://wwwn.cdc.gov/Nchs/Nhanes/'
varURL <- 'http://wwwn.cdc.gov/nchs/nhanes/search/variablelist.aspx'

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

#------------------------------------------------------------------------------
# An internal function that determines which survey year the table belongs to.
# For most tables the year can be determined by the letter suffix.
get_year_from_nh_table <- function(nh_table) {
if(nh_table %in% anomalytables2005) {return('2005-2006')}
nhloc <- data.frame(stringr::str_locate_all(nh_table, '_'))
nn <- nrow(nhloc)
if(nn!=0){ #Underscores were found
  if((nhloc$start[nn]+1) == nchar(nh_table)) {
    idx <- str_sub(nh_table, -1, -1)
    if(idx=='r'||idx=='R') {
      if(nn > 1) {
        newloc <- nhloc$start[nn-1]+1
        idx <- str_sub(nh_table, newloc, newloc)
      } else {stop('Invalid table name')}
    }
    return(data_idx[idx])
  } else { ## Underscore not 2nd to last. Assume table is from the first set.
    return("1999-2000")}
} else #If there are no underscores then table must be from first survey
  nh_year <- "1999-2000"
}

#------------------------------------------------------------------------------
# An internal function that converts a year into the nhanes interval.
# E.g. 2003 is converted to '2003-2004'
# @param year where year is numeric in yyyy format
# @return The 2-year interval that includes the year, e.g. 2001-2002
# 
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
#' @importFrom stringr str_replace str_c str_match str_to_title str_sub str_split
#' @importFrom rvest xml_nodes html_table
#' @importFrom xml2 read_html
#' @importFrom magrittr %>%
#' @importFrom plyr rename
#' @param nh_surveygroup The type of survey (DEMOGRAPHICS, DIETARY, EXAMINATION, LABORATORY, QUESTIONNAIRE).
#' Abbreviated terms may also be used: (DEMO, DIET, EXAM, LAB, Q).
#' @param year The year in yyyy format where 1999 <= yyyy <= 2014.
#' @param nchar Truncates the table description to a max length of nchar.
#' @param details If TRUE then a more detailed description of the tables is returned.
#' @param namesonly If TRUE then only the table names are returned.
#' @param includerdc If TRUE then RDC only tables are included in list.
#' @return The names of the tables in the specified survey group.
#' @details Data are retrieved via web scraping using html wrappers from package rvest.
#' It is often useful to display the table names in an NHANES survey. In effect this
#' is a convenient way to browse the available NHANES tables.
#' @examples
#' nhanesTables('EXAM', 2007)
#' nhanesTables('LAB', 2009, details=TRUE, includerdc=TRUE)
#' \donttest{nhanesTables('Q', 2005, namesonly=TRUE)}
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
                '&CycleBeginYear=', unlist(str_split(as.character(nh_year), '-'))[[1]] , sep='')
  # At this point df contains every table
  df <- as.data.frame(turl %>% read_html() %>% xml_nodes(xpath=xpath) %>% html_table())
  # By default we exclude RDC Only tables as those cannot be downloaded
  if(!includerdc) {
    df <- df[(df$Use.Constraints != "RDC Only"),]
  }
  
  if(details) {
    df <- unique(df[,3:length(df)])
  } else {
    df <- unique(df[,c('Data.File.Name', 'Data.File.Description')])
  }
  df <- rename(df, c("Data.File.Name"="FileName","Data.File.Description"="Description"))
  
  #Here we exclude tables that overlap from earlier surveys
  # Get possible table suffixes for the specified year
  if(nh_year != "1999-2000") { ## No exclusion needed for first survey
    suffix <- names(data_idx[which(data_idx == nh_year)])
    suffix <- unlist(lapply(suffix, function(x) {str_c('_', x, sep='')}))
    if(nh_year == '2005-2006') {suffix <- c(suffix, anomalytables2005)}
    matches <- unique(grep(paste(suffix,collapse="|"), df[['FileName']], value=TRUE))  
    df <- df[(df$FileName %in% matches),]
  }
  if(namesonly) {
    return(as.character(df[[1]]))
  }
  df$Description <- str_sub(df$Description, 1, nchar)
  row.names(df) <- NULL
  return(df)  
}

#------------------------------------------------------------------------------
#' Displays a list of variables in the specified NHANES table.
#' 
#' Enables quick display of table variables and their definitions.
#' 
#' @importFrom stringr str_replace str_c str_sub str_split
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
                '&CycleBeginYear=', unlist(str_split(as.character(nh_year), '-'))[[1]] , sep='')
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
  if( namesonly == TRUE ) {
    return(as.character(unique(df[[1]])))
  }
  row.names(df) <- NULL
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
#' are stored in SAS '.XPT' format. Function nhanes uses sasxport.get from package Hmisc 
#' to retrieve the data.
#' @examples 
#' nhanes('BPX_E')
#' nhanes('FOLATE_F')
#' @export
#' 
nhanes <- function(nh_table) {
  nht <- tryCatch({    
    nh_year <- get_year_from_nh_table(nh_table)
    url <- str_c(nhanesURL, nh_year, '/', nh_table, '.XPT', collapse='')
    return(sasxport.get(url, lowernames=FALSE))
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
#' Import Dual Energy X-ray Absorptiometry (DXA) data.
#' 
#' DXA data were acquired from 1999-2006. 
#' 
#' @importFrom stringr str_c
#' @importFrom Hmisc sasxport.get
#' @importFrom utils download.file
#' @param year The year of the data to import, where 1999<=year<=2006. 
#' @param suppl If TRUE then retrieve the supplemental data.
#' @param destfile The name of a destination file. If NULL then the data are imported 
#' into the R environment but no file is created.
#' @return By default the table is returned as a data frame. When downloading to file, the return argument
#' is the integer code from download.file where 0 means success and non-zero indicates failure to download.
#' @details  Provide destfile in order to write the data to file. If destfile is not provided then
#' the data will be imported into the R environment.
#' @examples
#' \donttest{dxa_b <- nhanesDXA(2001)}
#' \donttest{dxa_c_s <- nhanesDXA(2003, suppl=TRUE)}
#' \donttest{nhanesDXA(1999, destfile="dxx.xpt")}
#' @export
nhanesDXA <- function(year, suppl=FALSE, destfile=NULL) {
  dxaURL <- "ftp://ftp.cdc.gov/pub/health_Statistics/nchs/nhanes/dxx/"
  
  dxa_fname <- function(year, suppl) {
    if(year == 1999 | year == 2000) {fname = 'dxx'}
    else if(year == 2001 | year == 2002) {fname = 'dxx_b'}
    else if(year == 2003 | year == 2004) {fname = 'dxx_c'}
    else if(year == 2005 | year == 2006) {fname = 'DXX_D'}
    if(suppl == TRUE) {
      if(year == 2005 | year == 2006) {
        fname <- str_c(fname, '_S', collapse='')
      } else {fname <- str_c(fname, '_s', collapse='')}
    }
    return(fname)
  }
  
  if(year) {
    if(!(as.character(year) %in% c('1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006'))) {
      stop("Invalid survey year for DXA data")
    } else {
      fname <- dxa_fname(year, suppl)
      url <- str_c(dxaURL, fname, '.xpt', collapse='')
      if(!is.null(destfile)) {
        ok <- download.file(url, destfile, mode="wb", quiet=TRUE)
        return(ok)
      } else {
        tf <- tempfile()
        download.file(url, tf, mode="wb", quiet=TRUE)
        return(sasxport.get(tf,lowernames=FALSE))
      }
    }
  } else { # Year not provided - no data will be returned
    stop("Year is required")
  }
}

#------------------------------------------------------------------------------
#' Returns the attributes of an NHANES data table.
#' 
#' Returns attributes such as number of rows, columns, and memory size,
#' but does not return the table itself.
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
    tmp <- sasxport.get(url,lowernames=FALSE)
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
#' Perform a search over the comprehensive NHANES variable list.
#' 
#' The descriptions in the master variable list will be filtered by the
#' provided search terms to retrieve a list of relevant variables. 
#' The search can be restricted to specific survey years by specifying ystart and/or ystop.
#' 
#' @importFrom rvest html_table
#' @importFrom xml2 read_html
#' @param search_terms List of terms or keywords.
#' @param ignore.case Ignore case if TRUE. Default value is FALSE.
#' @param ystart Four digit year of first survey included in search, where ystart >= 1999.
#' @param ystop  Four digit year of final survey included in search, where ystop >= ystart.
#' @param includerdc If TRUE then RDC only tables are included in list.
#' @param nchar Truncates the variable description to a max length of nchar.
#' @param namesonly If TRUE then only the table names are returned.
#' @return A list of tables that match the search terms. 
#' If namesonly=TRUE, then only the table names are returned.
#' @details nhanesSearch is useful to obtain a comprehensive list of relevant tables.
#' @examples
#'  \donttest{nhanesSearch("bladder", ystart=2001, ystop=2008, nchar=50)}
#'  \donttest{nhanesSearch("urin", ignore.case=TRUE, ystart=2009, namesonly=TRUE)}
#'  \donttest{nhanesSearch(c("urine", "urinary"), ignore.case=TRUE, ystop=2006, namesonly=TRUE)}
#' @export
#' 
nhanesSearch <- function(search_terms=NULL, ignore.case=FALSE, ystart=NULL, ystop=NULL, includerdc=FALSE, nchar=100, namesonly=FALSE){
  is.even <- function(x) {x %% 2 == 0}
  df  <- data.frame(read_html(varURL) %>% html_table())
  if(includerdc == FALSE){
    df <- df[(df$Use.Constraints != "RDC Only"),]
  }
  if(is.null(search_terms)) {
    if(namesonly == TRUE) {
      return(unique(df$Data.File.Name))
    } else { return(df) }
  }
  df <- df[grep(paste(search_terms,collapse="|"), df[['Variable.Description']], ignore.case=ignore.case, value=FALSE),]
  
  if(!is.null(ystop)){  # ystop has been provided
    if(is.numeric(ystop)) {
      if(ystop < 1999) {stop("Invalid stop year")}
    } else {
      stop(paste( c(ystop, "is not a valid stop year"), collapse=' '))
    }
    if(!is.null(ystart)) { # ystart has also been provided
      if(!is.numeric(ystart)) {stop("Start year (ystart) must be a 4-digit year")}
      if( ystart > ystop ) {
        stop('Stop year (ystop) cannot precede the Start year (ystart)')
      } else { #Determine if Start year is odd or even
        if(is.even(ystart)) {
          df <- df[(df$EndYear >= ystart),]
        } else {
          df <- df[(df$Begin.Year >= ystart),]
        }
        if(is.even(ystop)) {
          df <- df[(df$EndYear <= ystop),]
        } else {
          df <- df[(df$Begin.Year <= ystop),]
        }
      }
    } else { # No ystart, assume it is 1999 (i.e. the first survey)
      if(is.even(ystop)) {
        df <- df[(df$EndYear <= ystop),]
      } else {
        df <- df[(df$Begin.Year <= ystop),]
      }
    }
  } else if(!is.null(ystart)) { # ystart only, i.e. no ystop
    if(!is.numeric(ystart)) {stop("Start year (ystart) must be a 4-digit year")}
    if(is.even(ystart)) {
      df <- df[(df$EndYear >= ystart),]
    } else {
      df <- df[(df$Begin.Year >= ystart),]
    }
  }
  
  row.names(df) <- NULL
  if(namesonly) {
    return(unique(df$Data.File.Name))
  }
  df$Variable.Description <- str_sub(df$Variable.Description, 1, nchar)
  return(df)
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
#' @param details If TRUE then all available table translation information is displayed.
#' @param dxa If TRUE then the 2005-2006 DXA translation table will be used.
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
#' \donttest{nhanesTranslate('BPX_F', 'BPACSZ', data=nhanes('BPX_F'))}
#' @export
#' 
nhanesTranslate <- function(nh_table, colnames=NULL, data = NULL, nchar = 32, details=FALSE, dxa=FALSE) {
  if(is.null(colnames)) {
    message('Column name is required')
    return(0)
  }
  
  # Parse nh_table to find the suffix, e.g. for table 'BPX_E', the suffix is '_E'
  # If there is no suffix, then we are likely dealing with data from 1999-2000
  
  get_translation_table <- function(colname, url) {
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
  
  if(dxa) {
    code_translation_url <- "http://www.cdc.gov/nchs/nhanes/nhanes2005-2006/DXX_D.htm"
  } else {
    nh_year <- get_year_from_nh_table(nh_table)
    if(is.null(nh_year)) {
      return(NULL)
    }  
    code_translation_url <- str_c(nhanesURL, nh_year, '/', nh_table, '.htm', sep='')
  }
  translations <- lapply(colnames, get_translation_table, code_translation_url)
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
    nskip <- grep('Range', translations) ## 'Range' of values indicates the column is not coded
    for( i in 1:length(colnames) ) {
      if(!(i %in% nskip)) {
        cname <- unlist(colnames[i])
        sstr <- str_c('^', cname, '$') # Construct the search string
        idx <- grep(sstr, names(data)) 
        if(idx>0) { ## The column is present. Next we need to decide if it should be translated.
          if(length(levels(as.factor(data[[idx]]))) > 1) {
            data[[idx]] <- as.factor(data[[idx]])
            data[[idx]] <- suppressMessages(plyr::mapvalues(data[[idx]], from = translations[[cname]][['Code.or.Value']], 
                                                      to = str_sub(translations[[cname]][['Value.Description']], 1, nchar)))
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
#' Open a browser to NHANES.
#' 
#' The browser may be directed to a specific year, survey, or table.
#' 
#' @importFrom stringr str_c str_to_title str_split str_sub str_extract_all
#' @importFrom utils browseURL
#' @param year The year in yyyy format where 1999 <= yyyy <= 2014.
#' @param nh_surveygroup The type of survey (DEMOGRAPHICS, DIETARY, EXAMINATION, LABORATORY, QUESTIONNAIRE).
#' Abbreviated terms may also be used: (DEMO, DIET, EXAM, LAB, Q).
#' @param nh_table The name of an NHANES table.
#' @param dxa If TRUE then browse to the DXA page.
#' @details browseNHANES will open a web browser to the specified NHANES site.
#' @examples
#' browseNHANES()                     # Defaults to the main data sets page
#' browseNHANES(2005)                 # The main page for the specified survey year
#' browseNHANES(2009, 'EXAM')         # Page for the specified year and survey group
#' browseNHANES(nh_table = 'VIX_D')   # Page for a specific table
#' browseNHANES(dxa = TRUE)           # DXA main page
#' @export
#' 

browseNHANES <- function(year=NULL, nh_surveygroup=NULL, nh_table=NULL, dxa=FALSE) {
  if(dxa) {
    browseURL("http://www.cdc.gov/nchs/nhanes/dxx/dxa.htm")
  } else {
    if(!is.null(nh_table)){
      nh_year <- get_year_from_nh_table(nh_table)
      url <- str_c(nhanesURL, nh_year, '/', nh_table, '.htm', sep='')
      browseURL(url)
    } else if(!is.null(year)) {
      if(!is.null(nh_surveygroup)) {
        nh_year <- get_nh_survey_years(year)
        url <- str_c(nhanesURL, 'Search/DataPage.aspx?Component=', 
                     str_to_title(as.character(nhanes_group[nh_surveygroup])), 
                     '&CycleBeginYear=', unlist(str_split(as.character(nh_year), '-'))[[1]] , sep='')
        browseURL(url)
      } else {
        nh_year <- get_nh_survey_years(year)
        nh_year <- str_c(str_sub(unlist(str_extract_all(nh_year,"[[:digit:]]{4}")),3,4),collapse='_')
        url <- str_c(nhanesURL, 'search/nhanes', nh_year, '.aspx', sep='')
        browseURL(url)
      }
    } else {
      browseURL("http://www.cdc.gov/nchs/nhanes/nhanes_questionnaires.htm")
    }
  }
}
#------------------------------------------------------------------------------
