# FUNCTIONS
#  nhanesSearch
#  nhanesSearchTableNames
#  nhanesSearchVarName
#
#------------------------------------------------------------------------------
#' Perform a search over the comprehensive NHANES variable list.
#' 
#' The descriptions in the master variable list will be filtered by the
#' provided search terms to retrieve a list of relevant variables. 
#' The search can be restricted to specific survey years by specifying ystart and/or ystop.
#' 
#' @importFrom rvest html_table
#' @importFrom xml2 xml_children xml_text read_html
#' @param search_terms List of terms or keywords.
#' @param exclude_terms List of exclusive terms or keywords.
#' @param data_group Which data groups (e.g. DIET, EXAM, LAB) to search. Default is to search all groups.
#' @param ignore.case Ignore case if TRUE. (Default=FALSE).
#' @param ystart Four digit year of first survey included in search, where ystart >= 1999.
#' @param ystop  Four digit year of final survey included in search, where ystop >= ystart.
#' @param includerdc If TRUE then RDC only tables are included in list (default=FALSE).
#' @param nchar Truncates the variable description to a max length of nchar.
#' @param namesonly If TRUE then only the table names are returned (default=FALSE).
#' @return Returns a data frame that describes variables that matched the search terms. If namesonly=TRUE,
#' then a character vector of table names that contain matched variables is returned. 
#' @details nhanesSearch is useful to obtain a comprehensive list of relevant tables.
#' Search terms will be matched against the variable descriptions in the NHANES Comprehensive
#' Variable Lists.
#' Matching variables must have at least one of the search_terms and not have any exclude_terms.
#' The search may be restricted to specific surveys using ystart and ystop.
#' If no arguments are given, then nhanesSearch returns the complete variable list.
#' @examples
#'  \donttest{bladder = nhanesSearch("bladder", ystart=2001, ystop=2008, nchar=50)}
#'  \donttest{dim(bladder)}
#'  \donttest{urin = nhanesSearch("urin", exclude_terms="During", ystart=2009)}
#'  \donttest{dim(urin)}
#'  \donttest{urine = nhanesSearch(c("urine", "urinary"), ignore.case=TRUE, ystop=2006, namesonly=TRUE)}
#'  \donttest{length(urine)}
#' @export
#' 
nhanesSearch <- function(search_terms = NULL, exclude_terms = NULL,
                         data_group = NULL, ignore.case = FALSE, 
                         ystart = NULL, ystop = NULL, includerdc = FALSE,
                         nchar = 128, namesonly = FALSE)
{
  
  if(is.null(search_terms)) {
    stop("Search term is missing")
  }

  if(.useDB()){
    return(.nhanesSearchDB(search_terms, exclude_terms, data_group,
                           ignore.case, ystart, ystop, includerdc, nchar,
                           namesonly))
  }

  
  # Need to loop over url's
  
  df_initialized = FALSE
  for(i in 1:length(varURLs)) {  
    #    vlhtml <- read_html(varURLs[i])
    vlhtml <- .checkHtml(varURLs[i])
    
    if(!is.null(vlhtml)) {
      xpathh <- '//*[@id="GridView1"]/thead/tr'
      hnodes <- html_elements(vlhtml, xpath=xpathh)
      vmcols <- sapply(xml_children(hnodes),xml_text)
      vmcols <- unlist(lapply(str_split(vmcols, " "), paste0, collapse='.'))
      
      xpathv <- '//*[@id="GridView1"]/tbody/tr'
      vnodes <- html_elements(vlhtml, xpath=xpathv)
      if(length(vnodes) > 0){
        if(!df_initialized) {
          df <- t(sapply(lapply(vnodes,xml_children),xml_text)) |> as.data.frame()
          df_initialized = TRUE
        } else {
          dfadd <- t(sapply(lapply(vnodes,xml_children),xml_text)) |> as.data.frame()
          df <- rbind(df, dfadd)
        }
      }
    }
  }
  
  if(!df_initialized) { # There was no successful match
    message("Empty result set")
    return(NULL)
  }
  names(df) <- vmcols
  
  # Remove rdc tables if desired
  if(includerdc == FALSE) {
    df <- subset(df, Use.Constraints != "RDC Only")
  }
  
  if(!is.null(search_terms)) {
    pattern <- paste(search_terms, collapse = "|")
    df <- subset(df, grepl(pattern, Variable.Description, ignore.case = ignore.case))
    if(nrow(df) == 0) {
      message("No matches found")
      return(NULL)
    }
  }
  
  if(!is.null(data_group)) { # Restrict search to specific data group(s) e.g. 'EXAM' or 'LAB'
    sgroups <- unique(nhanes_group[data_group])
    sgroups <- sgroups[!is.na(sgroups)]
    if(length(sgroups) > 0) {
      pattern <- paste(sgroups, collapse = "|")
      df <- subset(df, grepl(pattern, Component, ignore.case = TRUE))
    }
  }
  
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
        if(.is.even(ystart)) {
          df <- subset(df, as.numeric(EndYear) >= ystart)
        } else {
          df <- subset(df, as.numeric(Begin.Year) >= ystart)
        }
        if(.is.even(ystop)) {
          df <- subset(df, as.numeric(EndYear) <= ystop)
        } else {
          df <- subset(df, as.numeric(Begin.Year) <= ystop)
        }
      }
    } else { # No ystart, assume it is 1999 (i.e. the first survey)
      if(.is.even(ystop)) {
        df <- subset(df, as.numeric(EndYear) <= ystop)
      } else {
        df <- subset(df, as.numeric(Begin.Year) <= ystop)
      }
    }
  } else if(!is.null(ystart)) { # ystart only, i.e. no ystop
    if(!is.numeric(ystart)) {stop("Start year (ystart) must be a 4-digit year")}
    if(.is.even(ystart)) {
      df <- subset(df, as.numeric(EndYear) >= ystart)
    } else {
      df <- subset(df, as.numeric(Begin.Year) >= ystart)
    }
  }
  
  if(!is.null(exclude_terms)) {
    pattern <- paste(exclude_terms, collapse = "|")
    df <- subset(df, !grepl(pattern, Variable.Description, ignore.case = ignore.case))
  }
  row.names(df) <- NULL
  if(namesonly) {
    return(sort(unique(df$Data.File.Name)))
  }
  df$Variable.Description <- substring(df$Variable.Description, 1, nchar)
  return(df)
}
#------------------------------------------------------------------------------
#' Search for matching table names
#' 
#' Returns a list of table names that match a specified pattern.
#' 
#' @importFrom rvest html_table html_nodes html_attr
#' @importFrom xml2 read_html
#' @param pattern Pattern of table names to match  
#' @param ystart Four digit year of first survey included in search, where ystart >= 1999.
#' @param ystop  Four digit year of final survey included in search, where ystop >= ystart.
#' @param includerdc If TRUE then RDC only tables are included (default=FALSE).
#' @param includewithdrawn IF TRUE then withdrawn tables are included (default=FALSE).
#' @param nchar Truncates the variable description to a max length of nchar.
#' @param details If TRUE then complete table information from the comprehensive
#' data list is returned (default=FALSE).
#' @return Returns a character vector of table names that match the given pattern. If details=TRUE,
#' then a data frame of table attributes is returned. NULL is returned when an
#' HTML read error is encountered.
#' @details Searches the Doc File field in the NHANES Comprehensive Data List 
#' (see https://wwwn.cdc.gov/nchs/nhanes/search/DataPage.aspx) for tables
#' that match a given name pattern. Only a single pattern may be entered.
#' @examples
#' \donttest{bmx = nhanesSearchTableNames('BMX')}
#' \donttest{length(bmx)}
#' \donttest{hepbd = nhanesSearchTableNames('HEPBD')}
#' \donttest{dim(hepbd)}
#' \donttest{hpvs = nhanesSearchTableNames('HPVS', includerdc=TRUE, details=TRUE)}
#' \donttest{dim(hpvs)}
#' @export
#' 
nhanesSearchTableNames <- function(pattern=NULL, ystart=NULL, ystop=NULL, includerdc=FALSE, 
                                   includewithdrawn=FALSE, nchar=128, details=FALSE) {
  if(is.null(pattern)) {stop('No pattern was entered')}

  if(.useDB()){
    return(.nhanesSearchTableNamesDB(pattern, ystart, ystop, includerdc,
                                     includewithdrawn, nchar, details))
  }


  if(length(pattern)>1) {
    pattern <- pattern[1]
    warning("Multiple patterns entered. Only the first will be matched.")
  }
  
  hurl <- .checkHtml(dataURL)
  if(is.null(hurl)) {
    message("Error occurred during read. No table names returned")
    return(NULL)
  }
  df <- hurl |> html_elements(xpath=xpath) |> html_table() |> data.frame()
  df <- subset(df, grepl(paste(pattern,collapse="|"), Doc.File))
#  df <- subset(df, Doc.File %in% grep(paste(pattern,collapse="|"), Doc.File, value=TRUE))
  if(nrow(df)==0) {return(NULL)}
  if(includerdc) {
#    df <- subset(df, !(Data.File == 'RDC Only'))
    
    lurl <- .checkHtml(ladDataURL)
    if(is.null(lurl)) {
      message("Error occurred during read. No limited access tables returned")
      return(NULL)
    }
    ladf <- lurl |> html_elements(xpath=xpath) |> html_table() |> data.frame()
    ladf <- subset(ladf, Doc.File %in% grep(paste(pattern,collapse="|"), Doc.File, value=TRUE))
    ladf <- ladf[c(names(df))]
    df <- rbind.data.frame(df,ladf)
  }
  if(!includewithdrawn) {
    df <- subset(df, !(Date.Published == 'Withdrawn'))
  }

  if( !is.null(ystart) || !is.null(ystop) ) {
    # Use the first year of cycle (the odd year) for comparison
    year1 <- as.integer(matrix(unlist(strsplit(df$Years, '-')), ncol=2, byrow=TRUE)[,1])
    
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
        } else { #Determine if Start year is odd or even. If even then convert to odd year.
          if(.is.even(ystart)) {
            ystart <- ystart - 1
          }
          df <- subset(df, year1 >= ystart & year1 <= ystop)
        }
      } else { # No ystart, assume it is 1999 (i.e. the first survey)
        df <- subset(df, year1 <= ystop)
      }
    } else if(!is.null(ystart)) { # ystart only, i.e. no ystop
      if(!is.numeric(ystart)) {stop("Start year (ystart) must be a 4-digit year")}
      if(.is.even(ystart)) {
        ystart <- ystart - 1
      } 
      df <- subset(df, year1 >= ystart)
    }
  }
  
  if(nrow(df)==0) {return(NULL)}
  row.names(df) <- NULL
  if(isTRUE(details)) {
    df <- df[order(df$Doc.File), ]
    df$Data.File <- substring(df$Data.File, 1, nchar)
    return(df)
  } else {
    return(sort(unlist(strsplit(df$Doc.File, " Doc"))))
  }
}
#------------------------------------------------------------------------------
#' Search for tables that contain a specified variable.
#' 
#' Returns a list of table names that contain the variable
#' @importFrom rvest html_elements html_table
#' @importFrom xml2 xml_children xml_text read_html
#' @param varname Name of variable to match.
#' @param ystart Four digit year of first survey included in search, where ystart >= 1999.
#' @param ystop  Four digit year of final survey included in search, where ystop >= ystart.
#' @param includerdc If TRUE then RDC only tables are included in list (default=FALSE).
#' @param nchar Truncates the variable description to a max length of nchar.
#' @param namesonly If TRUE then only the table names are returned (default=TRUE).
#' @return By default, a character vector of table names that include the specified variable 
#' is returned. If namesonly=FALSE, then a data frame of table attributes is returned. 
#' @details The NHANES Comprehensive Variable List is scanned to find all data tables that
#' contain the given variable name. Only a single variable name may be entered, and only
#' exact matches will be found.
#' @examples 
#' \donttest{nhanesSearchVarName('BMXLEG')}
#' \donttest{nhanesSearchVarName('BMXHEAD', ystart=2003)}
#' @export
#'  
nhanesSearchVarName <- function(varname=NULL, ystart=NULL, ystop=NULL, includerdc=FALSE, nchar=128, namesonly=TRUE) {
  if(is.null(varname)) {stop('No varname was entered')}

  if(.useDB()){
    return(.nhanesSearchVarNameDB(varname, ystart, ystop, includerdc,nchar, namesonly))
  }

  if(length(varname)>1) {
    varname <- varname[1]
    warning("Multiple variable names entered. Only the first will be matched.")
  }
  
  #  xpt <- paste0('//*[@id="ContentPlaceHolder1_GridView1"]/*[td[1]="', varname, '"]')
  xpt <- paste0('//*[@id="GridView1"]/tbody/*[td[1]="', varname, '"]')
  df_initialized = FALSE
  
  for(i in 1:length(varURLs)) {
    
    hurl <- .checkHtml(varURLs[i])
    
    if(!is.null(hurl)) {
      tabletree <- hurl |> html_elements(xpath=xpt)    
      ttlist <- lapply(lapply(tabletree, xml_children), xml_text)
      # Convert the list to a data frame
      
      if(length(ttlist) > 0) { # Determine if there was a successful match
        if(!df_initialized) {
          df <- unique(data.frame(matrix(unlist(ttlist), nrow=length(ttlist), byrow=TRUE)))
          df_initialized = TRUE
        } else { # End up here if df is already initialized
          dfadd <- unique(data.frame(matrix(unlist(ttlist), nrow=length(ttlist), byrow=TRUE)))
          if(nrow(dfadd) > 0) {
            df <- rbind(df,dfadd)
          }
        }
      }
    }
  }
  
  if(!df_initialized) { # There was no successful match
    message("Empty result set")
    return(NULL)
  }
  
  names(df) <- c('Variable.Name', 'Variable.Description',
                 'Data.File.Name', 'Data.File.Description',
                 'Begin.Year', 'EndYear', 'Component',
                 'Use.Constraints')
  
  if(includerdc == FALSE){
    df <- subset(df, Use.Constraints != "RDC Only")
  }
  
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
        if(.is.even(ystart)) {
          df <- subset(df, EndYear >= ystart)
        } else {
          df <- subset(df, Begin.Year >= ystart)
        }
        if(.is.even(ystop)) {
          df <- subset(df, EndYear <= ystop)
        } else {
          df <- subset(df, Begin.Year <= ystop)
        }
      }
    } else { # No ystart, assume it is 1999 (i.e. the first survey)
      if(.is.even(ystop)) {
        df <- subset(df, EndYear <= ystop)
      } else {
        df <- subset(df, Begin.Year <= ystop)
      }
    }
  } else if(!is.null(ystart)) { # ystart only, i.e. no ystop
    if(!is.numeric(ystart)) {stop("Start year (ystart) must be a 4-digit year")}
    if(.is.even(ystart)) {
      df <- subset(df, EndYear >= ystart)
    } else {
      df <- subset(df, Begin.Year >= ystart)
    }
  }
  
  row.names(df) <- NULL
  if(nrow(df)==0) { return(NULL) }
  if(namesonly) {
    return( sort(unique(df$Data.File.Name)) )
  }
  df <- df[order(df$Data.File.Name), ]
  df$Variable.Description <- substring(df$Variable.Description, 1, nchar)
  return(df)
}

