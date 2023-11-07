# FUNCTION nhanesTranslate
#
#------------------------------------------------------------------------------
#' Display code translation information.
#' 
#' Returns code translations for categorical variables, 
#' which appear in most NHANES tables.
#' 
#' @importFrom stringr str_locate str_sub 
#' @importFrom rvest html_elements html_table
#' @importFrom xml2 read_html
#' @importFrom plyr mapvalues
#' @param nh_table The name of the NHANES table to retrieve.
#' @param colnames The names of the columns to translate. It will
#'   translate all the columns by default.
#' @param data If a data frame is passed, then code translation will
#'   be applied directly to the data frame. \cr In that case the
#'   return argument is the code-translated data frame.
#' @param nchar Applies only when data is defined. Code translations
#'   can be very long. \cr Truncate the length by setting nchar
#'   (default = 128).
#' @param mincategories The minimum number of categories needed for
#'   code translations to be applied to the data (default=2).
#' @param details If TRUE then all available table translation
#'   information is displayed (default=FALSE).
#' @param dxa If TRUE then the 2005-2006 DXA translation table will be
#'   used (default=FALSE).
#'
#' @return The code translation table (or translated data frame when
#'   data is defined). Returns NULL upon error.
#' @details Most NHANES data tables have encoded values. E.g. 1 =
#'   'Male', 2 = 'Female'.  Thus it is often helpful to view the code
#'   translations and perhaps insert the translated values in a data
#'   frame. Only a single table may be specified, but multiple
#'   variables within that table can be selected. Code translations
#'   are retrieved for each variable. If the environment variable
#'   \code{NHANES_TABLE_BASE} was set during startup, the value of
#'   this variable is used as the base URL instead of
#'   \url{https://wwwn.cdc.gov} (this allows the use of a local or
#'   alternative mirror of the CDC documentation).
#' @examples
#' \donttest{nhanesTranslate('DEMO_B', c('DMDBORN','DMDCITZN'))}
#' \donttest{nhanesTranslate('BPX_F', 'BPACSZ', details=TRUE)}
#' \donttest{nhanesTranslate('BPX_F', 'BPACSZ', data=nhanes('BPX_F'))}
#' \donttest{trans_demo = nhanesTranslate('DEMO_B')}
#' \donttest{length(trans_demo)}
#' @export
#' 
nhanesTranslate <- function(nh_table, colnames=NULL, data = NULL, nchar = 128, 
                            mincategories = 2, details=FALSE, dxa=FALSE) {

  if(isFALSE(dxa) && !grepl("^(P_|Y_)\\w+", nh_table) && .useDB()) {
    return(.nhanesTranslateDB(nh_table, colnames,data,nchar,mincategories,details))
  }

  # if(is.null(colnames)) {
  #   message('Column name is required')
  #   return(NULL)
  # }
  
  if(!is.null(data) & details == TRUE) {
    details = FALSE
    warning("When a data table is passed to nhanesTranslate, the details variable is ignored")
  }
  
  get_translation_table <- function(colname, hurl, details) {
    xpt <- paste0('//*[h3[a[@name="', colname, '"]]]')
    
    tabletree <- hurl |> html_elements(xpath=xpt)
    if(length(tabletree)==0) { # If not found then try 'id' instead of 'name'
      xpt <- paste0('//*[h3[@id="', colname, '"]]')
      
      tabletree <- hurl |> html_elements(xpath=xpt)
    }
    if(length(tabletree)>0) {
      tabletrans <- html_elements(tabletree, 'table') |> html_table() |> as.data.frame()
    } else { # Code table not found so let's see if last letter should be lowercase
      nc <- nchar(colname)
      if(length(grep("[[:upper:]]", stringr::str_sub(colname, start=nc, end=nc)))>0){
        lcnm <- colname
        stringr::str_sub(lcnm, start=nc, end=nc) <-
            tolower(stringr::str_sub(lcnm, start=nc, end=nc))
        xpt <- paste0('//*[h3[a[@name="', lcnm, '"]]]')
        
        tabletree <- hurl |> html_elements(xpath=xpt)

        if(length(tabletree)==0) { # If not found then try 'id' instead of 'name'
          xpt <- paste0('//*[h3[@id="', lcnm, '"]]')
          tabletree <- hurl |> html_elements(xpath=xpt)
        }
        
        if(length(tabletree)>0) {
          tabletrans <- html_elements(tabletree, 'table') |> html_table() |> as.data.frame()
        } else { # Still not found even after converting to lowercase
          warning(c('Column "', colname, '" not found'), collapse='')
          return(NULL)
        }
      } else { #Last character is not an uppercase letter, thus can't convert to lowercase
        warning(c('Column "', colname, '" not found'), collapse='')
        return(NULL)
      }
    }
    
    if(length(tabletrans) > 0) {
      if(details == FALSE) {
        tabletrans <- tabletrans[,c('Code.or.Value', 'Value.Description')]
      }
      return(tabletrans)
    } else { 
      if(!(colname=='SEQN')) {
        message(paste(c('No translation table is available for ', colname), collapse=''))
      }
      return(NULL)
    }
  }
  
  if(dxa) {
    code_translation_url <- "https://wwwn.cdc.gov/nchs/data/nhanes/dxa/dxx_d.htm"
  } else {
    nh_year <- .get_year_from_nh_table(nh_table)
    if(anyNA(nh_year)) {
      return(NULL)
    }
    code_translation_url <- 
      if(nh_year == "Nnyfs"){
        paste0("https://wwwn.cdc.gov/Nchs/", nh_year, '/', nh_table, '.htm')
      } else {
        paste0(nhanesTableURL, nh_year, '/', nh_table, '.htm')
      }
  }
  hurl <- .checkHtml(code_translation_url)
  if(is.null(colnames) )
    colnames = .getVarNames(hurl)$VarNames
  translations <- lapply(colnames, get_translation_table, hurl, details)
  names(translations) <- colnames
  
  #nchar_max <- 128
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
        sstr <- paste0('^', cname, '$') # Construct the search string
        idx <- grep(sstr, names(data)) 
        if(length(idx)>0) { ## The column is present. Next we need to decide if it should be translated.
          if(length(levels(as.factor(data[[idx]]))) >= mincategories) {
               # If we reached this point then yes we are translating
               # Check for SAS label attribute
            idx_label <- attr(data[[idx]],"label")
            data[[idx]] <- as.factor(data[[idx]])
              data[[idx]] <-
                  suppressMessages(
                      plyr::mapvalues(data[[idx]],
                                      from = translations[[cname]][['Code.or.Value']], 
                                      to = str_sub(translations[[cname]][['Value.Description']], 1, nchar)))
            if(!is.null(idx_label)) {
              attr(data[[idx]],"label") <- idx_label
              }
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



## alternative translate interface: given input (raw) data and input
## codebook. The intent is for this to work with data either from the
## NHANES website or from the DB


raw2translated <- function(df, codebook)
{
    cb_info <- nhanesAttr_codebook(src = codebook)


}


checkAmbiguous <- function(nh_table, force = FALSE)
{
    if (!force && !isTRUE(nhanesOptions("use.db")))
        stop("DB not available. Use 'force = TRUE' to download source files")
    ## data <- nhanes(nh_table)
    cb <- nhanesCodebook(nh_table)
    cb_info <- nhanesAttr_codebook(src = cb)
    ambiguous <- with(cb_info, num & nlevels != 2)
    if (any(ambiguous, na.rm = TRUE)) {
        return(lapply(cb[which(ambiguous)], function(x) x[[length(x)]]))
    }
    invisible()
}





