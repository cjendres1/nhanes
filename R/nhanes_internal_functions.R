# FUNCTIONS
#  .get_year_from_nh_table
#  .get_nh_survey_years
#  .is.even
#  .checkHtml
#
#------------------------------------------------------------------------------
# An internal function that determines which survey year the table belongs to.
# For most tables the year is indicated by the letter suffix following an underscore.
# E.g. for table 'BPX_E', the suffix is '_E'
# If there is no suffix, then we are likely dealing with data from 1999-2000.

.get_year_from_nh_table <- function(nh_table) {
  if(nh_table %in% anomalytables2005) { return('2005-2006') }
  if(startsWith(nh_table, "P_")) { return('2017-2018') } # Pre-pandemic
  if(startsWith(nh_table, "Y_")) { return('Nnyfs') } # Youth survey
  ## exceptions: PFC_POOL and SSNH4THY are cycle 2 but don't have _B suffix
  if(nh_table %in% c("PFC_POOL", "SSNH4THY")) { return('2001-2002') }
  nhloc <- data.frame(stringr::str_locate_all(nh_table, '_'))
  nn <- nrow(nhloc)
  if(nn!=0){ #Underscores were found
    if((nhloc$start[nn]+1) == nchar(nh_table)) {
      idx <- stringr::str_sub(nh_table, -1, -1)
      if(idx == 'r' || idx == 'R') {
        if(nn > 1) {
          newloc <- nhloc$start[nn-1]+1
          idx <- stringr::str_sub(nh_table, newloc, newloc)
        } else {stop('Invalid table name')}
      }
      return(data_idx[idx])
    } else { ## Underscore not 2nd to last. Assume table is from the first set.
      return("1999-2000")}
  } else { #If there are no underscores then table must be from first survey
    return("1999-2000")
  }
  ## FIXME check: may be simpler to just check endsWith("_A"), endsWith("_B"), etc
}

#------------------------------------------------------------------------------
# An internal function that converts a year into the nhanes interval.
# E.g. 2003 is converted to '2003-2004'
# @param year where year is numeric in yyyy format
# @return The 2-year interval that includes the year, e.g. 2001-2002
# 
.get_nh_survey_years <- function(year) {
  tryCatch(nh_years[[ as.character(year) ]],
           error = function(e) stop('Data for year ', year, ' are not available'))
}

# Internal function to determine if a number is even
.is.even <- function(x) {x %% 2 == 0}

# Internal function to test for successful html read
# FUNCTION checkHtml
# If read_html is successful, then the html is returned.
# Otherwise return NULL for proper error handling
# And NA is returned if the CDC handled the page not found error
.checkHtml <- function(url) {
  out <- tryCatch(
    {
      # when "try" is successful, 'tryCatch()' returns the html 
      if (isTRUE(nhanesOptions("log.access"))) message("Downloading: ", url)
      xml2::read_html(url)
    },
    error=function(cond) {
      # If there is an error, determine if it's a timeout error or URL error 
      ccond <- as.character(cond)
      if(length(grep('imeout',ccond)) > 0) {
        message("Timeout was reached: No data pulled", "\n")
      } else if(length(grep('Could not resolve', ccond))>0) {
        message(cond)
        #message("Could not resolve host", "\n")
      } else { message(paste(c("URL ", url, " does not seem to exist"), collapse='')) }   
      
      # Return NULL
      return(NULL)
    },
    warning=function(cond) {
      message(cond)
      return(NULL)
    }
  )
  ##check to see if they handle the page not found issue
  if( !is.null(out)) {
    pageNotFound = rvest::html_element(out, xpath="//meta[@content='Page Not Found']")
    if (!is.na(pageNotFound)) out = NULL
  }
  
  return(out)
}



