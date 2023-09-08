# FUNCTION nhanesCodebook
#
#------------------------------------------------------------------------------
#' Display codebook for selected variable.
#' 
#' Returns full NHANES codebook including Variable Name, SAS Label, English Text, Target,
#' and Value distribution.
#' 
#' @importFrom stringr str_c str_sub str_remove_all str_trim
#' @importFrom rvest html_elements html_table html_text2
#'  
#' @param nh_table The name of the NHANES table that contains the desired variable.
#' @param colname The name of the table column (variable).
#' @param dxa If TRUE then the 2005-2006 DXA codebook will be used (default=FALSE).
#' @details Each NHANES variable has a codebook that provides a basic description
#' as well as the distribution or range of values. This function returns the full
#' codebook information for the selected variable.
#' @return The codebook is returned as a list object. Returns NULL upon error.
#' @examples
#' \donttest{nhanesCodebook('AUX_D', 'AUQ020D')}
#' \donttest{nhanesCodebook('BPX_J', 'BPACSZ')}
#' @export
#'
nhanesCodebook <- function(nh_table, colname, dxa=FALSE) {
  if(is.null(colname)) {
    message('Column name is required')
    return(0)
  }

  if(dxa==FALSE & !is.na(.collection_date) & !is.na(.container_version)){
    return(.nhanesCodebookDB(nh_table, colname))
  }
  
  if(dxa) {
    url <- "https://wwwn.cdc.gov/nchs/data/nhanes/dxa/dxx_d.htm"
  } else {  nh_year <- .get_year_from_nh_table(nh_table)
  if(is.null(nh_year)) {
    return(NULL)
  }
  if(nh_year == "Nnyfs"){
    url <- str_c("https://wwwn.cdc.gov/Nchs/", nh_year, '/', nh_table, '.htm', sep='')
  } else {
    url <- str_c(nhanesURL, nh_year, '/', nh_table, '.htm', sep='')
  }
  }
  
  xpt <- str_c('//*[h3[a[@name="', colname, '"]]]', sep='')
  
  hurl <- .checkHtml(url)
  if(is.null(hurl)) {
    tabletree <- NULL
  } else {
    tabletree <- hurl %>% html_elements(xpath=xpt)
  }
  #    tabletree <- url %>% read_html() %>% html_elements(xpath=xpt)
  if(length(tabletree)==0) { # If not found then try 'id' instead of 'name'
    xpt <- str_c('//*[h3[@id="', colname, '"]]', sep='')
    
    hurl <- .checkHtml(url)
    if(is.null(hurl)) {
      tabletree <- NULL
    } else {
      tabletree <- hurl %>% html_elements(xpath=xpt)
    }
    #      tabletree <- url %>% read_html() %>% html_elements(xpath=xpt)
  }
  if(length(tabletree)>0) {
    codetitles <- html_elements(tabletree, "dt") %>% html_text2()
    codetext <- html_elements(tabletree, "dd") %>% html_text2()
    names(codetext) <- codetitles
    tabletrans <- html_elements(tabletree, 'table') %>% html_table()
    if(length(tabletrans) > 0) {
      names(tabletrans) <- colname
      codebook <- c(codetext, tabletrans)
    } else {
      codebook <- codetext
    }
    return(codebook)
  } else { # Code table not found so let's see if last letter should be lowercase
    nc <- nchar(colname)
    if(length(grep("[[:upper:]]", stringr::str_sub(colname, start=nc, end=nc)))>0){
      lcnm <- colname
      stringr::str_sub(lcnm, start=nc, end=nc) <- tolower(stringr::str_sub(lcnm, start=nc, end=nc))
      xpt <- str_c('//*[h3[a[@name="', lcnm, '"]]]', sep='')
      
      hurl <- .checkHtml(url)
      if(is.null(hurl)) {
        tabletree <- NULL
      } else {
        tabletree <- hurl %>% html_elements(xpath=xpt)
      }
      #        tabletree <- url %>% read_html() %>% html_elements(xpath=xpt)
      if(length(tabletree)==0) { # If not found then try 'id' instead of 'name'
        xpt <- str_c('//*[h3[@id="', lcnm, '"]]', sep='')
        
        hurl <- .checkHtml(url)
        if(is.null(hurl)) {
          tabletree <- NULL
        } else {
          tabletree <- hurl %>% html_elements(xpath=xpt)
        }
        #          tabletree <- url %>% read_html() %>% html_elements(xpath=xpt)
      }
      
      if(length(tabletree)>0) {
        codetitles <- html_elements(tabletree, "dt") %>% html_text2()
        codetext <- html_elements(tabletree, "dd") %>% html_text2()
        names(codetext) <- codetitles
        tabletrans <- html_elements(tabletree, 'table') %>% html_table()
        if(length(tabletrans) > 0) {
          names(tabletrans) <- colname
          codebook <- c(codetext, tabletrans)
        } else {
          codebook <- codetext
        }
        return(codebook)
      } else { # Still not found even after converting to lowercase
        warning(c('Column "', colname, '" not found'), collapse='')
        return(NULL)
      }
    } else { #Last character is not an uppercase letter, thus can't convert to lowercase
      warning(c('Column "', colname, '" not found'), collapse='')
      return(NULL)
    }
  }
}

