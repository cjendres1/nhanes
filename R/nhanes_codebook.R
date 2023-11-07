# FUNCTION nhanesCodebook
#
#------------------------------------------------------------------------------
#' Display codebook for selected variable.
#' 
#' Returns full NHANES codebook including Variable Name, SAS Label, English Text, Target,
#' and Value distribution.
#' 
#' @importFrom stringr str_remove_all str_trim
#' @importFrom rvest html_elements html_table html_text2
#'  
#' @param nh_table The name of the NHANES table that contains the desired variable.
#' @param colname The name of the table column (variable).
#' @param dxa If TRUE then the 2005-2006 DXA codebook will be used (default=FALSE).
#' @details Each NHANES variable has a codebook that provides a basic
#'   description as well as the distribution or range of values. This
#'   function returns the full codebook information for the selected
#'   variable. If the environment variable \code{NHANES_TABLE_BASE}
#'   was set during startup, the value of this variable is used as the
#'   base URL instead of \url{https://wwwn.cdc.gov} (this allows the
#'   use of a local or alternative mirror of the CDC documentation).
#' @return The codebook is returned as a list object. Returns NULL upon error.
#' @examples
#' \donttest{nhanesCodebook('AUX_D', 'AUQ020D')}
#' \donttest{nhanesCodebook('BPX_J', 'BPACSZ')}
#' \donttest{bpx_code = nhanesCodebook('BPX_J')}
#' \donttest{length(bpx_code)}
#' @export
#'
nhanesCodebook <- function(nh_table, colname=NULL, dxa=FALSE) {
## drop this after testing
##  if(is.null(colname)) 
##    colname = nhanesAttr(nh_table)$names

  if(isFALSE(dxa) && !grepl("^(P_|Y_)\\w+", nh_table) && .useDB()) {
    return(.nhanesCodebookDB(nh_table, colname))
  }
  
  if(dxa) {
    url <- "https://wwwn.cdc.gov/nchs/data/nhanes/dxa/dxx_d.htm"
  } else {
    nh_year <- .get_year_from_nh_table(nh_table)
    if(anyNA(nh_year)) {
      return(NULL)
    }
    if(nh_year == "Nnyfs"){
      url <- paste0("https://wwwn.cdc.gov/Nchs/", nh_year, '/', nh_table, '.htm')
    } else {
      url <- paste0(nhanesTableURL, nh_year, '/', nh_table, '.htm')
    }
  }
  hurl <- .checkHtml(url)
  ##will be NA if the CDC handled a page not found error - then try restricted
  if( is.null(hurl) ||  is.na(hurl)) {
    url = paste0("https://wwwn.cdc.gov/Nchs/Nhanes/limited_access/", nh_table, ".htm")
    hurl = .checkHtml(url)
    if(is.null(hurl) || is.na(hurl)) stop(paste0("could not find a web page for ", nh_table))
  }
  if(is.null(colname) )
    colname = .getVarNames(hurl)$VarNames

  ans = vector("list", length=length(colname))
  names(ans)=colname

  for(i in seq(along=colname)) 
    ans[[i]] = .codeBookHelper(colname[i], hurl)
    
  if ( length(ans)==1 ) 
    return(ans[[1]])
  else 
    return(ans)
} 


##' Download and parse an NHANES doc file
##'
##' Downloads and parses an NHANES doc file from a URL and returns it as a list
##' @title Parse NHANES doc URL
##' @param url URL to be downloaded
##' @return list with one element for each variable
##' @export
nhanesParseCodeBook <- function(url) {
  if (length(url) != 1) stop("'url' must have length 1")
  if (startsWith(tolower(url), "/nchs/nhanes"))
    url <- paste0(nhanesManifestPrefix, url)
  hurl <- .checkHtml(url)
  if (is.null(hurl) ||  is.na(hurl)) {
    stop(paste0("could not find a web page at: ", url))
  }
  colname = .getVarNames(hurl)$VarNames
  lapply(colname, .codeBookHelper, hurl)
} 



##helper function to get the variable names from the HTML and not via nhanesAttr
.getVarNames = function(doc) {
    xx = xml2::xml_find_all(doc, "//*[a]")
    loc = grep("Codebook", xx)
    if(length(loc) > 0 ) {
      ##get the subdoc and then all li elements in it
      d2 = xx[[loc]]
      li = rvest::html_elements(d2, "li")
      gg = xml2::xml_text(li, trim=TRUE)
    } else { ##look elsewhere for Codebook info
       x2 = xml2::xml_find_all(doc, "//div[@id='Codebook']")
       x3 = xml2::xml_find_all(x2, "//h3[a[@name]]")
       ## VarNames = unlist(xml2::xml_attrs(xml2::xml_children(x3), "name"))
       gg = xml2::xml_text(x3, trim=TRUE)
   }
   firstDash = sapply(gg, function(x) unlist(gregexpr('-', x, fixed=T))[1])
   VarNames = substr(gg, 1, firstDash-1)
   VarNames = trimws(VarNames)
   VarDesc = substr(gg, firstDash+1, nchar(gg))
   VarDesc = trimws(VarDesc)
   return(list(VarNames=VarNames, VarDesc=VarDesc))
 }

  
##helper function that gives a couple of locations in the document to search
.testLocations = function(colname, hurl) {
  xpt <- paste0('//*[h3[a[@name="', colname, '"]]]')
  tabletree <- hurl |> html_elements(xpath=xpt)

  if(length(tabletree)==0) { # If not found then try 'id' instead of 'name'
    xpt <- paste0('//*[h3[@id="', colname, '"]]')
    tabletree <- hurl |> html_elements(xpath=xpt)
  }
  return(tabletree)
}

.codeBookHelper = function(colname, hurl) {
  
  ##no html/document to search
  if( is.null(hurl) ) return(NULL)

  tabletree = .testLocations(colname, hurl)
  ##CDC sometimes is inconsistent in capitalizing the last two alpha characters
  ## or the last one...so we will check and if upper case - look for lower case
  if( length(tabletree) == 0 ) {
   spCN = strsplit(colname, split="")
   upperC = spCN[[1]] %in% LETTERS
   nc = length(upperC)
   if( upperC[nc] ) {
     lcnm = colname
     substr(lcnm, nc,nc) = tolower(substr(lcnm, nc,nc))
     tabletree = .testLocations(lcnm, hurl)
     ##if second to last character is upper case lower case it and test again
     ##and we didn't find a match
     if( upperC[nc-1] && (length(tabletree)==0) ){
       substr(lcnm, nc-1, nc-1) = tolower(substr(lcnm, nc-1, nc-1))
       tabletree = .testLocations(lcnm, hurl)
     }
     ##and of course one more weird one ...second to last lower, last upper
     ## lcnm has the last two characters as letters and they are both lower case
     if(length(tabletree)==0 && upperC[nc-1]) {
     substr(lcnm, nc, nc) = toupper(substr(lcnm, nc,nc))
     tabletree = .testLocations(lcnm, hurl)
     }
   }
  }
  if(length(tabletree)>0) {
    codetitles <- html_elements(tabletree, "dt") |> html_text2()
    codetext <- html_elements(tabletree, "dd") |> html_text2()
    names(codetext) <- codetitles
    tabletrans <- html_elements(tabletree, 'table') |> html_table()
    if(length(tabletrans) > 0) {
      names(tabletrans) <- colname
      codebook <- c(codetext, tabletrans)
    } else {
      codebook <- codetext
    }
    return(as.list(codebook))
  } else { 
     warning(c('Column "', colname, '" not found'), collapse='')
     return(NULL)
  }
}
