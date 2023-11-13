#nhanesA - retrieve data from the CDC NHANES repository
# Christopher J. Endres 09/14/2023
# FUNCTIONS:
#   nhanes
#   nhanesFromURL
#   nhanesDXA
#   nhanesAttr
#   nhanesTableSummary
#   browseNHANES

#------------------------------------------------------------------------------
#' Download an NHANES table and return as a data frame.
#' 
#' Use to download NHANES data tables that are in SAS format.
#' 
#' @importFrom foreign read.xport lookup.xport
#' @param nh_table The name of the specific table to retrieve.
#' @param includelabels If TRUE, then include SAS labels as variable
#'   attribute (default = FALSE).
#' @param translated translated whether the variables are translated.
#' @param nchar Maximum length of translated string (default =
#'   128). Ignored if translated=FALSE.
#' @return The table is returned as a data frame.
#' @details Downloads a table from the NHANES website as is, i.e. in
#'   its entirety with no modification or cleansing. If the
#'   environment variable \code{NHANES_TABLE_BASE} was set during
#'   startup, the value of this variable is used as the base URL
#'   instead of \url{https://wwwn.cdc.gov} (this allows the use of a
#'   local or alternative mirror of the CDC data). NHANES tables are
#'   stored in SAS '.XPT' format but are imported as a data frame.
#'   The \code{nhanes} function cannot be used to import limited
#'   access data.
#' @examples 
#' \donttest{bpx_e = nhanes('BPX_E')}
#' \donttest{dim(bpx_e)}
#' \donttest{folate_f = nhanes('FOLATE_F', includelabels = TRUE)}
#' \donttest{dim(folate_f)}
#' @export
#' 
nhanes <- function(nh_table, includelabels = FALSE, translated = TRUE, nchar = 128) {

  if(!grepl("^(Y_)\\w+", nh_table) && .useDB()){
    return(.nhanesDB(nh_table, includelabels, translated))
  }

  nht <- tryCatch({    
    nh_year <- .get_year_from_nh_table(nh_table)
    
    if(startsWith(nh_table, "Y_")) {
      url <- paste0('https://wwwn.cdc.gov/Nchs/', nh_year, '/', nh_table, '.XPT')
    } else {
      url <- paste0(nhanesTableURL, nh_year, '/', nh_table, '.XPT')
    }
    
    tf <- tempfile()
    if (isTRUE(nhanesOptions("log.access"))) message("Downloading: ", url)
    download.file(url, tf, mode = "wb", quiet = TRUE)
    
    nh_df <- read.xport(tf)

    if(translated){
      # suppress warning because there will be a warning and the function returns NULL when no columns need to translated.
      suppressWarnings(suppressMessages({nh_df = 
        nhanesTranslate(nh_table,colnames = colnames(nh_df)[2:ncol(nh_df)],data = nh_df, nchar=nchar)}))
    }
    
    if(includelabels) {
      xport_struct <- lookup.xport(tf)
      column_names  <- xport_struct[[1]]$name
      column_labels <- xport_struct[[1]]$label
      names(column_labels) <- column_names
      
      # Ideal case where rows and labels are identical
      if(identical(names(nh_df), column_names)) {
        for( name in column_names) {
          attr(nh_df[[name]],"label") = column_labels[which(names(column_labels)==name)]
        }
      } else {
        message(paste0("Column names and labels are not consistent for table ", nh_table, ". No labels added"))
      }
  
    } 
    return(nh_df)
    
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


##' Download an NHANES table from URL
##'
##' Downloads an NHANES table from a URL and returns it as a data frame
##' @title Parse NHANES doc URL
##' @importFrom tools file_path_sans_ext
##' @param url URL of XPT file to be downloaded
##' @param translated logical, whether variable codes should be translated
##' @param nchar integer, labels are truncated after this
##' @return data frame
##' @export
nhanesFromURL <- function(url, translated = TRUE, nchar = 128)
{
  if (length(url) != 1) stop("'url' must have length 1")
  if (startsWith(tolower(url), "/nchs/nhanes"))
    url <- paste0(nhanesManifestPrefix, url)
  tryCatch({    
    tf <- tempfile()
    if (isTRUE(nhanesOptions("log.access"))) message("Downloading: ", url)
    download.file(url, tf, mode = "wb", quiet = TRUE)
    nh_df <- read.xport(tf)
    if (translated) {
      ## guess table name
      nh_table <- file_path_sans_ext(basename(url))
      ## suppress warning because there will be a warning and the
      ## function returns NULL when no columns need to translated.
      suppressWarnings(suppressMessages({
        nh_df <- nhanesTranslate(nh_table,
                                 colnames = colnames(nh_df)[2:ncol(nh_df)],
                                 data = nh_df,
                                 nchar=nchar)
      }))
    }
    nh_df
  },
  error = function(cond) {
    stop(paste0("could not find a XPT file at: ", url))
  },
  warning = function(cond) {
    message(cond, '\n')
  })
} 




## #------------------------------------------------------------------------------
#' Import Dual Energy X-ray Absorptiometry (DXA) data.
#' 
#' DXA data were acquired from 1999-2006. 
#' 
#' @importFrom foreign read.xport
#' @importFrom utils download.file
#' @param year The year of the data to import, where 1999<=year<=2006. 
#' @param suppl If TRUE then retrieve the supplemental data (default=FALSE).
#' @param destfile The name of a destination file. If NULL then the data are imported 
#' into the R environment but no file is created.
#' @return By default the table is returned as a data frame. When downloading to file, the return argument
#' is the integer code from download.file where 0 means success and non-zero indicates failure to download.
#' @details  Provide destfile in order to write the data to file. If destfile is not provided then
#' the data will be imported into the R environment.
#' @examples
#' \donttest{dxa_b <- nhanesDXA(2001)}
#' \donttest{dxa_c_s <- nhanesDXA(2003, suppl=TRUE)}
#' \dontrun{dxa = nhanesDXA(1999, destfile="dxx.xpt")}
#' @export
nhanesDXA <- function(year, suppl=FALSE, destfile=NULL) {

  dxa_fname <- function(year, suppl) {
    fname <- 
      if(year == 1999 || year == 2000) { 'dxx'}
      else if(year == 2001 || year == 2002) { 'dxx_b'}
      else if(year == 2003 || year == 2004) { 'dxx_c'}
      else if(year == 2005 || year == 2006) { 'DXX_D'}
    if(isTRUE(suppl)) {
      fname <- paste0(fname, 
                      if(year == 2005 || year == 2006) '_S' else '_s')
    }
    return(fname)
  }
  
  if(year) {
    if(!(as.character(year) %in% c('1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006'))) {
      stop("Invalid survey year for DXA data")
    } else {
      fname <- dxa_fname(year, suppl)
      url <- paste0(dxaURL, fname, '.xpt')
      if (isTRUE(nhanesOptions("log.access"))) message("Downloading: ", url)
      if(!is.null(destfile)) {
        ok <- suppressWarnings(tryCatch({download.file(url, destfile, mode="wb", quiet=TRUE)},
                                        error=function(cond){message(cond); return(NULL)}))         
        return(ok)
      } else {
        tf <- tempfile()
        ok <- suppressWarnings(tryCatch({download.file(url, tf, mode="wb", quiet=TRUE)},
                                        error=function(cond){message(cond); return(NULL)}))
        if(!is.null(ok)) {
          return(read.xport(tf))
        } else { return(NULL) }
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
#' @importFrom foreign read.xport
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
#' @details nhanesAttr allows one to check the size and other
#'   charactersistics of a data table before importing into R. To
#'   retrieve these characteristics, the specified table is
#'   downloaded, characteristics are determined, then the table is
#'   deleted. Downloads a table from the NHANES website as is, i.e. in
#'   its entirety with no modification or cleansing. If the
#'   environment variable \code{NHANES_TABLE_BASE} was set during
#'   startup, the value of this variable is used as the base URL
#'   instead of \url{https://wwwn.cdc.gov} (this allows the use of a
#'   local or alternative mirror of the CDC data).
#' @examples 
#' nhanesAttr('BPX_E')
#' nhanesAttr('FOLATE_F')
#' @export
#' 
nhanesAttr <- function(nh_table) {
  nht <- tryCatch({    

    nh_year <- .get_year_from_nh_table(nh_table)
    
    if(startsWith(nh_table, "Y_")) {
      url <- paste0('https://wwwn.cdc.gov/Nchs/', nh_year, '/', nh_table, '.XPT')
    } else {
      url <- paste0(nhanesTableURL, nh_year, '/', nh_table, '.XPT')
    }
    
    tf <- tempfile()
    if (isTRUE(nhanesOptions("log.access"))) message("Downloading: ", url)
    download.file(url, tf, mode = "wb", quiet = TRUE)
    
#    tmp <- read.xport(tf)
    xport_struct <- lookup.xport(tf)
    
    column_names  <- xport_struct[[1]]$name
    column_labels <- xport_struct[[1]]$label
    names(column_labels) <- column_names
    nh_df <- read.xport(tf)
    
    nhtatt <- attributes(nh_df)
    nhtatt$row.names <- NULL
    nhtatt$nrow <- nrow(nh_df)
    nhtatt$ncol <- ncol(nh_df)
    nhtatt$unique <- (length(unique(nh_df$SEQN)) == nhtatt$nrow)
    nhtatt$na <- sum(is.na(nh_df))
    nhtatt$size <- object.size(nh_df)
    nhtatt$types <- sapply(nh_df,class)
    
    # Ideal case where rows and labels are identical
    if(identical(names(nh_df), column_names)) {
      for( name in column_names) {
        attr(nh_df[[name]],"label") = column_labels[which(names(column_labels)==name)]
      }
      
      return_label <- function(var) {
        return(as.character(attr(var, "label")))
      }
      nhtatt$labels = sapply(nh_df, return_label)
      
    } else {
      message(paste0("Column names and labels are not consistent for table ", nh_table, ". No labels added"))
    }
    
    rm(nh_df)
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
#' Open a browser to NHANES.
#' 
#' The browser may be directed to a specific year, survey, or table.
#' 
#' @importFrom stringr str_split str_extract_all
#' @importFrom utils browseURL
#' @param year The year in yyyy format where 1999 <= yyyy.
#' @param data_group The type of survey (DEMOGRAPHICS, DIETARY,
#'   EXAMINATION, LABORATORY, QUESTIONNAIRE).  Abbreviated terms may
#'   also be used: (DEMO, DIET, EXAM, LAB, Q).
#' @param nh_table The name of an NHANES table.
#' @param local logical flag. If \code{TRUE}, and a local or
#'   alternative source was specificed using the environment variable
#'   \code{NHANES_TABLE_BASE}, this will be used in preference to the
#'   CDC website at \url{https://wwwn.cdc.gov} for named tables.
#' @param browse logical flag, indicating whether the specific NHANES
#'   site should be opened using a browser (which is the default
#'   behaviour).
#' @return A character string giving the URL, invisibly if the URL is
#'   also opened using \code{\link{browseURL}}.
#' @details By default, browseNHANES will open a web browser to the
#'   specified NHANES site.
#' @examples
#' browseNHANES(browse = FALSE)       # Defaults to the main data sets page
#' browseNHANES(2005)                 # The main page for the specified survey year
#' browseNHANES(2009, 'EXAM')         # Page for the specified year and survey group
#' browseNHANES(nh_table = 'VIX_D')   # Page for a specific table
#' browseNHANES(nh_table = 'DXA')     # DXA main page
#' @export
#' 
browseNHANES <- function(year = NULL, data_group = NULL, nh_table = NULL,
                         local = TRUE, browse = TRUE)
{
  handleURL <- function(url) {
    if (isFALSE(browse)) return(url)
    else {
      browseURL(url)
      return(invisible(url))
    }
  }
  if(!is.null(nh_table)){ # Specific table name was given
    if('DXA'==toupper(nh_table)) { # Special case for DXA
      handleURL("https://wwwn.cdc.gov/nchs/nhanes/dxa/dxa.aspx")
    } else {
      nh_year <- .get_year_from_nh_table(nh_table)
      url <- paste0(if (local) nhanesTableURL else nhanesURL,
                   nh_year, '/', nh_table, '.htm')
      handleURL(url)
    }
  } else if(!is.null(year)) {
    if(!is.null(data_group)) {
      nh_year <- .get_nh_survey_years(year)
      url <- paste0(nhanesURL, 'Search/DataPage.aspx?Component=', 
                    nhanes_group[data_group],
                    '&CycleBeginYear=', unlist(str_split(nh_year, '-'))[[1]])
      handleURL(url)
    } else { # Go to the two year survey page 
      nh_year <- .get_nh_survey_years(year)
#      nh_year <- str_c(str_sub(unlist(str_extract_all(nh_year,"[[:digit:]]{4}")),3,4),collapse='_')
      nh_year <- unlist(str_extract_all(nh_year, "[[:digit:]]{4}"))[1]
      url <- paste0(nhanesURL, 'continuousnhanes/default.aspx?BeginYear=', nh_year)
      handleURL(url)
    }
  } else {
    handleURL("https://wwwn.cdc.gov/nchs/nhanes/Default.aspx")
  }
}
#------------------------------------------------------------------------------



## Alternative approaches to get attributes of a NHANES table: either
## use the actual data, or use the codebook. Ideally these should
## match (to the extent that the codebook has the relevant
## information).

## The first one is similar to nhanesAttr() but does not use the XPT file directly

variableSummary_data <- function(x) {
  data.frame(nobs_data = length(x),
             na_data = sum(is.na(x)),
             size = object.size(x),
             num = is.numeric(x),
             cat = is.factor(x) || is.character(x),
             unique = !anyDuplicated(x))
}

nhanesSummary_data <- function(nh_table, src = nhanes(nh_table, ...), ...)
{
  ## start by getting data, delegating details to nhanes()
  df <- src
  var_info <- lapply(df, variableSummary_data)
  nhtatt <- Reduce(rbind, var_info)
  nhtatt <- cbind(varname = names(df),
                  nhtatt)
  rownames(nhtatt) <- NULL
  if (!missing(nh_table)) nhtatt <- cbind(table = nh_table, nhtatt)
  return(nhtatt)
}

## We can get much of the same information from the codebook, and it
## can be useful to check for inconsistencies. The codebook has one
## entry for each variable in it, and the following helper function
## summarizes it. It can be typically called on the entry for a
## variable within the list returned by nhanesCodebook(). E.g.,
## nhanesCodebook("FOLATE_F")$LBDFOL |> variableSummary()

variableSummary_codebook <- function(x) {
  varName <- x[["Variable Name:"]]
  sasLabel <- x[["SAS Label:"]]
  varInfo <- if (is.list(x)) x[[varName]] # may be NULL (e.g. for SEQN)
             else NULL
  varSummary <-
    if (!is.null(varInfo)) {  # consistency checks
      colnames(varInfo) <- make.names(colnames(varInfo))
      ## In a very few tables, the 'varInfo' table has duplicate rows
      if (anyDuplicated(varInfo)) {
        warning("Duplicate codebook rows in variable ", varName)
        varInfo <- varInfo[!duplicated(varInfo), ]
      }
      with(varInfo, # convert to 'fix' names
      {
        n1 <- sum(Count)
        n2 <- Cumulative[[length(Cumulative)]]
        if (n1 != n2) {
          stop("Inconsistent observation counts in ", varName, "(", n1, ", ", n2, ")")
        }
        skip <- any(!is.na(`Skip.to.Item`))
        ## some tables (like DRXFCD_* food tables) have NA instead of
        ## "Missing" in Value.Description. 
        nmissing <- Count[`Value.Description` == "Missing" | is.na(`Value.Description`)]
        ## A very few instances have no 'Missing' code: warn for them
        stopifnot(length(nmissing) <= 1)
        if (length(nmissing) == 0 || !is.finite(nmissing)) {
          warning("No 'Missing' code in variable ", varName)
          nmissing <- NA_real_
        }
        looksLikeNumeric <- "Range of Values" %in% `Value.Description`
        numLevels <- length(Count) # should be 2 for numeric
        data.frame(varname = varName, label = sasLabel,
                   nobs_cb = n1, na_cb = nmissing,
                   has_range = looksLikeNumeric, nlevels = numLevels,
                   skip = skip)
      })
    } else 
      data.frame(varname = varName, label = sasLabel,
                 nobs_cb = NA, na_cb = NA,
                 has_range = NA, nlevels = NA,
                 skip = NA)
}


nhanesSummary_codebook <- function(nh_table, src = nhanesCodebook(nh_table, ...), ...)
{
  ## start by getting codebook. This is a list, not data.frame, with
  ## one component for each variable
  df <- src
  var_info <- lapply(df, variableSummary_codebook)
  nhtatt <- Reduce(rbind, var_info)
  if (!missing(nh_table)) nhtatt <- cbind(table = nh_table, nhtatt)
  return(nhtatt)
}

##' Summarize a NHANES table
##'
##' Returns a per-variable summary of a NHANES table either using the
##' actual data or its corresponding codebook
##' @title Summarize NHANES table
##' @param nh_table the name of a valid NHANES table
##' @param use character string, whether to create a summary from the
##'   data itself or the codebook, which respectively use either the
##'   NHANES SAS data files or the HTML documentation files. If
##'   \code{use = "both"} then both are computed as merged; the
##'   \code{src} and \code{...} arguments are ignored in this case.
##' @param ... additional arguments, usually passed on to either
##'   \code{\link{nhanes}} or \code{\link{nhanesCodebook}} as
##'   appropriate. Alternatively, the \code{src} argument can be used
##'   to pass on an already available data frame or codebook, but this
##'   must be consistent with the \code{use} argument.
##' @return A data frame with one row per variable, with columns
##'   depending on the value of the \code{use} argument.
##' @examples
##' \donttest{nhanesTableSummary('DEMO_D', use = "data")}
##' \donttest{nhanesTableSummary('DEMO_D', use = "codebook")}
##' @export
nhanesTableSummary <- function(nh_table, use = c("data", "codebook", "both"), ...)
{
  use <- match.arg(use)
  switch(use,
         data = nhanesSummary_data(nh_table, ...),
         codebook = nhanesSummary_codebook(nh_table, ...),
         both = {
           info_cb <- nhanesSummary_codebook(nh_table)
           info_data <- nhanesSummary_data(nh_table)
           ## merge them, but be careful because they may not be in
           ## same order (esp in the database)
           rownames(info_data) <- info_data$varname
           ans <- cbind(info_cb, info_data[info_cb$varname, -c(1, 2)])
           rownames(ans) <- NULL
           ans
         })
}




