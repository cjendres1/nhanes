# FUNCTIONS:
#   nhanesManifest
#   nhanesTables
#   nhanesTableVars
#------------------------------------------------------------------------------

.get_content_length <- function(url, verbose = FALSE)
{
    url_base <- "https://wwwn.cdc.gov"
    if (!startsWith(url, "/Nchs/Nhanes")) {
        if (verbose) message("SKIPPING ", url)
        return(NA_real_)
    }
    url <- paste0(url_base, url)
    if (verbose) message(url)
    h <- tolower(curlGetHeaders(url))
    ok <- startsWith(h, "content-length")
    if (any(ok)) {
        ## pick the last one
        id <- rev(which(ok))[[1]]
        as.numeric(strsplit(trimws(h[[id]]), ":")[[1]][[2]])
    }
    else NA_real_
}

##' Downloads and parses NHANES manifests for public data
##' (available at
##' \url{https://wwwn.cdc.gov/Nchs/Nhanes/search/DataPage.aspx}),
##' limited access data
##' (\url{https://wwwn.cdc.gov/Nchs/Nhanes/search/DataPage.aspx?Component=LimitedAccess}),
##' and variables
##' (\url{https://wwwn.cdc.gov/nchs/nhanes/search/variablelist.aspx?Component=Demographics}, etc.),
##' and returns them as data frames.
##'
##' @title Download and parse NHANES manifests
##' @param which Either "public" or "limitedaccess" to get a manifest
##'   of available tables, or "variables" to get a manifest of
##'   available variables.
##' @param sizes Logical, whether to compute data file sizes (as
##'   reported by the server) and include them in the result.
##' @param verbose Logical flag indicating whether information on
##'   progress should be reported.
##' @param use_cache Logical flag indicating whether a cached version
##'   (from a previous download in the same session) should be used.
##' @param max_age Maximum allowed age of the cache in seconds
##'   (defaults to 24 hours). Cached versions that are older are
##'   ignored, even if available.
##' @return A data frame, with columns that depend on
##'   \code{which}. For a manifest of tables, columns are "Table",
##'   "DocURL", "DataURL", "Years", "Date.Published". If \code{sizes =
##'   TRUE}, an additional column "DataSize" giving the data file
##'   sizes in bytes (as reported by the server) is included. For
##'   limited access tables, the "DataURL" and "DataSize" columns are
##'   omitted. For a manifest of variables, columns are "VarName",
##'   "VarDesc", "Table", "TableDesc", "BeginYear", "EndYear",
##'   "Component", and "UseConstraints".
##' @note Duplicate rows are removed from the result. Most of these
##'   duplicates arise from duplications in the source tables for
##'   multi-cycle tables (which are repeated once for each cycle). One
##'   special case is the WHQ table which has two variables, WHD120
##'   and WHQ030, duplicated with differing variable
##'   descriptions. These are removed explicitly, keeping only the
##'   first occurrence.
##' @examples
##' manifest <- nhanesManifest(sizes = FALSE)
##' dim(manifest)
##' 
##' @export
nhanesManifest <- function(which = c("public", "limitedaccess", "variables"),
                           sizes = FALSE, verbose = getOption("verbose"),
                           use_cache = TRUE, max_age = 24 * 60 * 60)
{
  which <- match.arg(which)
  cache_key <- if (which == "public" && isTRUE(sizes)) "public+sizes"
               else which
  if (isTRUE(use_cache)) {
    cache_val <- .nhanesCacheEnv[[ cache_key ]]
    if (!is.null(cache_val) && as.numeric(Sys.time() - cache_val$timestamp) < max_age) {
      if (verbose) message("Using previously cached version of manifest")
      return(cache_val$manifest)
    }
  }
  ## otherwise, fresh download
  ans <- 
    switch(which,
           public = nhanesManifest_public(sizes = sizes, verbose = verbose),
           limitedaccess = nhanesManifest_limitedaccess(verbose = verbose),
           variables = nhanesManifest_variables(verbose = verbose)) |>
      unique()
  .nhanesCacheEnv[[ cache_key ]] <- list(manifest = ans, timestamp = Sys.time())
  ans
}


.nhanesCacheEnv <- new.env(parent = emptyenv())


nhanesManifest_public <- function(sizes, verbose)
{
  if (verbose) message("Downloading ", dataURL)
  hurl <- .checkHtml(dataURL)
  if(is.null(hurl)) {
    message("Error occurred during read. No tables returned")
    return(NULL)
  }
  ##get to the table
  xpath <- '//*[@id="GridView1"]'
  tab1 <- hurl |> html_elements(xpath=xpath)
  ##pull out all the hrefs
  hrefs <- tab1 |> html_nodes("a") |> html_attr("href") |> parseRedirect()
  ## There's a spurious # which needs to be removed
  hrefs <- hrefs[hrefs != "#"]
  df <- tab1 |> html_table() |> as.data.frame()
  df$Table <- sub(" Doc", "", df$Doc.File)
  ## PAHS_H was withdrawn - only one entry in the table
  ## The corresponding row has no useful HREFs, so there is a length mismatch
  ## subset(df, Date.Published == "Withdrawn")
  df <- subset(df, Date.Published != "Withdrawn")
  ## make sure lengths now match
  if (nrow(df) * 2 != length(hrefs)) stop("Wrong number of URLs in table manifest")
  df$DocURL <- hrefs[c(TRUE, FALSE)]
  df$DataURL <- hrefs[c(FALSE, TRUE)]
  ## subset(df, tools::file_ext(DataURL) != "XPT")
  df <- subset(df, startsWith(DataURL, "/") & endsWith(toupper(DataURL), ".XPT"))
  df <- df[c("Table", "DocURL", "DataURL", "Years", "Date.Published")]
  if (sizes) {
    if (verbose) message("Checking data file sizes...")
    s <- sapply(df$DataURL, .get_content_length, verbose = verbose)
    df$DataSize <- s
  }
  return(df)
}

nhanesManifest_limitedaccess <- function(verbose)
{
  if (verbose) message("Downloading ", ladDataURL)
  hurl <- .checkHtml(ladDataURL)
  if(is.null(hurl)) {
    message("Error occurred during read. No tables returned")
    return(NULL)
  }
  ##get to the table
  xpath <- '//*[@id="GridView1"]'
  tab1 <- hurl |> html_elements(xpath=xpath)
  ##pull out all the hrefs
  tab2 = tab1 |> html_nodes("a") |> html_attr("href")
  ## drop Omp and # (withdrawn)
  skip <- (tab2 %in% c("#", "/Nchs/Nhanes/Omp/Default.aspx"))
  tab2 <- tab2[!skip]
  ##whenever they update we need to error out and then fix it
  if(length(tab2) != 223) stop("CDC updated data manifest")
  htmNames = tab2
  df = tab1 |> html_table() |> as.data.frame()
  df = subset(df, !skip)
  df$Table = sub(" Doc", "", df$Doc.File)
  df$DocURL = htmNames
  df = df[,c("Table", "DocURL", "Years", "Date.Published")]
  return(df)
}

nhanesManifest_variables <- function(verbose = TRUE)
{
  xpath <- '//*[@id="GridView1"]'
  parseComponent <- function(url)
  {
    if (verbose) message("Downloading ", url)
    hurl <- .checkHtml(url)
    if(!is.null(hurl)) {
      tab <- hurl |> html_elements(xpath = xpath)
      df <- tab |> html_table() |> as.data.frame()
      if (nrow(df) > 0) return(df)
      stop("Failed to parse URL: ", url)
    }
  }
  df <- Reduce(rbind, lapply(varURLs, parseComponent))
  names(df) <- c("VarName", "VarDesc", "Table", "TableDesc",
                 "BeginYear", "EndYear", "Component",
                 "UseConstraints")
  ## WHQ has TWO variables (WHD120 and WHQ030) which are duplicated;
  ## these are not removed when we retain unique rows later because
  ## the VarDesc column is different. We handle this as a special case
  ## and omit them here.
  WHQ_WHD120_dup <- with(df, which(Table == "WHQ" & VarName == "WHD120"))
  if (length(WHQ_WHD120_dup) > 1) { # retain first occurrence only
      drop_rows <- WHQ_WHD120_dup[-1]
      df <- df[ -drop_rows, ]
  }
  WHQ_WHQ030_dup <- with(df, which(Table == "WHQ" & VarName == "WHQ030"))
  if (length(WHQ_WHQ030_dup) > 1) { # retain first occurrence only
      drop_rows <- WHQ_WHQ030_dup[-1]
      df <- df[ -drop_rows, ]
  }
  df
}





# helper functions

string2url <- function(s)
{
  s <- gsub("=", ":",
            gsub("&", "\n", s, fixed = TRUE),
            fixed = TRUE)
  e <- read.dcf(textConnection(s))
  with(as.list(e[1, , drop = TRUE]),
       sprintf("/Nchs/Nhanes/%s-%s/%s.%s", b, e, d, x))
}


parseRedirect <- function(s, prefix = "../vitamind/analyticalnote.aspx?")
{
  ans <- s
  tofix <- startsWith(tolower(s), tolower(prefix))
  ss <- substring(s[tofix], 1 + nchar(prefix), 999)
  ss <- sapply(ss, string2url)
  ans[tofix] <- ss
  ans
}



#------------------------------------------------------------------------------
#' Returns a list of table names for the specified survey group.
#' 
#' Enables quick display of all available tables in the survey group.
#' 
#' @importFrom stringr str_replace str_match str_split str_remove str_detect
#' @importFrom rvest html_elements html_table
#' @importFrom xml2 read_html
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
#' exam = nhanesTables('EXAM', 2007)
#' dim(exam)
#' \donttest{lab = nhanesTables('LAB', 2009, details=TRUE, includerdc=TRUE)}
#' \donttest{dim(lab)}
#' \donttest{q = nhanesTables('Q', 2005, namesonly=TRUE)}
#' \donttest{length(q)}
#' \donttest{diet = nhanesTables('DIET', 'P')}
#' \donttest{dim(diet)}
#' \donttest{exam = nhanesTables('EXAM', 'Y')}
#' \donttest{dim(exam)}
#' @export
#'

nhanesTables <- function(data_group, year, nchar = 128,
                         details = FALSE, namesonly = FALSE, includerdc=FALSE)
{
  if( !(data_group %in% names(nhanes_group)) ) {
    stop("Invalid survey group")
    return(NULL)
  }
  
  if(is.numeric(year) && .useDB()){
    return(.nhanesTablesDB(data_group, year, nchar, details, namesonly, includerdc))
  }
  
  component <- nhanes_group[data_group]
  if (length(component) != 1) stop("'data_group' must be a single string")

  if(year == 'P' || year == 'p') {
    turl <- paste0(nhanesURL, 'search/datapage.aspx?Component=',
                   component,
                   '&CycleBeginYear=', '2017-2020')
  } else if (year == 'Y' || year == 'y') {
    turl <- paste0(nhanesURL, 'search/NnyfsData.aspx?Component=',
                   component,
                   '&CycleBeginYear=', '2012')
  } else {
    nh_year <- .get_nh_survey_years(year)
    turl <- paste0(nhanesURL, 'search/variablelist.aspx?Component=',
                   component,
                   '&CycleBeginYear=', unlist(str_split(nh_year, '-'))[[1]])
  }
  
  # At this point df contains every table for the specified survey & year
  hurl <- .checkHtml(turl)
  if(is.null(hurl)) {
    message("Error occurred during read. No tables returned")
    return(NULL)
  }
  df <- hurl |> html_elements(xpath=xpath) |> html_table() |> as.data.frame()
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
  
  if (!(nhanes_group[data_group] == 'Non-Public')){
    if(!includerdc) {
      df <- subset(df, Use.Constraints != "RDC Only")
    }
  }
  
  if(details) {
    df <- unique(df[,3:length(df)])
  } else {
    df <- unique(df[,c('Data.File.Name', 'Data.File.Description')])
  }
  #  df <- rename(df, c("Data.File.Name"="FileName","Data.File.Description"="Description"))
  
  # Here we exclude tables that overlap from earlier surveys
  # Get possible table suffixes for the specified year
  if(nh_year != "1999-2000") { ## No exclusion needed for first survey
    suffix <- paste0("_", names(data_idx[data_idx == nh_year]))
    ## FIXME: Should also add a $ at the end?
    if(nh_year == '2005-2006') { suffix <- c(suffix, anomalytables2005) }
    pattern <- paste(suffix, collapse = "|")
    ## matches <- unique(grep(pattern, df[['Data.File.Name']], value=TRUE))  
    ## df <- df[(df$Data.File.Name %in% matches),]
    df <- subset(df, grepl(pattern, Data.File.Name))
  }
  if(namesonly) {
    return(as.character(df[[1]]))
  }
  df$Data.File.Description <- substring(df$Data.File.Description, 1, nchar)
  row.names(df) <- NULL
  return(df)  
}

#------------------------------------------------------------------------------
#' Displays a list of variables in the specified NHANES table.
#' 
#' Enables quick display of table variables and their definitions.
#' 
#' @importFrom stringr str_replace str_split
#' @importFrom rvest html_elements html_table
#' @importFrom xml2 read_html
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
#' \donttest{lab_cbc = nhanesTableVars('LAB', 'CBC_E')}
#' \donttest{dim(lab_cbc)}
#' \donttest{exam_ohx = nhanesTableVars('EXAM', 'OHX_E', details=TRUE, nchar=50)}
#' \donttest{dim(exam_ohx)}
#' \donttest{demo = nhanesTableVars('DEMO', 'DEMO_F', namesonly = TRUE)}
#' \donttest{length(demo)}
#' @export
#' 
nhanesTableVars <- function(data_group, nh_table, details = FALSE, nchar=128, namesonly = FALSE) {
  if( !(data_group %in% names(nhanes_group)) ) {
    stop("Invalid survey group")
    return(NULL)
  }

  if(!grepl("^(P_|Y_)\\w+", nh_table) && .useDB()){
    return(.nhanesTableVarsDB(data_group, nh_table, details, nchar, namesonly))
  }

  component <- nhanes_group[data_group]

  if(length(grep('^P_', nh_table))>0){
    nh_year <- '2017-2020'
    turl <- paste0(nhanesURL, 'search/variablelist.aspx?Component=', 
                   component, 
                   '&Cycle=', nh_year)
    
  } else {
    nh_year <- .get_year_from_nh_table(nh_table)
    turl <- paste0(nhanesURL, 'search/variablelist.aspx?Component=', 
                   component, 
                   '&CycleBeginYear=', unlist(str_split(nh_year, '-'))[[1]]) 
  }
  
  hurl <- .checkHtml(turl) 
  if(is.null(hurl)) {
    message("Error occurred during read. No table variables returned")
    return(NULL)
  }
  df <- hurl |> html_elements(xpath=xpath) |> html_table() |> as.data.frame()
  
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
  df[[2]] <- substring(df[[2]], 1, nchar)
  if( namesonly == TRUE ) {
    return(as.character(unique(df[[1]])))
  }
  row.names(df) <- NULL
  return(unique(df))
}

