# FUNCTIONS:
#   nhanesManifest
#------------------------------------------------------------------------------

## Helper functions used by nhanesManifest(). 

## Compute the size of a file hosted at a URL, as reported by the server

.nhanesFileSize <- function(url)
{
    h <- tolower(curlGetHeaders(url))
    ok <- startsWith(h, "content-length")
    if (any(ok)) {
        ## pick the last one
        id <- rev(which(ok))[[1]]
        as.numeric(strsplit(trimws(h[[id]]), ":")[[1]][[2]])
    }
    else NA_real_
}

## Compute a generous upper bound for how many seconds should be
## sufficient to download a file from a given URL. Uses the file size
## reported by the server.

estimate_timeout <- function(url, factor = 1, perMB = 10)
{
    ## By default, estimate at 10 sec / MB, and multiply by factor
    if (factor > 0) {
        fsize <- .nhanesFileSize(url)
        factor * perMB * (fsize / 1e6)
    }
    else NA_real_
}

## Compute the size of a file hosted at a URL, as reported by the
## server, using a URL fragment (with prefix). Used to compute and
## report file sizes in the manifest if requested.

.get_content_length <- function(url, verbose = FALSE)
{
    url_base <- "https://wwwn.cdc.gov"
    if (!startsWith(tolower(url), "/nchs/")) {
        if (verbose) message("SKIPPING ", url)
        return(NA_real_)
    }
    url <- paste0(url_base, url)
    if (verbose) message(url)
    .nhanesFileSize(url)
}


## In <https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx>, the
## "links" to some data / doc files (VID_B, VID_C, etc) do not
## actually link to the data / doc files, but instead refer to a
## 'analytical note', with further query parameters used to create a
## redirect URL to the actual data / doc files. These helper functions
## suitably map the hrefs in the data listing.

## convert query parameters in URL to target URLs
string2url <- function(s)
{
  s <- gsub("=", ":",
            gsub("&", "\n", s, fixed = TRUE),
            fixed = TRUE)
  params <- read.dcf(textConnection(s))
  with(as.list(params[1, , drop = TRUE]),
       sprintf("/nchs/data/Nhanes/Public/%s/DataFiles/%s.%s",
               b, d, x)) # another component (e=endYear) is ignored in new scheme
}

## suitably remap a vector of URLs in which some may point to the analytical note
parseRedirect <- function(s, prefix = "../vitamind/analyticalnote.aspx?")
{
  ans <- s
  tofix <- startsWith(tolower(s), tolower(prefix))
  if (!any(tofix)) return(s)
  ss <- substring(s[tofix], 1 + nchar(prefix), 999)
  ss <- sapply(ss, string2url)
  ans[tofix] <- ss
  ans
}


##' Download and parse NHANES manifests
##' 
##' Downloads and parses NHANES manifests for data tables and
##' variables, and returns them as data frames.
##'
##' The NHANES website maintains several listings (manifests) of
##' tables and associated variables, which can be downloaded using these functions.
##'
##' The list of tables for which data is available publicly can be
##' found at
##' \url{https://wwwn.cdc.gov/Nchs/Nhanes/search/DataPage.aspx}, with
##' further restriction to specific components possible by specifying
##' an additional query parameter as below. This is the \emph{public}
##' manifest.
##'
##' Limited access tables (also referred to as RDC only tables) are
##' listed at
##' \url{https://wwwn.cdc.gov/Nchs/Nhanes/search/DataPage.aspx?Component=LimitedAccess}.
##' This is the \emph{limited access} manifest.
##' 
##' Available variables are listed according to component at
##' \url{https://wwwn.cdc.gov/nchs/nhanes/search/variablelist.aspx?Component=Demographics},
##' etc. These are the \emph{variable} manifests.
##'
##' @param which Either "public" or "limitedaccess" to get a manifest
##'   of available tables, or "variables" to get a manifest of
##'   available variables.
##' @param sizes Logical, whether to compute data file sizes (as
##'   reported by the server) and include them in the result.
##' @param dxa Logical, whether to include information on DXA tables.
##'   These tables contain imputed imputed Dual Energy X-ray
##'   Absorptiometry measurements, and are listed separately, not in
##'   the main listing.
##' @param component An optional character string specifying the
##'   component for which the public data manifest is to be
##'   downloaded. Valid values are \code{"demographics"},
##'   \code{"dietary"}, \code{"examination"}, \code{"laboratory"}, and
##'   \code{"questionnaire"}.  Partial matching is allowed, and case
##'   is ignored. Specifying a component for the public manifest will
##'   return a subset of the tables, but has the advantage that the
##'   result will include a description of each table.
##' @param verbose Logical flag indicating whether information on
##'   progress should be reported.
##' @param use_cache Logical flag indicating whether a cached version
##'   (from a previous download in the same session) should be used.
##' @param max_age Maximum allowed age of the cache in seconds
##'   (defaults to 24 hours). Cached versions that are older are
##'   ignored, even if available.
##' @return A data frame, with columns that depend on
##'   \code{which}.
##'
##'   For a manifest of tables, columns are "Table", "DocURL",
##'   "DataURL", "Years", "Date.Published". If \code{component} is
##'   specified, an additional column "Description" giving a
##'   description of the table will be included. If \code{sizes =
##'   TRUE}, an additional column "DataSize" giving the data file
##'   sizes in bytes (as reported by the server) is included.
##'
##'   For limited access tables, the "DataURL" and "DataSize" columns
##'   are omitted.
##'
##'   For a manifest of variables, columns are "VarName", "VarDesc",
##'   "Table", "TableDesc", "BeginYear", "EndYear", "Component", and
##'   "UseConstraints".
##' @note Duplicate rows are removed from the result. Most of these
##'   duplicates arise from duplications in the source tables for
##'   multi-cycle tables (which are repeated once for each cycle). One
##'   special case is the WHQ table which has two variables, WHD120
##'   and WHQ030, duplicated with differing variable
##'   descriptions. These are removed explicitly, keeping only the
##'   first occurrence.
##' @examples
##' \donttest{
##' ## May fail if CDC website is (available but) malformatted
##' try({
##'     manifest <- nhanesManifest(sizes = FALSE)
##'     dim(manifest)
##'     varmf <- nhanesManifest("variables", component = "lab")
##'     head(varmf)
##' })
##' }
##' 
##' @export
nhanesManifest <- function(which = c("public", "limitedaccess", "variables"),
                           sizes = FALSE, dxa = FALSE, component = NULL,
                           verbose = getOption("verbose"),
                           use_cache = TRUE, max_age = 24 * 60 * 60)
{
  if (!is.null(component)) {
    comps <- c("demographics", "dietary", "examination", "laboratory", "questionnaire")
    i <- pmatch(tolower(component), comps, nomatch = 0L)
    component <- if (i) comps[[i]] else NULL
    if (!is.null(component) && isTRUE(dxa)) {
      warning("'dxa = TRUE' is ignored when 'component' is specified")
      dxa <- FALSE
    }
  }
  which <- match.arg(which)
  cache_key <- if (which == "public")
                 paste(c("public", "sizes", "dxa", component)[c(TRUE, sizes, dxa, TRUE)],
                       collapse = "+")
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
           public = if (dxa) rbind(nhanesManifest_public(sizes = sizes, verbose = verbose),
                                   nhanesManifest_DXA_hardcoded(sizes))
                    else nhanesManifest_public(sizes = sizes, verbose = verbose,
                                               component = component),
           limitedaccess = nhanesManifest_limitedaccess(verbose = verbose),
           variables = nhanesManifest_variables(verbose = verbose)) |>
      unique()
  .nhanesCacheEnv[[ cache_key ]] <- list(manifest = ans, timestamp = Sys.time())
  ans
}



## download and parse public manifest

nhanesManifest_public <- function(sizes, verbose, component = NULL)
{
  if (!is.null(component)) dataURL <- paste0(dataURL, "?Component=", component)
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
  ## There are several spurious # which needs to be removed (withdrawn tables)
  hrefs <- hrefs[hrefs != "#"]
  df <- tab1 |> html_table() |> as.data.frame()
  df$Table <- sub(" Doc", "", df$Doc.File)
  ## PAHS_H and SSEVD_* were withdrawn
  ## The corresponding row has no useful HREFs, so there is a length mismatch
  ## subset(df, Date.Published == "Withdrawn")
  df <- subset(df, Date.Published != "Withdrawn")
  ## Compensate for spurious rows with Doc link but no Data link
  ## As of Dec 2024: DNMEPI
  ## As of Sep 2025: DSBI_L, DSII_L, DSPI_L (no more DNMEPI)
  for (drop_name in c("DNMEPI", "DSBI_L", "DSII_L", "DSPI_L")) {
      df <- subset(df, !(startsWith(Doc.File, drop_name) | startsWith(Data.File, drop_name)))
      hrefs <- hrefs[ !startsWith(basename(hrefs), drop_name)  ]
  }

  ## make sure lengths now match
  if (nrow(df) * 2 != length(hrefs)) stop("Wrong number of URLs in table manifest")
  df$DocURL <- hrefs[c(TRUE, FALSE)]
  df$DataURL <- hrefs[c(FALSE, TRUE)]
  ## subset(df, tools::file_ext(DataURL) != "XPT")
  df <- subset(df, startsWith(DataURL, "/") & endsWith(toupper(DataURL), ".XPT"))
  if (!is.null(df$Data.File.Name)) {
    df$Description <- df$Data.File.Name
    df <- df[c("Table", "Description", "DocURL", "DataURL", "Years", "Date.Published")]
  }
  else
    df <- df[c("Table", "DocURL", "DataURL", "Years", "Date.Published")]
  if (sizes) {
    if (verbose) message("Checking data file sizes...")
    s <- sapply(df$DataURL, .get_content_length, verbose = verbose)
    df$DataSize <- s
  }
  return(df)
}


## Downloading DXA manifest: 

## The following function can get the DXA table details from
## https://wwwn.cdc.gov/Nchs/Nhanes/Dxa/Dxa.aspx. However, one problem
## with this approach is that the Doc files for DXA, DXA_B, and DXA_C
## are PDF files which we cannot parse, and only the DXA_D doc is
## HTML. The workaround is to use the DXA_D doc / codebook for all
## four. We do this by maintaining a hard-coded version of the result,
## assuming that the information will not change going forward (the
## last update happened in 2016).

nhanesManifest_DXA <- function(sizes, verbose)
{
  if (verbose) message("Downloading ", dxaTablesURL)
  hurl <- .checkHtml(dxaTablesURL)
  if(is.null(hurl)) {
    message("Error occurred during read. No tables returned")
    return(NULL)
  }
  ##get to the table
  xpath <- '//*[@id="GridView1"]'
  tab1 <- hurl |> html_elements(xpath=xpath)
  ##pull out all the hrefs
  hrefs <- tab1 |> html_nodes("a") |> html_attr("href")
  df <- tab1 |> html_table() |> as.data.frame()
  df$Table <- sub(" Doc", "", df$Doc.File)
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

nhanesManifest_DXA_hardcoded <- function(sizes, verbose)
{
    keep <- if (isTRUE(sizes)) 1:6 else 1:5
    ## manually edited from nhanesManifest_DXA(sizes = TRUE)
    data.frame(Table = c("DXX_D", "DXX_C", "DXX_B", "DXX"),
               DocURL = rep("/nchs/data/nhanes/dxa/dxx_d.htm", 4),
               DataURL = c("/nchs/data/nhanes/dxa/dxx_d.xpt", 
                           "/nchs/data/nhanes/dxa/dxx_c.xpt",
                           "/nchs/data/nhanes/dxa/dxx_b.xpt", 
                           "/nchs/data/nhanes/dxa/dxx.xpt"),
               Years = c("2005-2006", "2003-2004", 
                         "2001-2002", "1999-2000"),
               Date.Published = c("Updated December 2016", 
                                  "Updated March 2010",
                                  "Updated March 2010",
                                  "Updated March 2010"),
               DataSize = c(29517840, 30371680, 32695200, 24737440))[keep]
}


## download and parse limited access manifest

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
  ## if(length(tab2) != 224) stop("CDC updated data manifest")
  htmNames = tab2
  df = tab1 |> html_table() |> as.data.frame()
  df = subset(df, !skip)
  df$Table = sub(" Doc", "", df$Doc.File)
  df$DocURL = htmNames
  df$Description = df$Data.File.Name
  df = df[,c("Table", "Description", "DocURL", "Years", "Date.Published")]
  return(df)
}

## download and parse variable manifest

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
