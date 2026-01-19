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
#'   code translations to be applied to the data (default=1).
#' @param details If TRUE then all available table translation
#'   information is displayed (default=FALSE).
#' @param dxa If TRUE then the 2005-2006 DXA translation table will be
#'   used (default=FALSE).
#' @param cleanse_numeric Logical flag. If \code{TRUE}, some special
#'   codes in numeric variables, such as \sQuote{Refused} and
#'   \sQuote{Don't know} will be converted to \code{NA}.
#' @param file Optional local file path to save or read the HTML content.
#'   If specified and file exists, the local file will be used instead of 
#'   downloading.
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
#' \donttest{
#' ## May fail if CDC website is unavailable
#' try({
#'     nhanesTranslate('DEMO_B', c('DMDBORN','DMDCITZN')) |> print()
#'     nhanesTranslate('BPX_F', 'BPACSZ', details = TRUE) |> print()
#'     nhanesTranslate('BPX_F', 'BPACSZ', data = nhanes('BPX_F')) |> str()
#'     trans_demo = nhanesTranslate('DEMO_B')
#'     length(trans_demo)
#' })
#' }
#' @export
#' 
nhanesTranslate <- function(nh_table, colnames=NULL, data = NULL, nchar = 128, 
                            mincategories = 1, details=FALSE, dxa=FALSE,
                            cleanse_numeric = FALSE,
                            file = NULL)
{
  if(isFALSE(dxa) && !grepl("^(Y_)\\w+", nh_table) && .useDB()) {
    return(.nhanesTranslateDB(nh_table, colnames,data,nchar,mincategories,details))
  }

  # if(is.null(colnames)) {
  #   message('Column name is required')
  #   return(NULL)
  # }
  
  if(!is.null(data) && details == TRUE) {
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
        paste0("https://wwwn.cdc.gov/Nchs/Data/", nh_year, '/Public/2012/DataFiles/', nh_table, '.htm')
      } else {
        paste0(nhanesTableURL, nh_year, '/DataFiles/', nh_table, '.htm')
      }
  }
  # use local file if specified and already exists
  if (!is.null(file) && file.exists(file)) {
    code_translation_url = file
  }
  hurl <- .checkHtml(code_translation_url)
  if (!is.null(file) && !file.exists(file) && !is.null(hurl)) {
    dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
    xml2::write_html(hurl, file)
  }
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
    nskip <- grep('Range of Values', translations) ## 'Range' of values indicates the column is not coded
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
      else if (isTRUE(cleanse_numeric)) {
        cname <- unlist(colnames[i])
        sstr <- paste0('^', cname, '$') # Construct the search string
        idx <- grep(sstr, names(data)) 
        if (length(idx) > 0 && !is.null(translations[[cname]])) {
          ## The column is present.
            data[[idx]] <- code2numeric(data[[idx]], translations[[cname]])
            translated <- c(translated, cname)
        }
        else {
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



## The raw2translated() function below provides an alternative
## translation interface given input (raw) data and input
## codebook. The intent is for this to work with data either from the
## NHANES website or from the DB. raw2translated() is currently
## unexported, but used in nhanesFromURL(). It may be exported in some
## form in future.

## The intermediate functions are various helper utilities.


## numeric variables sometimes have 'codes' which have special
## meaning. Often these indicate left or right censoring (e.g., age >=
## 80 is coded as 80), or some special kind of missingness (e.g.,
## "refused" / "don't know"). Sometimes these need to be handled on a case
## by case basis.

## Converting these to numerical values inevitably lose information,
## but we need to do _something_. The following table is a (possibly
## incomplete) list of special codes that come up in the context of
## numeric variables, with an indication of how we plan to handle
## them.


specialNumericCodes <-
    c(Missing = "NA",            # already NA
      "Don't know" = "NA",
      Refused = "NA",
      "0" = "unknown",
      "No Lab Result" = "unknown",
      "Since birth" = "unknown",
      Refuse = "NA",
      "Fill Value of Limit of Detection" = "unknown",
      None = "unknown",
      Never = "unknown",
      "No lab specimen" = "unknown",
      "Compliance <= 0.2" = "unknown",
      "Could not obtain" = "NA",
      "900 +" = "censored",
      "Less than 1 month" = "censored",
      "Day 1 dietary recall not done/incomplete" = "unknown",
      "Day 2 dietary recall not done/incomplete" = "unknown",
      "95 cigarettes or more" = "censored",
      "Below Limit of Detection" = "unknown",
      "Provider did not specify goal" = "unknown",
      "2000 or more" = "censored",
      "1 cigarette or less" = "censored",
      "Never on a daily basis" = "unknown",
      "Participants 6+ years with no lab specimen" = "unknown",
      "3 or More" = "censored",
      "Don't Know" = "NA",
      "Value greater than or equal to 5.00" = "censored",
      "7 or more" = "censored",
      "80 years or older" = "censored",
      "1-14 minutes" = "interval",
      "70 or more" = "censored",
      "8400 and over" = "censored",
      "First Below Detection Limit Fill Value" = "unknown",
      "Never smoked cigarettes regularly" = "unknown",
      "No modification" = "unknown",
      "No time spent outdoors" = "unknown",
      "Non-Respondent" = "NA",
      "Second Below Detection Limit Fill Value" = "unknown",
      "Still breastfeeding" = "unknown",
      "Still drinking formula" = "unknown",
      "100 or more" = "censored",
      "Below Detection Limit Fill Value" = "unknown",
      "More than 21 meals per week" = "censored",
      "No Lab Specimen" = "NA",
      "3 or more" = "censored",
      "DON'T KNOW" = "NA",
      "Less than 1 year" = "censored",
      "Less than one hour" = "censored",
      "1 month or less" = "censored",
      "13 pounds or more" = "censored",
      "20 or more times" = "censored",
      "6 years or less" = "censored",
      "7 or more people in the Household" = "censored",
      "85 years or older" = "censored",
      "Don't know/not sure" = "NA",
      "First Fill Value of Limit of Detection" = "unknown",
      "Second Fill Value of Limit of Detection" = "unknown",
      "100 +" = "censored",
      "11 or more" = "censored",
      "11 years or under" = "censored",
      "19 years or under" = "censored",
      "60 years or older" = "censored",
      "At or below detection limit fill value" = "unknown",
      "Dont Know" = "NA",
      "More than 1095 days (3-year) old" = "unknown",
      "Never had cholesterol test" = "unknown",
      "Never heard of LDL" = "unknown",
      "Never smoked a whole cigarette" = "unknown",
      "Participants 12+ years with no lab specimen" = "unknown",
      "12 hours or more" = "censored",
      "20 or more" = "censored",
      "6 times or more" = "censored",
      "6 years or under" = "censored",
      "At work or at school 9 to 5 seven days a week" = "unknown",
      "Does not work or go to school" = "unknown",
      "Hasn't started yet" = "unknown",
      REFUSED = "NA",
      "12 years or younger" = "censored",
      "13 or more" = "censored",
      "40 or more" = "censored",
      "7 or more people in the Family" = "censored",
      "Current HH FS benefits recipient last receive" = "unknown",
      "Less than weekly" = "unknown",
      "More than 90 times in 30 days" = "censored",
      "Non-current HH FS benefits recipient last rec" = "unknown",
      "PIR value greater than or equal to 5.00" = "censored",
      "Since Birth" = "unknown",
      Ungradable = "unknown",
      "11 or More" = "censored",
      "40 or More" = "censored",
      "50 years or more" = "censored",
      "85 or older" = "censored",
      "95 or more" = "censored",
      English = "categorical",
      "English and Spanish" = "categorical",
      "Less than one year" = "unknown",
      "More than 1 year unspecified" = "unknown",
      "Never smoked a pipe regularly" = "unknown",
      "Never smoked cigars regularly" = "unknown",
      "Never used chewing tobacco regularly" = "unknown",
      "Never used snuff regularly" = "unknown",
      "No Lab Result or Not Fasting for 8 to <24 hou" = "NA",
      "No lab samples" = "NA",
      "Not MEC Examined" = "NA",
      Other = "NA",
      Spanish = "categorical",
      "Unable to do activity (blind)" = "NA",
      "11 pounds or more" = "censored",
      "13 or More" = "censored",
      "14 hours or more" = "censored",
      "15 drinks or more" = "censored",
      "20 years or older" = "censored",
      "3 pounds or less" = "censored",
      "480 Months or more" = "censored",
      "500 mg or higher" = "censored",
      "60 minutes or more" = "censored",
      "600 Months or more" = "censored",
      "70 to 150" = "interval",
      "80 Hours or more" = "censored",
      "85 or greater years" = "censored",
      "Below First Limit of Detection" = "unknown",
      "Below Second Limit of Detection" = "unknown",
      "Don't know what is 'whole grain'" = "unknown",
      "Less than monthly" = "unknown",
      "Less then 3 hours" = "censored",
      "More than $1000" = "censored",
      "More than 21" = "censored",
      "More than 300 days" = "censored",
      "More than 365 days (1-year) old" = "unknown",
      "More than 730 days (2-year) old" = "unknown",
      "Never heard of A1C test" = "unknown",
      "No Lab samples" = "unknown",
      "Not tested in last 12 months" = "unknown",
      "Participants 3+ years with no lab specimen" = "unknown",
      refused = "NA",
      "Single person family" = "unknown",
      "0-5 Months" = "interval",
      "1 year or less" = "censored",
      "1-5 Hours" = "interval",
      "20 days or more" = "censored",
      "20 to 150" = "interval",
      "4 or more" = "censored",
      "400 and over" = "censored",
      "60 or more months" = "censored",
      "7 years or less" = "censored",
      "80 or greater years" = "censored",
      "9 or fewer" = "censored",
      "Less than 10 years of age" = "censored",
      "Less than one day" = "censored",
      "More than 20 times a month" = "censored",
      "More than 21 times per week" = "censored",
      "No lab result" = "NA",
      "No lab Result" = "NA",
      "Participants 3+ years with no Lab Result" = "unknown",
      "Participants 3+ years with no surplus lab spe" = "unknown",
      "Participants 6+ years with no Lab Result" = "unknown",
      "Participants 6+ years with no lab specimen." = "unknown",
      "Third Fill Value of Limit of Detection" = "unknown"
      )


## The next two functions convert 'raw' codes to either numeric or
## string (categorical) values using a codebook. For now, we will only
##
## - convert the 'NA' values to NA
## - complain if we see 'categorical' values

code2numeric <- function(x, cb, cleanse = TRUE)
{
    if (isFALSE(cleanse)) return(x)
    ## x is numeric codes (which may alread include NAs from '.')
    ## cb is a data frame with columns Code.or.Value and Value.Description
    ##
    ## The codes corresponding to special Value.Description-s need to be handled
    cb <- subset(cb, !(Value.Description %in% c("Range of Values", "Missing")))
    if (nrow(cb) == 0) return(x)
    map <- with(cb, structure(as.numeric(Code.or.Value), names = Value.Description))
    missingDesc <- names(which(specialNumericCodes == "NA"))
    categoricalDesc <- names(which(specialNumericCodes == "categorical"))
    ## are any remaining values to be mapped to NA
    wmissing <- names(map) %in% missingDesc
    if (any(wmissing)) {
        x[ x %in% map[wmissing] ] <- NA_real_
    }
    if (any(names(map) %in% categoricalDesc)) {
        warning('non-numeric descriptions found in apparently numeric variable: ',
                paste(map[ names(map) %in% categoricalDesc ], collapse = ", "))
    }
    x
}

code2categorical <- function(x, cb) {
    map <- with(cb, structure(Value.Description, names = as.character(Code.or.Value)))
    map[as.character(x)]
}

## some variables are already character strings, but they may also
## have 'special' codes that need to be translated. code2categorical
## will not handle them because unrecognized codes become NA
char2categorical <- function(x, cb) {
    if (!is.character(x)) stop("Expected character, found ", typeof(x))
    map <- with(cb, structure(Value.Description, names = as.character(Code.or.Value)))
    i <- which(x %in% names(map)) # should be relatively few
    x[i] <- map[x[i]]
    x
}

translateVariable <- function(x, cb, cleanse_numeric = TRUE) {
    colnames(cb) <- make.names(colnames(cb)) # 'fix' names if needed
    ## decide if 'numeric'
    if ("Range of Values" %in% cb$Value.Description)
        code2numeric(x, cb, cleanse = cleanse_numeric)
    else if ("Value was recorded" %in% cb$Value.Description)
        char2categorical(x, cb)
    else
        code2categorical(x, cb)
}

raw2translated <- function(rawdf, codebook, cleanse_numeric = TRUE)
{
    vars <- names(rawdf)
    vars <- vars[!(vars %in% c("SEQN", "SAMPLEID"))] # keep these as-is; anything else?
    ## codebook can be NULL if no codebook info present (e.g., for diet / food variables)
    if (!is.null(codebook))
        names(codebook) <- toupper(names(codebook))
    for (v in vars) {
        ## most variables are coded as numbers, but some are
        ## character. We do not make a distinction, but we may want to
        ## alert about character variables because they are rare
        if (is.null(codebook) && is.character(rawdf[[v]])) {
            warning("Skipping translation for character variable with missing codebook: ", v)
        }
        else if (is.null(codebook[[v]])) {
            warning("Variable not found in codebook, skipping translation for variable: ", v)
        }
        else {
            names(codebook[[v]]) <- toupper(names(codebook[[v]]))
            if (!is.list(codebook[[v]]) || is.null(codebook[[v]][[v]])) {
                warning("Missing codebook table, skipping translation for variable: ", v)
            }
            else {
                rawdf[[v]] <- translateVariable(rawdf[[v]], codebook[[v]][[v]],
                                                cleanse_numeric = cleanse_numeric)
            }
        }
    }
    rawdf
}

