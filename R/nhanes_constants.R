# nhanes_constants.R

## The two 'constants' nhanesTableURL and nhanesManifestPrefix are
## designed to be changed dynamically (currently by setting an
## environment variable) to allow <table>.htm and <table>.xpt files
## to be accessed from a location other than the NHANES website (e.g.,
## from a local copy). To keep the implementing functions see them as
## regular character variables, we implement them as active bindings
## (see .onLoad() in zzz.R).
##
## Default values:
##   nhanesTableURL <- 'https://wwwn.cdc.gov//Nchs/Data/Nhanes/Public/'
##   nhanesManifestPrefix <- 'https://wwwn.cdc.gov'

## Note that the CDC changed the layout of data / doc files in November
## 2024. For example, the DEMO_H doc URL, which was previously 
##
## <https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DEMO_H.htm>
##
## has changed to
##
## <https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/DEMO_H.htm>
##
## Consequently, the default value of `nhanesTableURL` has from
## 'https://wwwn.cdc.gov/Nchs/Nhanes/' to
## 'https://wwwn.cdc.gov//Nchs/Data/Nhanes/Public/'. The 'DataFiles'
## part has been incorporated as needed in other functions such as nhanes().

ab_nhanesTableURL <- function(x) {
    if (!missing(x)) stop("Invalid assignment")
    paste0(Sys.getenv("NHANES_TABLE_BASE", unset = "https://wwwn.cdc.gov"),
           "/Nchs/Data/Nhanes/Public/")
}

ab_nhanesManifestPrefix <- function(x) {
    if (!missing(x)) stop("Invalid assignment")
    Sys.getenv("NHANES_TABLE_BASE", unset = "https://wwwn.cdc.gov")
}

nhanesURL <- 'https://wwwn.cdc.gov/Nchs/Nhanes/'
dataURL <- 'https://wwwn.cdc.gov/Nchs/Nhanes/search/DataPage.aspx'
ladDataURL <- 'https://wwwn.cdc.gov/Nchs/Nhanes/search/DataPage.aspx?Component=LimitedAccess'
dxaURL  <- "https://wwwn.cdc.gov/nchs/data/nhanes/dxa/"
dxaTablesURL  <- "https://wwwn.cdc.gov/Nchs/Nhanes/Dxa/Dxa.aspx"

demoURL <- "https://wwwn.cdc.gov/nchs/nhanes/search/variablelist.aspx?Component=Demographics"
dietURL <- "https://wwwn.cdc.gov/nchs/nhanes/search/variablelist.aspx?Component=Dietary"
examURL <- "https://wwwn.cdc.gov/nchs/nhanes/search/variablelist.aspx?Component=Examination"
labURL  <- "https://wwwn.cdc.gov/nchs/nhanes/search/variablelist.aspx?Component=Laboratory"
qURL    <- "https://wwwn.cdc.gov/nchs/nhanes/search/variablelist.aspx?Component=Questionnaire"
ladURL  <- "https://wwwn.cdc.gov/nchs/nhanes/search/variablelist.aspx?Component=LimitedAccess"
varURLs <- c(demoURL, dietURL, examURL, labURL, qURL) #, ladURL)


# Create a list of nhanes groups
# Include convenient aliases
nhanes_group <-
  c(DEMO         = "Demographics",
    DEMOGRAPHICS = "Demographics",
    DIETARY      = "Dietary",
    DIET         = "Dietary",
    EXAMINATION  = "Examination",
    EXAM         = "Examination",
    LABORATORY   = "Laboratory",
    LAB          = "Laboratory",
    QUESTIONNAIRE= "Questionnaire",
    Q            = "Questionnaire",
    LIMITED      = "Non-Public",
    LTD          = "Non-Public")

## nhanes_survey_groups <- unique(nhanes_group) # not used anywhere

# Although continuous NHANES is grouped in 2-year intervals,
# for convenience we want to specify using a single year
nh_years <-
  c(`1999` = "1999-2000", `2000` = "1999-2000", `2001` = "2001-2002", 
    `2002` = "2001-2002", `2003` = "2003-2004", `2004` = "2003-2004", 
    `2005` = "2005-2006", `2006` = "2005-2006", `2007` = "2007-2008", 
    `2008` = "2007-2008", `2009` = "2009-2010", `2010` = "2009-2010", 
    `2011` = "2011-2012", `2012` = "2011-2012", `2013` = "2013-2014", 
    `2014` = "2013-2014", `2015` = "2015-2016", `2016` = "2015-2016", 
    `2017` = "2017-2018", `2018` = "2017-2018", `2019` = "2019-2020", 
    `2020` = "2019-2020", `2021` = "2021-2022", `2022` = "2021-2022", 
    `2023` = "2023-2024", `2024` = "2023-2024")


# Continuous NHANES table names have a letter suffix that indicates the collection interval
data_idx <-
  c(A = "1999", a = "1999", B = "2001", b = "2001", 
    C = "2003", c = "2003", D = "2005", E = "2007", 
    F = "2009", G = "2011", H = "2013", I = "2015", 
    J = "2017", K = "2019", L = "2021", M = "2023")

anomalytables2003 <- c('SSUECD_R')
anomalytables2005 <- c('CHLMD_DR', 'HSV_DR')
nchar_max <- 1024
nchar_default <- 128

#xpath <- '//*[@id="ContentPlaceHolder1_GridView1"]'
xpath <- '//*[@id="GridView1"]'

