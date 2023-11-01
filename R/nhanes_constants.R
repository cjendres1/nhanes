# nhanes_constants.R
nhanesURL <- 'https://wwwn.cdc.gov/Nchs/Nhanes/'
dataURL <- 'https://wwwn.cdc.gov/Nchs/Nhanes/search/DataPage.aspx'
ladDataURL <- 'https://wwwn.cdc.gov/Nchs/Nhanes/search/DataPage.aspx?Component=LimitedAccess'
dxaURL  <- "https://wwwn.cdc.gov/nchs/data/nhanes/dxa/"

demoURL <- "https://wwwn.cdc.gov/nchs/nhanes/search/variablelist.aspx?Component=Demographics"
dietURL <- "https://wwwn.cdc.gov/nchs/nhanes/search/variablelist.aspx?Component=Dietary"
examURL <- "https://wwwn.cdc.gov/nchs/nhanes/search/variablelist.aspx?Component=Examination"
labURL  <- "https://wwwn.cdc.gov/nchs/nhanes/search/variablelist.aspx?Component=Laboratory"
qURL    <- "https://wwwn.cdc.gov/nchs/nhanes/search/variablelist.aspx?Component=Questionnaire"
# ladURL  <- "https://wwwn.cdc.gov/nchs/nhanes/search/variablelist.aspx?Component=LimitedAccess"
varURLs <- c(demoURL, dietURL, examURL, labURL, qURL) #, ladURL)


# Create a list of nhanes groups
# Include convenient aliases
nhanes_group <- list()
nhanes_group['DEMO']          <- "DEMOGRAPHICS"
nhanes_group['DEMOGRAPHICS']  <- "DEMOGRAPHICS"
nhanes_group['DIETARY']       <- "DIETARY"
nhanes_group['DIET']          <- "DIETARY"
nhanes_group['EXAMINATION']   <- "EXAMINATION"
nhanes_group['EXAM']          <- "EXAMINATION"
nhanes_group['LABORATORY']    <- "LABORATORY"
nhanes_group['LAB']           <- "LABORATORY"
nhanes_group['QUESTIONNAIRE'] <- "QUESTIONNAIRE"
nhanes_group['Q']             <- "QUESTIONNAIRE"
nhanes_group['LIMITED']       <- "NON-PUBLIC"
nhanes_group['LTD']           <- "NON-PUBLIC"
nhanes_survey_groups <- unlist(unique(nhanes_group))

# Although continuous NHANES is grouped in 2-year intervals,
# for convenience we want to specify using a single year
nh_years <- list()
nh_years['1999'] <- "1999-2000"
nh_years['2000'] <- "1999-2000"
nh_years['2001'] <- "2001-2002"
nh_years['2002'] <- "2001-2002"
nh_years['2003'] <- "2003-2004"
nh_years['2004'] <- "2003-2004"
nh_years['2005'] <- "2005-2006"
nh_years['2006'] <- "2005-2006"
nh_years['2007'] <- "2007-2008"
nh_years['2008'] <- "2007-2008"
nh_years['2009'] <- "2009-2010"
nh_years['2010'] <- "2009-2010"
nh_years['2011'] <- "2011-2012"
nh_years['2012'] <- "2011-2012"
nh_years['2013'] <- "2013-2014"
nh_years['2014'] <- "2013-2014"
nh_years['2015'] <- "2015-2016"
nh_years['2016'] <- "2015-2016"
nh_years['2017'] <- "2017-2018"
nh_years['2018'] <- "2017-2018"
nh_years['2019'] <- "2019-2020"
nh_years['2020'] <- "2019-2020"
nh_years['2021'] <- "2021-2022"
nh_years['2022'] <- "2021-2022"
nh_years['2023'] <- "2023-2024"
nh_years['2024'] <- "2023-2024"

# Continuous NHANES table names have a letter suffix that indicates the collection interval
data_idx <- list()
data_idx["A"] <- '1999-2000'
data_idx["a"] <- '1999-2000'
data_idx["B"] <- '2001-2002'
data_idx["b"] <- '2001-2002'
data_idx["C"] <- '2003-2004'
data_idx["c"] <- '2003-2004'
data_idx["D"] <- '2005-2006'
data_idx["E"] <- '2007-2008'
data_idx["F"] <- '2009-2010'
data_idx["G"] <- '2011-2012'
data_idx["H"] <- '2013-2014'
data_idx["I"] <- '2015-2016'
data_idx["J"] <- '2017-2018'
data_idx["K"] <- '2019-2020'
data_idx["L"] <- '2021-2022'
data_idx["M"] <- '2023-2024'

anomalytables2005 <- c('CHLMD_DR', 'SSUECD_R', 'HSV_DR')
nchar_max <- 1024
nchar_default <- 128

#xpath <- '//*[@id="ContentPlaceHolder1_GridView1"]'
xpath <- '//*[@id="GridView1"]'

