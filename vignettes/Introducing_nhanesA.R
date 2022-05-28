## ----nhanestables-------------------------------------------------------------
library(nhanesA)
nhanesTables('EXAM', 2005)

## ----nhanestablevars----------------------------------------------------------
nhanesTableVars('EXAM', 'BMX_D')

## ----nhanes-------------------------------------------------------------------
bmx_d  <- nhanes('BMX_D')
demo_d <- nhanes('DEMO_D')

## ----bmd1---------------------------------------------------------------------
bmx_demo <- merge(demo_d, bmx_d)
options(digits=4)
select_cols <- c('RIAGENDR', 'BMXHT', 'BMXWT', 'BMXLEG', 'BMXCALF', 'BMXTHICR')
print(bmx_demo[5:8,select_cols], row.names=FALSE)

## ----nhanestranslate----------------------------------------------------------
nhanesTranslate('DEMO_D', 'RIAGENDR')

## ----bmx1, eval=FALSE---------------------------------------------------------
#  demo_d <- nhanesTranslate('DEMO_D', 'RIAGENDR', data=demo_d)

## ----bmx_m1, echo=FALSE-------------------------------------------------------
message("Translated columns: RIAGENDR")

## ----bmx2, eval=FALSE---------------------------------------------------------
#  bmx_demo <- merge(demo_d, bmx_d)

## ----bmx3, echo=FALSE---------------------------------------------------------
bmx_demo$RIAGENDR[bmx_demo$RIAGENDR==1] <- 'Male'
bmx_demo$RIAGENDR[bmx_demo$RIAGENDR==2] <- 'Female'

## ----bmx_final_result---------------------------------------------------------
print(bmx_demo[5:8,select_cols], row.names=FALSE)

## ----nhanestranslate1, eval=FALSE---------------------------------------------
#  bpx_d <- nhanes('BPX_D')
#  head(bpx_d[,6:11])

## ----simpletranslate1, echo=FALSE---------------------------------------------
df <- data.frame(matrix(1,nrow=6,ncol=6))
names(df) <- c("BPQ150A", "BPQ150B", "BPQ150C", "BPQ150D", "BPAARM",  "BPACSZ")
df[2:6,1:4] <- 2
df[3,1] <- 1
df[3:6,6] <- 4
df[2,6] <- 3
df[4,6] <- 3
df[1,] <- NA
df

## ----nhanestranslate2, eval=FALSE---------------------------------------------
#  bpx_d_vars  <- nhanesTableVars('EXAM', 'BPX_D', namesonly=TRUE)
#  #Alternatively may use bpx_d_vars = names(bpx_d)
#  bpx_d <- suppressWarnings(nhanesTranslate('BPX_D', bpx_d_vars, data=bpx_d))

## ----simpletranslate2, echo=FALSE---------------------------------------------
translated <- c('BPAARM', 'BPACSZ', 'BPAEN2', 'BPAEN3', 'BPAEN4', 'BPQ150A', 'BPQ150B', 'BPQ150C', 'BPQ150D', 'BPXPTY', 'BPXPULS', 'PEASCCT1', 'PEASCST1')
message(paste(c("Translated columns:", translated), collapse = ' '))

## ----nhanestranslate3, eval=FALSE---------------------------------------------
#  head(bpx_d[,6:11])

## ----simpletranslate3, echo=FALSE---------------------------------------------
df$BPAARM[df$BPAARM==1] <- 'Right'
df[df==1] <- 'Yes'
df[df==2] <- 'No'
df[df==3] <- 'Adult (12X22)'
df[df==4] <- 'Large (15X32)'
df

## ----nhaneslapplytables, eval=FALSE-------------------------------------------
#  q2007names  <- nhanesTables('Q', 2007, namesonly=TRUE)
#  q2007tables <- lapply(q2007names, nhanes)
#  names(q2007tables) <- q2007names

## ----prepan, eval=FALSE-------------------------------------------------------
#  #List all pre-pandemic tables
#  nhanesSearchTableNames('^P_')
#  #List pre-pandemic EXAM tables
#  nhanesTables('EXAM', 'P')
#  #Table import and variable translation operate as usual
#  p_dxxfem <- nhanes('P_DXXFEM')
#  nhanesTranslate('P_BMX', 'BMDSTATS')

## ----nhanesdxa, eval=FALSE----------------------------------------------------
#  #Import into R
#  dxx_b <- nhanesDXA(2001)
#  #Save to file
#  nhanesDXA(2001, destfile="dxx_b.xpt")
#  #Import supplemental data
#  dxx_c_s <- nhanesDXA(2003, suppl=TRUE)
#  #Apply code translations
#  dxalist <- c('DXAEXSTS', 'DXIHE')
#  dxx_b <- nhanesTranslate(colnames=dxalist, data=dxx_b, dxa=TRUE)

## ----nnyfs, eval=FALSE--------------------------------------------------------
#  #List NNYFS EXAM tables
#  nhanesTables('EXAM', 'Y')
#  #Table import and variable translation operate as usual
#  y_cvx <- nhanes('Y_CVX')
#  nhanesTranslate('Y_CVX','CVXPARC')

## ----nhanessearch, eval=FALSE-------------------------------------------------
#  # nhanesSearch use examples
#  #
#  # Search on the word bladder, restrict to the 2001-2008 surveys,
#  # print out 50 characters of the variable description
#  nhanesSearch("bladder", ystart=2001, ystop=2008, nchar=50)
#  #
#  # Search on "urin" (will match urine, urinary, etc), from 1999-2010, return table names only
#  nhanesSearch("urin", ignore.case=TRUE, ystop=2010, namesonly=TRUE)
#  #
#  # Search on "urin", exclude "During", search surveys from 1999-2010, return table names only
#  nhanesSearch("urin", exclude_terms="during", ignore.case=TRUE, ystop=2010, namesonly=TRUE)
#  #
#  # Restrict search to 'EXAM' and 'LAB' data groups. Explicitly list matching and exclude terms, leave ignore.case set to default value of FALSE. Search surveys from 2009 to present.
#  nhanesSearch(c("urin", "Urin"), exclude_terms=c("During", "eaten during", "do during"), data_group=c('EXAM', 'LAB'), ystart=2009)
#  #
#  # Search on "tooth" or "teeth", all years
#  nhanesSearch(c("tooth", "teeth"), ignore.case=TRUE)
#  #
#  # Search for variables where the variable description begins with "Tooth"
#  nhanesSearch("^Tooth")

## ----nhanessearchvarname1, eval=FALSE-----------------------------------------
#  #nhanesSearchVarName use examples
#  nhanesSearchVarName('BPXPULS')

## ----nhanessearchvarname2, echo=FALSE-----------------------------------------
bpxtables <- c("BPX_D", "BPX_E", "BPX",   "BPX_C", "BPX_B", "BPX_F", "BPX_G", "BPX_H", "BPX_I", "BPX_J")
bpxtables

## ----nhanessearchvarname3, eval=FALSE-----------------------------------------
#  nhanesSearchVarName('CSQ260i', includerdc=TRUE, nchar=38, namesonly=FALSE)

## ----nhanessearchvarname4, echo=FALSE-----------------------------------------
df <- data.frame(Variable.Name=character(2),
                 Variable.Description=character(2),
                 Data.File.Name=character(2),
                 Data.File.Description=character(2),
                 Begin.Year=integer(2),
                 EndYear=integer(2),
                 Component=character(2),
                 Use.Constraints=character(2))
df[1,] <- list('CSQ260i', 'Do you now have any of the following p','CSX_G_R','Taste & Smell',
               2012,2012,'Examination', 'RDC Only')
df[2,] <- list('CSQ260i', 'Do you now have any of the following p','CSX_H','Taste & Smell',
                2013,    2014, 'Examination',            'None')
df

## ----nhanessearchtablenames1, eval=FALSE--------------------------------------
#  # nhanesSearchTableNames use examples
#  nhanesSearchTableNames('BMX')

## ----nhanessearchtablename2, echo=FALSE---------------------------------------
bpxtables <- c("BMX_D", "BMX",   "BMX_E", "BMX_C", "BMX_B", "BMX_F", "BMX_H", "BMX_G", "BMX_I", "BMX_J", "P_BMX")
bpxtables

## ----nhanessearchtablenames3, eval=FALSE--------------------------------------
#  nhanesSearchTableNames('HPVS', includerdc=TRUE, nchar=42, details=TRUE)

## ----nhanessearchtablenames4, echo=FALSE--------------------------------------
df <- data.frame(
  Years=character(),
  Data.File.Name=character(),
  Doc.File=character(),
  Data.File=character(),
  Date.Published=character())
df[1,] <- list('2009-2010', 'Human Papillomavirus (HPV) - 6, 11, 16 & 1', 'HPVSER_F Doc', 'HPVSER_F Data [XPT - 171.6 KB]','November 2013')
df[2,] <- list('2005-2006', 'Human Papillomavirus (HPV) - 6, 11, 16 & 1', 'HPVS_D_R Doc', 'RDC Only', 'July 2013')
df[3,] <- list('2007-2008', 'Human Papillomavirus (HPV) - 6, 11, 16 & 1', 'HPVSER_E Doc', 'HPVSER_E Data [XPT - 155.7 KB]         November 2013')
df[4,] <- list('2005-2006', 'Human Papillomavirus (HPV) - 6, 11, 16 & 1', 'HPVSER_D Doc', 'HPVSER_D Data [XPT - 151.6 KB]             July 2013')
df[5,] <- list('2005-2006', 'Human Papillomavirus (HPV) - Multiplexed 6', 'HPVSRM_D Doc', 'HPVSRM_D Data [XPT - 302.6 KB]          January 2015')
df[6,] <- list('2005-2006', 'Human Papillomavirus (HPV) DNA - Vaginal S', 'HPVSWR_D Doc', 'HPVSWR_D Data [XPT - 694.4 KB]         November 2010')
df[7,] <- list('2007-2008', 'Human Papillomavirus (HPV) DNA - Vaginal S', 'HPVSWR_E Doc', 'HPVSWR_E Data [XPT - 677.9 KB]           August 2012')
df[8,] <- list('2009-2010', 'Human Papillomavirus (HPV) DNA - Vaginal S', 'HPVSWR_F Doc', 'HPVSWR_F Data [XPT - 725.2 KB]           August 2012')
df[9,] <- list('2011-2012', 'Human Papillomavirus (HPV) DNA - Vaginal S', 'HPVSWR_G Doc', 'HPVSWR_G Data [XPT - 661.1 KB]            March 2015')
df[10,] <- list('2005-2006', 'Human Papillomavirus (HPV) DNA - Vaginal S', 'HPVSWR_D Doc', 'HPVSWR_D Data [XPT - 694.4 KB] Updated November 2018')
df[11,] <- list('2009-2010', 'Human Papillomavirus (HPV) DNA - Vaginal S', 'HPVS_F_R Doc', 'RDC Only', 'August 2012')
df[12,] <- list('2011-2012', 'Human Papillomavirus (HPV) DNA - Vaginal S', 'HPVS_G_R Doc', 'RDC Only', 'March 2015')
df[13,] <- list('2013-2014', 'Human Papillomavirus (HPV) DNA - Vaginal S', 'HPVSWR_H Doc', 'HPVSWR_H Data [XPT - 716.6 KB]         December 2016')
df[14,] <- list('2013-2014', 'Human Papillomavirus (HPV) DNA - Vaginal S', 'HPVS_H_R Doc', 'RDC Only', 'December 2016')
df[15,] <- list('2015-2016', 'Human Papillomavirus (HPV) DNA - Vaginal S', 'HPVSWC_I Doc', 'HPVSWC_I Data [XPT - 33.3 KB]         November 2018')
df[16,] <- list('2015-2016', 'Human Papillomavirus (HPV) DNA - Vaginal S', 'HPVSWR_I Doc', 'HPVSWR_I Data [XPT - 667.5 KB]         November 2018')
df[17,] <- list('2015-2016', 'Human Papillomavirus (HPV) DNA - Vaginal S', 'HPVS_I_R Doc', 'RDC Only', 'November 2018')
df[18,] <- list('2017-2018', 'Human Papillomavirus (HPV) DNA - Vaginal S', 'HPVS_J_R Doc', 'RDC Only', 'December 2020')
df

