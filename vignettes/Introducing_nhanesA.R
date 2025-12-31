## ----nhanestables, eval=FALSE-------------------------------------------------
# library(nhanesA)
# nhanesTables('EXAM', 2005)

## ----nhanestables1, echo=FALSE------------------------------------------------
df <- data.frame(matrix(1,nrow=13,ncol=2))
names(df) <- c('Data.File.Name', 'Data.File.Description')
df[1,] <- list('BPX_D',                                    'Blood Pressure')
df[2,] <- list('BMX_D',                                     'Body Measures')
df[3,] <- list('AUX_D',                                        'Audiometry')
df[4,] <- list('AUXTYM_D',                         'Audiometry - Tympanometry')
df[5,] <- list('DXXFEM_D',         'Dual Energy X-ray Absorptiometry - Femur')
df[6,] <- list('OPXFDT_D',     'Ophthalmology - Frequency Doubling Technology')
df[7,] <- list('OHX_D',                                       'Oral Health')
df[8,] <- list('PAXRAW_D',                         'Physical Activity Monitor')
df[9,] <- list('VIX_D',                                            'Vision')
df[10,] <- list('DXXAG_D', 'Dual Energy X-ray Absorptiometry - Android/Gynoid')
df[11,] <- list( 'AUXAR_D',                      'Audiometry - Acoustic Reflex')
df[12,] <- list('OPXRET_D',                   'Ophthalmology - Retinal Imaging')
df[13,] <- list('DXXSPN_D',          'Dual Energy X-ray Absorptiometry - Spine')
df

## ----nhanestablevars, eval=FALSE----------------------------------------------
# nhanesTableVars('EXAM', 'BMX_D')

## ----nhanestablevars1, echo=FALSE---------------------------------------------
df <- data.frame(matrix(1,nrow=27,ncol=2))
names(df) <- c('Variable.Name', 'Variable.Description')
df[1,] <- list('BMDSTATS', 'Body Measures Component status Code')
df[2,] <- list('BMIARMC',           'Arm Circumference Comment')
df[3,] <- list('BMIARML',            'Upper Arm Length Comment')
df[4,] <- list('BMICALF',               ' Maximal Calf Comment')
df[5,] <- list('BMIHEAD',          'Head Circumference Comment')
df[6,] <- list('BMIHT',             'Standing Height Comment')
df[7,] <- list('BMILEG',            'Upper Leg Length Comment')
df[8,] <- list('BMIRECUM',            'Recumbent Length Comment')
df[9,] <- list('BMISUB',        'Subscapular Skinfold Comment')
df[10,] <- list('BMITHICR',         'Thigh Circumference Comment')
df[11,] <- list('BMITRI',            'Triceps Skinfold Comment')
df[12,] <- list('BMIWAIST',         'Waist Circumference Comment')
df[13,] <- list('BMIWT',                      'Weight Comment')
df[14,] <- list('BMXARMC',              'Arm Circumference (cm)')
df[15,] <- list('BMXARML',               'Upper Arm Length (cm)')
df[16,] <- list('BMXBMI',           'Body Mass Index (kg/m**2)')
df[17,] <- list('BMXCALF',     'Maximal Calf Circumference (cm)')
df[18,] <- list('BMXHEAD',             'Head Circumference (cm)')
df[19,] <- list('BMXHT',                'Standing Height (cm)')
df[20,] <- list('BMXLEG',               'Upper Leg Length (cm)')
df[21,] <- list('BMXRECUM',               'Recumbent Length (cm)')
df[22,] <- list('BMXSUB',           'Subscapular Skinfold (mm)')
df[23,] <- list('BMXTHICR',            'Thigh Circumference (cm)')
df[24,] <- list('BMXTRI',               'Triceps Skinfold (mm)')
df[25,] <- list('BMXWAIST',            'Waist Circumference (cm)')
df[26,] <- list('BMXWT',                         'Weight (kg)')
df[27,] <- list('SEQN',         'Respondent sequence number.')
df

## ----nhanes, eval=FALSE-------------------------------------------------------
# bmx_d  <- nhanes('BMX_D')
# demo_d <- nhanes('DEMO_D')

## ----bmd1, eval=FALSE---------------------------------------------------------
# bmx_demo <- merge(demo_d, bmx_d)
# options(digits=4)
# select_cols <- c('RIAGENDR', 'BMXHT', 'BMXWT', 'BMXLEG', 'BMXCALF', 'BMXTHICR')
# print(bmx_demo[5:8,select_cols], row.names=FALSE)

## ----bmx4, echo=FALSE---------------------------------------------------------
df <- data.frame(matrix(1,nrow=4,ncol=6))
names(df) <- c('RIAGENDR', 'BMXHT', 'BMXWT', 'BMXLEG', 'BMXCALF', 'BMXTHICR')
df[1,] <- list('Female', 156.0, 75.2, 38.0, 36.6, 53.7)
df[2,] <- list('Male', 167.6,  69.5,   40.4,    35.6, 48.0)
df[3,] <- list('Female', 163.7,  45.0,   39.2,    31.7,     41.3)
df[4,] <- list('Male', 182.4, 101.9,   41.5,    42.6,     50.5)

print(df,row.names=FALSE)

## ----nhanescodebook, eval=FALSE-----------------------------------------------
# nhanesCodebook('DEMO_D', 'RIAGENDR')

## ----translate1, echo=FALSE---------------------------------------------------
df <- data.frame(matrix(1,nrow=3,ncol=5))
names(df) <- c("Code.or.Value", "Value.Description", "Count", "Cumulative", "Skip to Item")
df[1,] <- list(1, 'Male', 5080, 5080, NA)
df[2,] <- list(2, 'Female', 5268, 10348, NA)
df[3,] <- list('.', 'Missing', 0, 10348, NA)

codelist <- list("RIAGENDR", "Gender", "Gender of the sample person", 
                 "Both males and females 0 YEARS -\r 150 YEARS", df)
names(codelist) <- c('Variable Name', 'SAS Label', 'English Text', 'Target', 'RIAGENDR')

codelist

## ----nhanestranslate1, eval=FALSE---------------------------------------------
# bpx_d <- nhanes('BPX_D', translate=FALSE)
# head(bpx_d[,6:11])

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
# bpx_d_vars  <- nhanesTableVars('EXAM', 'BPX_D', namesonly=TRUE)
# #Alternatively may use bpx_d_vars = names(bpx_d)
# bpx_d <- nhanesTranslate('BPX_D', bpx_d_vars, data=bpx_d)

## ----simpletranslate2, echo=FALSE---------------------------------------------
translated <- c('BPAARM', 'BPACSZ', 'BPAEN2', 'BPAEN3', 'BPAEN4', 'BPQ150A', 'BPQ150B', 'BPQ150C', 'BPQ150D', 'BPXPTY', 'BPXPULS', 'PEASCCT1', 'PEASCST1')
message(paste(c("Translated columns:", translated), collapse = ' '))

## ----nhanestranslate3, eval=FALSE---------------------------------------------
# head(bpx_d[,6:11])

## ----simpletranslate3, echo=FALSE---------------------------------------------
df$BPAARM[df$BPAARM==1] <- 'Right'
df[df==1] <- 'Yes'
df[df==2] <- 'No'
df[df==3] <- 'Adult (12X22)'
df[df==4] <- 'Large (15X32)'
df

## ----nhaneslapplytables, eval=FALSE-------------------------------------------
# q2007names  <- nhanesTables('Q', 2007, namesonly=TRUE)
# q2007tables <- lapply(q2007names, nhanes)
# names(q2007tables) <- q2007names

## ----prepan, eval=FALSE-------------------------------------------------------
# #List all pre-pandemic tables
# nhanesSearchTableNames('^P_')
# #List table variables
# nhanesTableVars('EXAM', 'P_AUX', namesonly=TRUE)
# #List pre-pandemic EXAM tables
# nhanesTables('EXAM', 'P')
# #Table import, variable translation, and codebook display operate as usual
# p_dxxfem <- nhanes('P_DXXFEM')
# nhanesTranslate('P_BMX', 'BMDSTATS')
# nhanesCodebook('P_INS', 'LBDINSI')

## ----nhanesdxa, eval=FALSE----------------------------------------------------
# #Import into R
# dxx_b <- nhanesDXA(2001)
# #Save to file
# nhanesDXA(2001, destfile="dxx_b.xpt")
# #Import supplemental data
# dxx_c_s <- nhanesDXA(2003, suppl=TRUE)
# #Apply code translations
# dxalist <- c('DXAEXSTS', 'DXIHE')
# dxx_b <- nhanesTranslate("dxxb",colnames=dxalist, data=dxx_b, dxa=TRUE)

## ----nnyfs, eval=FALSE--------------------------------------------------------
# #List NNYFS EXAM tables
# nhanesTables('EXAM', 'Y')
# #Table import and variable translation operate as usual
# y_cvx <- nhanes('Y_CVX')
# nhanesTranslate('Y_CVX','CVXPARC')

## ----nhanessearch, eval=FALSE-------------------------------------------------
# # nhanesSearch use examples
# #
# # Search on the word bladder, restrict to the 2001-2008 surveys,
# # print out 50 characters of the variable description
# nhanesSearch("bladder", ystart=2001, ystop=2008, nchar=50)
# #
# # Search on "urin" (will match urine, urinary, etc), from 1999-2010, return table names only
# nhanesSearch("urin", ignore.case=TRUE, ystop=2010, namesonly=TRUE)
# #
# # Search on "urin", exclude "During", search surveys from 1999-2010, return table names only
# nhanesSearch("urin", exclude_terms="during", ignore.case=TRUE, ystop=2010, namesonly=TRUE)
# #
# # Restrict search to 'EXAM' and 'LAB' data groups. Explicitly list matching and exclude terms, leave ignore.case set to default value of FALSE. Search surveys from 2009 to present.
# nhanesSearch(c("urin", "Urin"), exclude_terms=c("During", "eaten during", "do during"), data_group=c('EXAM', 'LAB'), ystart=2009)
# #
# # Search on "tooth" or "teeth", all years
# nhanesSearch(c("tooth", "teeth"), ignore.case=TRUE)
# #
# # Search for variables where the variable description begins with "Tooth"
# nhanesSearch("^Tooth")

## ----nhanessearchvarname1, eval=FALSE-----------------------------------------
# #nhanesSearchVarName use examples
# nhanesSearchVarName('BPXPULS')

## ----nhanessearchvarname2, echo=FALSE-----------------------------------------
bpxtables <- c("BPX_D", "BPX_E", "BPX",   "BPX_C", "BPX_B", "BPX_F", "BPX_G", "BPX_H", "BPX_I", "BPX_J")
bpxtables

## ----nhanessearchvarname3, eval=FALSE-----------------------------------------
# nhanesSearchVarName('CSQ260i', includerdc=TRUE, nchar=38, namesonly=FALSE)

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
# # nhanesSearchTableNames use examples
# nhanesSearchTableNames('BMX')

## ----nhanessearchtablename2, echo=FALSE---------------------------------------
bpxtables <- c("BMX_D", "BMX",   "BMX_E", "BMX_C", "BMX_B", "BMX_F", "BMX_H", "BMX_G", "BMX_I", "BMX_J", "P_BMX")
bpxtables

## ----nhanessearchtablenames3, eval=FALSE--------------------------------------
# nhanesSearchTableNames('HPVS', includerdc=TRUE, nchar=42, details=TRUE)

## ----nhanessearchtablenames4, echo=FALSE--------------------------------------
df <- data.frame(
  Years=character(),
#  Data.File.Name=character(),
  Doc.File=character(),
  Data.File=character(),
  Date.Published=character())
df[1,] <- list('2009-2010', 'HPVSER_F Doc', 'HPVSER_F Data [XPT - 171.6 KB]', 'November 2013')
df[2,] <- list('2007-2008', 'HPVSER_E Doc', 'HPVSER_E Data [XPT - 155.7 KB]', 'November 2013')
df[3,] <- list('2005-2006', 'HPVSER_D Doc', 'HPVSER_D Data [XPT - 151.6 KB]', 'July 2013')
df[4,] <- list('2005-2006', 'HPVSRM_D Doc', 'HPVSRM_D Data [XPT - 302.6 KB]', 'January 2015')
df[5,] <- list('2007-2008', 'HPVSWR_E Doc', 'HPVSWR_E Data [XPT - 677.9 KB]', 'August 2012')
df[6,] <- list('2009-2010', 'HPVSWR_F Doc', 'HPVSWR_F Data [XPT - 725.2 KB]', 'August 2012')
df[7,] <- list('2011-2012', 'HPVSWR_G Doc', 'HPVSWR_G Data [XPT - 661.1 KB]', 'March 2015')
df[8,] <- list('2005-2006', 'HPVSWR_D Doc', 'HPVSWR_D Data [XPT - 694.4 KB]', 'Updated November 2018')
df[9,] <- list('2013-2014', 'HPVSWR_H Doc', 'HPVSWR_H Data [XPT - 716.6 KB]', 'December 2016')
df[10,] <- list('2015-2016', 'HPVSWC_I Doc', 'HPVSWC_I Data [XPT - 33.3 KB]', 'November 2018')
df[11,] <- list('2015-2016', 'HPVSWR_I Doc', 'HPVSWR_I Data [XPT - 667.5 KB]', 'November 2018')
df[12,] <- list('2005-2006', 'HPVS_D_R Doc', 'RDC Only', 'July 2013')
df[13,] <- list('2009-2010', 'HPVS_F_R Doc', 'RDC Only', 'August 2012')
df[14,] <- list('2011-2012', 'HPVS_G_R Doc', 'RDC Only', 'March 2015')
df[15,] <- list('2013-2014', 'HPVS_H_R Doc', 'RDC Only', 'December 2016')
df[16,] <- list('2015-2016', 'HPVS_I_R Doc', 'RDC Only', 'November 2018')
df[17,] <- list('2017-2018', 'HPVS_J_R Doc', 'RDC Only', 'December 2020')
df

