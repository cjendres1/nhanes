## ------------------------------------------------------------------------
suppressWarnings(library(nhanesA))
nhanesTables('EXAM', 2005)

## ------------------------------------------------------------------------
nhanesTableVars('EXAM', 'BMX_D')

## ------------------------------------------------------------------------
bmx_d  <- nhanes('BMX_D')
demo_d <- nhanes('DEMO_D')

## ------------------------------------------------------------------------
bmx_demo <- merge(demo_d, bmx_d)
options(digits=4)
aggregate(cbind(BMXHT, BMXWT, BMXLEG, BMXCALF, BMXTHICR)~RIAGENDR, bmx_demo, mean)

## ------------------------------------------------------------------------
nhanesTranslate('DEMO_D', 'RIAGENDR')

## ------------------------------------------------------------------------
levels(as.factor(demo_d$RIAGENDR))
demo_d <- nhanesTranslate('DEMO_D', 'RIAGENDR', data=demo_d)
levels(demo_d$RIAGENDR)
bmx_demo <- merge(demo_d, bmx_d)
aggregate(cbind(BMXHT, BMXWT, BMXLEG, BMXCALF, BMXTHICR)~RIAGENDR, bmx_demo, mean)

## ------------------------------------------------------------------------
bpx_d <- nhanes('BPX_D')
head(bpx_d[,6:11])
bpx_d_vars  <- nhanesTableVars('EXAM', 'BPX_D', namesonly=TRUE)
#Alternatively may use bpx_d_vars = names(bpx_d)
bpx_d <- suppressWarnings(nhanesTranslate('BPX_D', bpx_d_vars, data=bpx_d))
head(bpx_d[,6:11])

## ----eval=FALSE----------------------------------------------------------
#  q2007names  <- nhanesTables('Q', 2007, namesonly=TRUE)
#  q2007tables <- lapply(q2007names, nhanes)
#  names(q2007tables) <- q2007names

## ---- eval=FALSE---------------------------------------------------------
#  #Import into R
#  dxx_b <- nhanesDXA(2001)
#  #Save to file
#  nhanesDXA(2001, destfile="dxx_b.xpt")
#  #Import supplemental data
#  dxx_c_s <- nhanesDXA(2003, suppl=TRUE)
#  #Apply code translations
#  dxalist <- c('DXAEXSTS', 'DXITOT', 'DXIHE')
#  dxx_b <- nhanesTranslate(colnames=dxalist, data=dxx_b, dxa=TRUE)

## ---- eval=FALSE---------------------------------------------------------
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
#  nhanesSearch(c("urin", "Urin"), exclude_terms=c("During", "eaten during", "do during"), data_group=c( 'EXAM', 'LAB'), ystart=2009)
#  #
#  # Search on "tooth" or "teeth", all years
#  nhanesSearch(c("tooth", "teeth"), ignore.case=TRUE)
#  #
#  # Search for variables where the variable description begins with "Tooth"
#  nhanesSearch("^Tooth")

