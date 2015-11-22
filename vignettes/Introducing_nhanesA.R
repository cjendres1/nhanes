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
aggregate(cbind(BMXHT,BMXWT, BMXLEG, BMXCALF, BMXTHICR)~RIAGENDR, bmx_demo, mean)

## ------------------------------------------------------------------------
nhanesTranslate('DEMO_D', 'RIAGENDR')

## ------------------------------------------------------------------------
levels(as.factor(demo_d$RIAGENDR))
demo_d <- nhanesTranslate('DEMO_D', 'RIAGENDR', data=demo_d)
levels(demo_d$RIAGENDR)
bmx_demo <- merge(demo_d, bmx_d)
aggregate(cbind(BMXHT, BMXWT, BMXLEG, BMXCALF, BMXTHICR)~RIAGENDR, bmx_demo, mean)

## ----eval=FALSE----------------------------------------------------------
#  q2007names  <- nhanesTables('Q', 2007, namesonly=TRUE)
#  q2007tables <- lapply(q2007names, nhanes)
#  names(q2007tables) <- q2007names

## ------------------------------------------------------------------------
bpx_d <- nhanes('BPX_D')
head(bpx_d[,6:11])
bpx_d_vars  <- nhanesTableVars('EXAM', 'BPX_D', namesonly=TRUE)
#Alternatively may use bpx_d_vars = names(bpx_d)
bpx_d <- suppressWarnings(nhanesTranslate('BPX_D', bpx_d_vars, data=bpx_d))
head(bpx_d[,6:11])

## ---- eval=FALSE---------------------------------------------------------
#  #Importing into R
#  dxx_b <- nhanesDXA(2001)
#  #Save to file
#  nhanesDXA(2001, destfile="dxx_b.xpt")
#  #Import supplemental data
#  dxx_c_s <- nhanesDXA(2003, suppl=TRUE)

