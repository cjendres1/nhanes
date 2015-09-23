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
aggregate(cbind(bmxht,bmxwt, bmxleg, bmxcalf, bmxthicr)~riagendr, bmx_demo, mean)

## ------------------------------------------------------------------------
nhanesTranslate('DEMO_D', 'RIAGENDR')

## ------------------------------------------------------------------------
levels(as.factor(demo_d$riagendr))
demo_d <- nhanesTranslate('DEMO_D', 'RIAGENDR', data=demo_d)
levels(demo_d$riagendr)
bmx_demo <- merge(demo_d, bmx_d)
aggregate(cbind(bmxht,bmxwt, bmxleg, bmxcalf, bmxthicr)~riagendr, bmx_demo, mean)

## ----eval=FALSE----------------------------------------------------------
#  q2007names  <- nhanesTables('Q', 2007, namesonly=TRUE)
#  q2007tables <- lapply(q2007names, nhanes)
#  names(q2007tables) <- q2007names

## ------------------------------------------------------------------------
bpx_d <- nhanes('BPX_D')
head(bpx_d[,6:11])
bpx_d_vars  <- nhanesTableVars('EXAM', 'BPX_D', namesonly=TRUE)
bpx_d <- suppressWarnings(nhanesTranslate('BPX_D', bpx_d_vars, data=bpx_d))
head(bpx_d[,6:11])

