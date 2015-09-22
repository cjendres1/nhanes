## ------------------------------------------------------------------------
suppressWarnings(library(nhanesA))
nhanesTables('EXAM', 2005)

## ------------------------------------------------------------------------
nhanesTableVars('EXAM', 'BMX_D')

## ------------------------------------------------------------------------
bmx_d  <- nhanes('BMX_D')
demo_d <- nhanes('DEMO_D')

## ------------------------------------------------------------------------
bmx <- merge(demo_d, bmx_d)

## ------------------------------------------------------------------------
nhanesTranslate('DEMO_D', 'RIAGENDR')

## ---- eval=FALSE---------------------------------------------------------
#  levels(demo_b$RIAGENDR)
#  demo_b <- nhanesTranslate('DEMO_D', 'RIAGENDR', data=demo_b)
#  levels(demo_b$RIAGENDR)

## ----eval=FALSE----------------------------------------------------------
#  q2007names  <- nhanesTables('Q', 2007, namesonly=TRUE)
#  q2007tables <- lapply(q2007names, nhanes)
#  names(q2007tables) <- q2007names

## ----eval=FALSE----------------------------------------------------------
#  bpx_d <- nhanes('BPX_D')
#  head(bpx_d)
#  bpx_d_vars  <- nhanesTableVars('EXAM', 'BPX_D', namesonly=TRUE)
#  bpx_d <- nhanesTranslate('BPX_D', bpx_d_vars, data=bpx_d)
#  head(bpx_d)

