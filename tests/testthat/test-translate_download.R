nhanesA::nhanesOptions(log.access = TRUE)
testthat::test_that("nhanesTranslate: downloading a file works", {
  testthat::skip_if_offline()
  tfile = tempfile(fileext = ".htm")
  res = nhanesA::nhanesTranslate("DEMO_G", file = tfile)
  testthat::expect_true(file.exists(tfile))
  testthat::expect_message({
    res2 = nhanesA::nhanesTranslate("DEMO_G", file = tfile)
  }, regexp = paste0("Downloading.*", basename(tfile)))
})

testthat::test_that("nhanesTranslate: using an existing file works", {
  test_file = system.file("DEMO_G.htm", package = "nhanesA")
  # give garbage table but still read the file
  testthat::expect_message({
    res = nhanesA::nhanesTranslate("asdf", file = test_file)
  }, regexp = paste0("Downloading.*", basename(test_file)))
  testthat::expect_named(
    res,
    c("SDDSRVYR", "RIDSTATR", "RIAGENDR", "RIDAGEYR", "RIDAGEMN", 
      "RIDRETH1", "RIDRETH3", "RIDEXMON", "RIDEXAGY", "RIDEXAGM", "DMQMILIZ", 
      "DMQADFC", "DMDBORN4", "DMDCITZN", "DMDYRSUS", "DMDEDUC3", "DMDEDUC2", 
      "DMDMARTL", "RIDEXPRG", "SIALANG", "SIAPROXY", "SIAINTRP", "FIALANG", 
      "FIAPROXY", "FIAINTRP", "MIALANG", "MIAPROXY", "MIAINTRP", "AIALANGA", 
      "WTINT2YR", "WTMEC2YR", "SDMVPSU", "SDMVSTRA", "INDHHIN2", "INDFMIN2", 
      "INDFMPIR", "DMDHHSIZ", "DMDFMSIZ", "DMDHHSZA", "DMDHHSZB", "DMDHHSZE", 
      "DMDHRGND", "DMDHRAGE", "DMDHRBR4", "DMDHREDU", "DMDHRMAR", "DMDHSEDU"
    )
  )
})
