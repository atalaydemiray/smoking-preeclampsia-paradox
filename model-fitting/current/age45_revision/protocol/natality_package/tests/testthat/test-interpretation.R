test_that("paternity coverage is not relabeled as marital-status coverage", {
  path <- tempfile(fileext = '.rds')
  on.exit(unlink(path), add = TRUE)
  fixture <- data.frame(DOB_YY=c('2024','2024'),DMAR=c('1',' '),F_MAR_P=c('0','1'))
  saveRDS(fixture,path)
  expect_warning(x <- read_natality(2024,c('DMAR','F_MAR_P'),path=path),
                 'paternity-acknowledgment coverage')
  expect_identical(x$DMAR,fixture$DMAR)
  expect_identical(x$F_MAR_P,fixture$F_MAR_P)
  expect_length(attr(x,'natality_provenance')$interpretation_notes,2L)
})

test_that("marital coverage warning respects the documented boundary", {
  expect_silent(natality:::.nat_warn_interpretation(2016,'DMAR'))
  expect_warning(natality:::.nat_warn_interpretation(2017,'DMAR'),'California')
  expect_silent(natality:::.nat_warn_interpretation(2024,'MAGER'))
  expect_warning(natality:::.nat_warn_interpretation(2014,'F_MAR_P'),'not a validated marital-status')
})
