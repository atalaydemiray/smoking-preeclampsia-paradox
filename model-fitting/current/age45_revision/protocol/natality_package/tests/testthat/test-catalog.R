test_that("annual metadata are explicit", {
  expect_identical(natality_years(), 2014:2024)
  details <- natality_years(details = TRUE)
  expect_equal(nrow(details), 11)
  expect_true(all(details$metadata_status == "available"))
  expect_identical(details$data_status[details$year == 2024], "published")
  expect_identical(details$data_status[details$year == 2023], "published")
  expect_true(all(details$data_status %in% c("not_built", "validated_local", "published")))
})

test_that("variable searches and codebooks use annual source evidence", {
  vars <- natality_vars("gestation", years = 2024)
  expect_true(any(tolower(vars$local_name) == "oegest_comb"))
  book <- natality_codebook("RF_PPTERM", years = c(2014, 2024))
  expect_s3_class(book, "natality_codebook")
  expect_identical(sort(book$years$year), c(2014L, 2024L))
  expect_true(all(grepl("Previous Preterm Birth", book$years$rules_text)))
})

test_that("change history retains important differences", {
  changes <- natality_changes("DPLURAL", years = 2014:2024)
  expect_s3_class(changes, "natality_changes")
  expect_true(any(grepl("quadruplet", changes$notes$note, ignore.case = TRUE)))
  expect_identical(changes$comparability, "not_established")
})

test_that("codebooks distinguish printed widths from imported storage", {
  book <- natality_codebook('MRACE6', years=2022)$years
  expect_equal(book$printed_width, 2)
  expect_equal(book$position_span, 1)
  expect_true(book$layout_conflict)
  expect_identical(book$observed_conversion_storage, 'double')
})

test_that("source discovery includes native names but excludes territory-only geography", {
  expect_false(any(grepl('OCTERR|OCNTYFIPS|RCNTY', natality_vars(years=2017)$variable)))
  fields <- natality_vars(years=2017)$variable
  expect_true(all(c('IP_HEPB','IP_HEPC','F_AB_NIUC','CA_DOWN') %in% fields))
  expect_equal(nrow(natality_vars(years=2024)),237)
  expect_equal(nrow(natality_vars(years=2017)),226)
  book <- natality_codebook('F_AB_NIUC',2017)$years
  expect_equal(nrow(book),1)
  expect_identical(book$source_field,'F_AB_NIUC')
  expect_error(natality_codebook('OCTERR',2017),'not documented')
})
