test_that("local RDS reads preserve source values and apply explicit residence", {
  fixture <- data.frame(
    DOB_YY = rep(2024, 4),
    RESTATUS = c(1, 2, 3, 4),
    MAGER = c(20, 30, 40, 50),
    DBWT = c(3000, 9999, 2500, 3200)
  )
  path <- tempfile(fileext = ".rds")
  saveRDS(fixture, path)
  out <- read_natality(
    2024,
    vars = c("MAGER", "DBWT"),
    path = path,
    population = "us_residents"
  )
  expect_identical(names(out), c("MAGER", "DBWT", "year"))
  expect_identical(out$MAGER, c(20, 30, 40))
  expect_identical(out$DBWT, c(3000, 9999, 2500))
  expect_equal(attr(out, "natality_provenance")$input_rows, 4)
  expect_equal(attr(out, "natality_provenance")$output_rows, 3)
})

test_that("remote data are not claimed before publication", {
  entry <- natality:::.nat_annual_release(2024)
  entry$data_status <- "validated_local"
  entry$published <- FALSE
  entry$download_url <- NULL
  local_mocked_bindings(.nat_annual_release = function(year) entry)
  cache <- tempfile("empty-cache-")
  old <- options(natality.cache_dir = cache)
  on.exit({options(old); unlink(cache, recursive = TRUE)}, add = TRUE)
  expect_error(
    read_natality(2024, vars = "MAGER"),
    "has not been published"
  )
})

test_that("an empty published cache honors download FALSE", {
  entry <- natality:::.nat_annual_release(2024)
  entry$data_status <- "published"
  entry$published <- TRUE
  entry$download_url <- "https://example.test/natality_2024_source_v1.parquet"
  local_mocked_bindings(.nat_annual_release = function(year) entry)
  cache <- tempfile("empty-published-cache-")
  old <- options(natality.cache_dir = cache)
  on.exit({options(old); unlink(cache, recursive = TRUE)}, add = TRUE)
  expect_error(read_natality(2024, "MAGER", download = FALSE), "not available in the natality cache")
  expect_length(list.files(cache), 0)
})

test_that("multiple years preserve row totals, source codes and annual provenance", {
  paths <- setNames(vapply(c(2023,2024), function(year) {
    p <- tempfile(fileext = '.rds')
    saveRDS(data.frame(DOB_YY = rep(as.character(year),2),
                      RESTATUS = c('1','4'), MAGER = c('12','50')), p)
    p
  }, character(1)), c('2023','2024'))
  on.exit(unlink(paths), add = TRUE)
  expect_warning(x <- read_natality(2023:2024, 'MAGER', path = paths,
                                    population = 'us_residents'), 'comparability is not established')
  expect_equal(nrow(x),2)
  expect_identical(x$year,2023:2024)
  expect_identical(x$MAGER,c('12','12'))
  expect_identical(names(attr(x,'natality_provenance')$annual_releases),c('2023','2024'))
  expect_error(read_natality(2023:2024,'MAGER',path=unname(paths)), 'named by every requested year')
  saveRDS(data.frame(DOB_YY='2024',RESTATUS='1',MAGER=30),paths[['2024']])
  expect_error(suppressWarnings(read_natality(2023:2024,'MAGER',path=paths)), 'types or labels differ')
})

test_that("case and source-prefix collisions are never silently resolved", {
  path <- tempfile(fileext='.rds')
  on.exit(unlink(path), add=TRUE)
  saveRDS(data.frame(DOB_YY='2024', MAGER='30', source_MAGER='40'), path)
  expect_error(read_natality(2024,'MAGER',path=path),'ambiguous source column names')
})

test_that("invalid years fail before reading data", {
  for (year in list(NA_real_, Inf, 1e20, c(2024,2024), 2024.5)) {
    expect_error(read_natality(year, 'MAGER'), 'unique whole-number years')
  }
})

test_that("known transitions are surfaced even for nonadjacent requested years", {
  years <- c(2019L,2024L)
  paths <- setNames(vapply(years, function(y) {
    p <- tempfile(fileext='.rds')
    saveRDS(data.frame(DOB_YY=as.character(y),DPLURAL='4'),p)
    p
  },character(1)),as.character(years))
  on.exit(unlink(paths),add=TRUE)
  expect_warning(x <- read_natality(years,'DPLURAL',path=paths), 'DPLURAL 2019 -> 2020')
  expect_identical(x$DPLURAL,c('4','4'))
  expect_true(any(attr(x,'natality_provenance')$known_transitions$newer_year==2020))
})

test_that("unlisted source race codes are warned about and never recoded", {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add=TRUE)
  saveRDS(data.frame(DOB_YY=c("2015","2015"), RESTATUS=c("1","1"),
                     MRACE6=c("1","9"), MAGER=c("30","40")), path)
  expect_warning(x <- read_natality(2015, "MRACE6", path=path), "codes absent from the annual layout")
  expect_identical(x$MRACE6,c("1","9"))
  expect_equal(attr(x,"natality_provenance")$source_value_issues[[1]]$affected_returned_records,1)
  expect_no_warning(read_natality(2015,"MAGER",path=path))
  book <- natality_codebook("MRACE6", years=2015)
  expect_match(book$source_observations[[1]]$note,"2,923")
  expect_length(natality_codebook("MRACE6",years=2024)$source_observations,0)
})
