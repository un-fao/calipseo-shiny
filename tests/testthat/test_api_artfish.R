library(testthat)
source("../../global.R")

test_that("Artfish API for landings returns same output as DBI",{
  landings_from_dbi = accessArtfishDFromDB(pool)
  landings_from_api = accessArtfishDFromAPI()
  testthat::expect_equal(nrow(landings_from_dbi), nrow(landings_from_api))
  testthat::expect_equal(names(landings_from_dbi), names(landings_from_api))
  #note warnings with waldo::compare(landings_from_dbi, landings_from_api differen datatypes
})