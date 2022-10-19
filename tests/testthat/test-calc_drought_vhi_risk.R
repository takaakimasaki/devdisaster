testthat::test_that("list of PTI indicators can be extracted", {
  expect_equal({
    dim(devdisaster::calc_drought_vhi_risk(devdisaster::sf,start_year=2001,end_year=2001,threshold=40)[2]
    )
  }, 65)
})

testthat::test_that("list of PTI indicators can be extracted", {
  expect_equal({
    dim(
      devdisaster::calc_drought_vhi_risk(devdisaster::sf,start_year=2001,end_year=2001,threshold=40,pop=devdisaster::crop,pop_wt=TRUE)[2]
    )
  }, 65)
})
