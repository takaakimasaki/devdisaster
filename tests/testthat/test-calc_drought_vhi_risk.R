testthat::test_that("Check if the baseline model runs.", {
  expect_equal({
    vhi <- devdisaster::calc_drought_vhi_risk(devdisaster::sf,start_year=2001,end_year=2001,threshold=40)
    dim(vhi)[2]
    )
  }, 65)
})

testthat::test_that("Add cropland and population weights", {
  expect_equal({
      vhi <- devdisaster::calc_drought_vhi_risk(devdisaster::sf,start_year=2021,end_year=2021,threshold=40,ag=devdisaster::cropland, pop=devdisaster::crop_val,ag_wt=TRUE,pop_wt=TRUE)
      dim(vhi)[2]
  }, 65)
})

testthat::test_that("Only run with population weights", {
  expect_equal({
    vhi <- devdisaster::calc_drought_vhi_risk(devdisaster::sf,start_year=2021,end_year=2021,threshold=40,ag=devdisaster::cropland, pop=devdisaster::crop_val,ag_wt=FALSE,pop_wt=TRUE)
    dim(vhi)[2]
  }, 65)
})

testthat::test_that("Load VHI and calculate risk in different steps", {
  expect_equal({
    r <- load_vhi(devdisaster::sf, start_year= 2021, end_year=2021)
    vhi <- devdisaster::calc_drought_vhi_risk_loaded(devdisaster::sf, r, threshold=40, ag=devdisaster::cropland, pop=devdisaster::crop_val,ag_wt=FALSE,pop_wt=TRUE)
    dim(vhi)[2]
  }, 65)
})

