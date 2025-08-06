test_that("luxemburg example is converted to raster", {
  tph_file <- system.file(
    "extdata",
    "ver1_0_LU_1km_pt_ppl_within_10-20_min.csv.gz",
    package = "tphconv"
  )

  r <- tph_to_raster(tph_file)
  expect_s4_class(r, "SpatRaster")
  expect_equal(max(terra::values(r), na.rm = TRUE), 61065, tolerance = 1)
})

test_that("luxemburg example is saved to disk", {
  tph_file <- system.file(
    "extdata",
    "ver1_0_LU_1km_pt_ppl_within_10-20_min.csv.gz",
    package = "tphconv"
  )

  tmp_file <- tempfile(fileext = ".tiff")
  out_raster_file <- tph_to_raster(
    input_file = tph_file,
    out_raster_file = tmp_file
  )

  r <- terra::rast(out_raster_file)
  expect_s4_class(r, "SpatRaster")
  expect_equal(max(terra::values(r), na.rm = TRUE), 61065, tolerance = 1)
})
