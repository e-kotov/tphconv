test_that("luxembourg example is converted to sf", {
  tph_file <- system.file(
    "extdata",
    "ver1_0_LU_1km_pt_ppl_within_10-20_min.csv.gz",
    package = "tphconv"
  )

  sf_obj <- tph_to_vector(tph_file, return_as = "sf")
  expect_s3_class(sf_obj, "sf")
  expect_true("gisco_id" %in% names(sf_obj))
  expect_true(all(grepl("^CRS3035RES1000mN[0-9]+E[0-9]+$", sf_obj$gisco_id)))
})

test_that("luxembourg example is saved to disk as gpkg", {
  tph_file <- system.file(
    "extdata",
    "ver1_0_LU_1km_pt_ppl_within_10-20_min.csv.gz",
    package = "tphconv"
  )

  tmp_file <- tempfile(fileext = ".gpkg")
  out_sf_file <- tph_to_vector(
    input_file = tph_file,
    out_vector_file = tmp_file
  )

  sf_obj <- sf::st_read(out_sf_file)
  expect_s3_class(sf_obj, "sf")
  expect_true("gisco_id" %in% names(sf_obj))
  expect_true(all(grepl("^CRS3035RES1000mN[0-9]+E[0-9]+$", sf_obj$gisco_id)))
})
