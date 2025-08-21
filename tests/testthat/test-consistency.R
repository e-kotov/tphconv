test_that("data values from raster and vector outputs are consistent", {
  # This test requires sf
  if (!requireNamespace("sf", quietly = TRUE)) {
    skip("sf package not available")
  }

  # 1. Define input file
  tph_file <- system.file(
    "extdata",
    "ver1_0_LU_1km_pt_ppl_within_10-20_min.csv.gz",
    package = "tphconv"
  )

  # 2. Generate the vector grid (with its direct, unsnapped geometry)
  v_grid <- tph_to_vector(
    tph_file,
    return_as = "sf",
    out_column_name = "opportunities"
  )

  # 3. Generate the raster grid (with its snapped geometry)
  r_grid <- tph_to_raster(
    tph_file,
    out_column_name = "opportunities"
  )

  # 4. To properly test consistency, we must compare the raster's values at the
  # exact, snapped locations it uses for its cell centers. We recreate that
  # logic here instead of using the centroids from the unsnapped vector grid.
  df_raw <- readr::read_csv(tph_file, show_col_types = FALSE)
  coords_dst <- sf::sf_project(
    from = sf::st_crs(4326)$wkt,
    to = sf::st_crs(3035)$wkt,
    pts = as.matrix(df_raw[, c("lon", "lat")])
  )

  resolution_m <- 1000 # Match the default used in the functions

  # This logic exactly mimics the snapping inside tph_to_raster
  x_ll <- floor(coords_dst[, 1] / resolution_m) * resolution_m
  y_ll <- floor(coords_dst[, 2] / resolution_m) * resolution_m
  snapped_centroids <- data.frame(
    x = x_ll + (resolution_m / 2),
    y = y_ll + (resolution_m / 2)
  )

  # 5. Extract raster values at the snapped centroid locations.
  # This guarantees we sample the correct cells.
  extracted_vals <- terra::extract(r_grid, snapped_centroids)

  # 6. Compare the data values. We assume the row order is preserved from the
  # original CSV in all function calls.
  data_col_raw <- setdiff(names(df_raw), c("lon", "lat"))
  comparison_df <- data.frame(
    original_opps = df_raw[[data_col_raw]],
    vector_opps = v_grid$opportunities,
    raster_opps = extracted_vals$opportunities
  )

  # 7. Perform the comparison
  # Check that the data values are identical across all three sources.
  expect_equal(comparison_df$original_opps, comparison_df$vector_opps)
  expect_equal(comparison_df$original_opps, comparison_df$raster_opps)
  expect_equal(comparison_df$vector_opps, comparison_df$raster_opps)

  # Check that the number of valid data points is the same
  expect_equal(nrow(v_grid), nrow(na.omit(comparison_df)))
})
