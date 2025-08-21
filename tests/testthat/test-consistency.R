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

test_that("specific gisco id matches output", {
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

  # 2. Generate the vector grid
  v_grid <- tph_to_vector(
    tph_file,
    return_as = "sf",
    out_column_name = "opportunities"
  )

  # 3. Check for a specific gisco_id
  expect_true(
    v_grid[v_grid$opportunities == 10955, ]$gisco_id ==
      "CRS3035RES1000mN2935000E4033000"
  )

  # Check for a specific gisco_id in the raster grid
  r_grid <- tph_to_raster(
    tph_file,
    out_column_name = "opportunities"
  )

  target_gisco_id <- "CRS3035RES1000mN2935000E4033000"
  target_x <- 4033000
  target_y <- 2935000

  # 6. Find the cell index corresponding to these coordinates.
  # We add half the resolution (500m) to get the cell's center point.
  cell_index <- terra::cellFromXY(r_grid, cbind(target_x + 500, target_y + 500))

  # 7. Extract the values from that specific cell
  # r_grid[cell_index] returns a data.frame with columns for each layer
  values_in_cell <- r_grid[cell_index]

  # 8. Compare the extracted opportunity value with the expected value
  expect_equal(
    values_in_cell$opportunities,
    10955,
    label = "The raster cell value at the specified GISCO ID should match"
  )

  # 9. Compare the extracted gisco_id text label with the expected value
  expect_equal(
    as.character(values_in_cell$gisco_id),
    target_gisco_id,
    label = "The gisco_id label in the raster should match the expected ID"
  )

  # test table
  table_output <- tph_to_table(
    tph_file,
    out_column_name = "opportunities",
    add_centroid_coords = FALSE,
    add_gisco_corner_coords = FALSE
  )
  expect_true(
    table_output[table_output$opportunities == 10955, ]$gisco_id ==
      target_gisco_id
  )
})

test_that("-1 values inserted into real data are converted to NA", {
  # 1. Locate and read the original, unmodified package data
  tph_file <- system.file(
    "extdata",
    "ver1_0_LU_1km_pt_ppl_within_10-20_min.csv.gz",
    package = "tphconv"
  )
  original_df <- readr::read_csv(tph_file, show_col_types = FALSE)

  # 2. Modify the data in memory
  modified_df <- original_df
  rows_to_modify <- c(335, 600)

  # Find the data column name programmatically
  data_col <- setdiff(names(original_df), c("lon", "lat"))

  # Check if the data frame is large enough to avoid errors
  if (nrow(modified_df) < max(rows_to_modify)) {
    skip("The data file has fewer rows than the test expects.")
  }

  # Replace the values in our chosen rows with -1
  modified_df[[data_col]][rows_to_modify] <- -1

  # reset data col name to ensure it is consistent
  data_col <- "value"

  # 3. Write the modified data to a temporary file for testing
  temp_mod_file <- tempfile(fileext = ".csv")
  readr::write_csv(modified_df, temp_mod_file)
  on.exit(unlink(temp_mod_file)) # Ensure cleanup after the test

  # 4. Run the functions and verify the output

  # Test tph_to_table()
  table_out <- tph_to_table(temp_mod_file, out_column_name = data_col)
  expect_true(all(is.na(table_out[[data_col]][rows_to_modify])))
  expect_equal(sum(is.na(table_out[, data_col])), 2) # Ensure only modified rows are NA

  # Test tph_to_vector()
  vector_out <- tph_to_vector(temp_mod_file, out_column_name = data_col)
  expect_true(all(is.na(vector_out[[data_col]][rows_to_modify])))
  expect_false(is.na(vector_out[[data_col]][1])) # Check that other rows are unaffected

  # Test tph_to_raster()
  raster_out <- tph_to_raster(temp_mod_file, out_column_name = data_col)

  # Get the original lon/lat coordinates for the rows we changed
  coords_to_check <- original_df[rows_to_modify, c("lon", "lat")]

  # Extract raster values at these locations. terra::extract handles reprojection.
  extracted_vals <- terra::extract(
    raster_out,
    coords_to_check,
    method = "simple"
  )

  # The extracted values for the modified points should be NA
  expect_true(all(is.na(extracted_vals[[data_col]])))
})
