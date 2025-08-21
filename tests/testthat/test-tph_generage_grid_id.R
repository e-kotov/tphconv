# Load necessary libraries for testing
# Ensure you have these installed: install.packages(c("testthat", "terra", "sf"))
library(testthat)
library(terra)
library(sf)

# Source the function to be tested
# Make sure the file path is correct or the function is loaded in the environment
# source("path/to/your/tph_generage_grid_id.R")

# --- Test Suite for tph_generage_grid_id ---

test_that("Grid IDs are generated correctly for various spatial objects", {
  # --- 1. Define Test Parameters and Expected Outputs ---

  # Define the target grid cell properties
  crs_epsg <- 3035
  crs_wkt <- sf::st_crs(crs_epsg)$wkt
  cell_size <- 1000
  x_coord <- 3760000
  y_coord <- 2888000

  # Define the expected ID strings based on the function's logic
  expected_id_full <- "CRS3035RES1000mN2888000E3760000"
  expected_id_short <- "1kmN2888E3760"

  # --- 2. Create Test Data ---

  # a) Create an sf polygon object
  # This polygon represents the exact boundaries of the target grid cell.
  poly_coords <- matrix(
    c(
      x_coord,
      y_coord, # Bottom-left
      x_coord + cell_size,
      y_coord, # Bottom-right
      x_coord + cell_size,
      y_coord + cell_size, # Top-right
      x_coord,
      y_coord + cell_size, # Top-left
      x_coord,
      y_coord # Close the polygon
    ),
    ncol = 2,
    byrow = TRUE
  )

  sf_poly <- sf::st_sfc(sf::st_polygon(list(poly_coords)), crs = crs_epsg)
  sf_obj <- sf::st_sf(data.frame(id = 1), geometry = sf_poly)

  # b) Create a SpatVector from the sf object
  sv_obj <- terra::vect(sf_obj)

  # c) Create a SpatRaster
  # This raster has a single cell that corresponds to the target grid cell.
  sr_obj <- terra::rast(
    xmin = x_coord,
    xmax = x_coord + cell_size,
    ymin = y_coord,
    ymax = y_coord + cell_size,
    resolution = cell_size,
    crs = crs_wkt
  )
  # Assign a value to the cell to ensure it's not NA
  terra::values(sr_obj) <- 1

  # d) Create a SpatRaster with an NA cell for mask testing
  sr_obj_na <- terra::rast(sr_obj)
  terra::values(sr_obj_na) <- NA

  # --- 3. Run Tests ---

  # Test 1: SpatRaster input
  # FIX: Use unname() to remove cell index names from the output before comparison
  expect_equal(
    unname(tph_generage_grid_id(sr_obj, style = "gisco_full")),
    expected_id_full,
    label = "SpatRaster should produce the correct full GISCO ID"
  )
  expect_equal(
    unname(tph_generage_grid_id(sr_obj, style = "gisco_short")),
    expected_id_short,
    label = "SpatRaster should produce the correct short GISCO ID"
  )

  # Test 2: SpatVector input
  expect_equal(
    tph_generage_grid_id(sv_obj, style = "gisco_full"),
    expected_id_full,
    label = "SpatVector should produce the correct full GISCO ID"
  )
  expect_equal(
    tph_generage_grid_id(sv_obj, style = "gisco_short"),
    expected_id_short,
    label = "SpatVector should produce the correct short GISCO ID"
  )

  # Test 3: sf object input
  expect_equal(
    tph_generage_grid_id(sf_obj, style = "gisco_full"),
    expected_id_full,
    label = "sf object should produce the correct full GISCO ID"
  )
  expect_equal(
    tph_generage_grid_id(sf_obj, style = "gisco_short"),
    expected_id_short,
    label = "sf object should produce the correct short GISCO ID"
  )

  # Test 4: sfc object input (geometry only)
  expect_equal(
    tph_generage_grid_id(sf_poly, style = "gisco_full"),
    expected_id_full,
    label = "sfc object should produce the correct full GISCO ID"
  )
  expect_equal(
    tph_generage_grid_id(sf_poly, style = "gisco_short"),
    expected_id_short,
    label = "sfc object should produce the correct short GISCO ID"
  )

  # --- 4. Test Edge Cases ---

  # Test NA masking in SpatRaster
  # Expect an empty character vector when the only cell is NA and masking is on
  expect_equal(
    tph_generage_grid_id(sr_obj_na, raster_mask_na = TRUE),
    character(0),
    label = "Should return empty vector for fully NA raster with masking"
  )

  # Expect the ID when masking is off, even if the cell is NA
  # FIX: Use unname() to remove cell index names from the output before comparison
  expect_equal(
    unname(tph_generage_grid_id(sr_obj_na, raster_mask_na = FALSE)),
    expected_id_full,
    label = "Should return ID for NA raster when masking is off"
  )

  # Test empty vector input
  expect_equal(
    tph_generage_grid_id(sv_obj[0, ]),
    character(0),
    label = "Should return empty vector for empty SpatVector"
  )

  # Test empty sf input
  expect_equal(
    tph_generage_grid_id(sf_obj[0, ]),
    character(0),
    label = "Should return empty vector for empty sf object"
  )
})
