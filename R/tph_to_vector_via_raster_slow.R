#' Convert Transport Opportunities CSV to a Spatial Vector Object via Raster
#'
#' @description{
#' **WARNING: This function is just a prototype and it is slow, as it first creates a raster and then vectorizes it. You must have found this function by accident, as it is not exported. Kindly, use the \code{\link{tph_to_vector}} instead.**
#' Reads a CSV file (optionally gzipped) of public transport accessibility data and converts it
#' into a spatial vector format (`sf` or `terra::SpatVector`). The function
#' first rasterizes the input point data onto a regular grid and then converts
#' the raster cells into polygons. An INSPIRE-compliant grid cell ID can also
#' be generated for each polygon.
#'}
#' @inheritParams tph_to_raster
#' @param output_file Character or `NULL`. If a file path is provided (e.g.,
#'   `"data/luxembourg.gpkg"`), the output is written to that file using
#'   `terra::writeVector`, and the file path is returned. If `NULL` (the default),
#'   the function returns an in-memory spatial object.
#' @param return_as Character. Specifies the class of the returned object when
#'   `output_file` is `NULL`. Can be either `"sf"` (the default) or
#'   `"SpatVector"`. This argument is ignored if `output_file` is not `NULL`.
#' @param add_id Logical. If `TRUE` (the default), an INSPIRE-compliant grid
#'   cell ID is generated and added as a column named `"id"`. Set to `FALSE`
#'   to omit this step.
#'
#' @return
#' Depending on the arguments:
#' - If `output_file` is `NULL`, returns an `sf` object (default) or a `SpatVector`
#'   object (if `return_as = "SpatVector"`) with polygon geometries. The object
#'   will include an `"id"` column unless `add_id` is `FALSE`.
#' - If `output_file` is a character string, the path to the created file is returned.
#' @keywords internal
#'
tph_to_vector_via_raster_slow <- function(
  input_file,
  out_column_name = NULL,
  crs_src = 4326,
  crs_dst = 3035,
  x_offset = -500,
  y_offset = -500,
  resolution_m = 1000,
  output_file = NULL,
  return_as = c("sf", "SpatVector"),
  add_id = TRUE
) {
  # Validate arguments
  return_as <- match.arg(return_as)

  # 1. Convert the input CSV to an intermediate raster
  raster_grid <- tph_to_raster(
    input_file = input_file,
    out_column_name = out_column_name,
    crs_src = crs_src,
    crs_dst = crs_dst,
    x_offset = x_offset,
    y_offset = y_offset,
    resolution_m = resolution_m
  )

  # 2. Convert the raster to polygons (SpatVector)
  vectorized_grid <- terra::as.polygons(raster_grid, na.rm = TRUE)

  # 3. Generate standardized grid IDs for each polygon (optional)
  if (add_id) {
    vectorized_grid$id <- tph_generage_grid_id(
      x = vectorized_grid,
      cellsize = resolution_m,
      crs = crs_dst
    )
  }

  # 4. Handle output: either write to a file or return an in-memory object
  if (!is.null(output_file)) {
    # Ensure output directory exists
    out_dir <- fs::path_dir(output_file)
    if (!fs::dir_exists(out_dir)) {
      fs::dir_create(out_dir, recurse = TRUE)
    }

    # Write SpatVector directly to disk, which is more efficient
    terra::writeVector(
      vectorized_grid,
      filename = output_file,
      overwrite = TRUE
    )

    # Return the file path
    return(output_file)
  } else {
    # Return an in-memory object based on user's choice
    if (return_as == "sf") {
      return(sf::st_as_sf(vectorized_grid))
    } else {
      # return_as == "SpatVector"
      return(vectorized_grid)
    }
  }
}
