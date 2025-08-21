#' Generate a Table of Grid IDs and Values from a TPH CSV
#'
#' Reads a transport opportunities CSV file and efficiently generates a table
#' containing the corresponding INSPIRE/GISCO grid cell IDs and data values.
#'
#' This function is the fastest method for converting TPH data, as it operates
#' directly on coordinates and avoids creating any intermediate spatial objects
#' (rasters, polygons, or sf points).
#'
#' @inheritParams tph_to_vector
#' @param add_centroid_coords Logical. If `TRUE`, adds the original `lon` and `lat`
#'   as well as the projected centroid coordinates (`x_centroid`, `y_centroid`)
#'   to the output table. Defaults to `FALSE`.
#' @param add_gisco_corner_coords Logical. If `TRUE`, adds the projected lower-left
#'   (south-west) corner coordinates (`x_ll`, `y_ll`) of the grid cell to the
#'   output table. Defaults to `FALSE`.
#'
#' @return A `data.frame` with the `gisco_id`, the data value, and optional
#'   coordinate columns.
#'
#' @export
#' @examples
#' tph_file <- system.file(
#'   "extdata",
#'   "ver1_0_LU_1km_pt_ppl_within_10-20_min.csv.gz",
#'   package = "tphconv"
#' )
#'
#' # Generate a table with both centroid and corner coordinates
#' full_table <- tph_to_table(
#'   tph_file,
#'   add_centroid_coords = TRUE,
#'   add_gisco_corner_coords = TRUE
#' )
#' head(full_table)
#'
tph_to_table <- function(
  input_file,
  out_column_name = NULL,
  crs_src = 4326,
  crs_dst = 3035,
  x_offset = -500,
  y_offset = -500,
  resolution_m = 1000,
  add_centroid_coords = FALSE,
  add_gisco_corner_coords = FALSE
) {
  # 1. Read the input CSV
  df <- readr::read_csv(input_file, show_col_types = FALSE)

  # 2. Identify and sanitize the data column name
  data_col <- setdiff(names(df), c("lon", "lat"))
  if (length(data_col) != 1) {
    stop("Expected exactly one data column besides 'lon' and 'lat'.")
  }
  if (!is.null(out_column_name)) {
    names(df)[names(df) == data_col] <- out_column_name
    field_name <- sanitize_names(out_column_name)
  } else {
    field_name <- sanitize_names(data_col)
  }
  names(df) <- sanitize_names(names(df))

  # 3. Project coordinates from source to destination CRS
  coords_dst <- sf::sf_project(
    from = sf::st_crs(crs_src)$wkt,
    to = sf::st_crs(crs_dst)$wkt,
    pts = as.matrix(df[, c("lon", "lat")])
  )

  # The projected coordinates are the true centroids
  x_centroid <- coords_dst[, 1]
  y_centroid <- coords_dst[, 2]

  # 4. Apply offsets to find the lower-left corner from the centroid
  x_ll <- x_centroid + x_offset
  y_ll <- y_centroid + y_offset

  # 5. Get the destination EPSG code for the ID function
  epsg_dst <- sf::st_crs(crs_dst)$epsg
  if (is.na(epsg_dst)) {
    stop("Could not determine a valid integer EPSG code for `crs_dst`.")
  }

  # 6. Call the core helper function to generate IDs
  gisco_ids <- generate_grid_id_from_coords(
    xll = x_ll,
    yll = y_ll,
    cellsize = resolution_m,
    epsg = epsg_dst,
    style = "gisco_full"
  )

  # 7. Construct the final table
  result_table <- data.frame(gisco_id = gisco_ids)
  result_table[[field_name]] <- df[[field_name]]

  if (add_centroid_coords) {
    result_table$lon <- df$lon
    result_table$lat <- df$lat
    result_table$x_centroid <- x_centroid
    result_table$y_centroid <- y_centroid
  }

  if (add_gisco_corner_coords) {
    result_table$x_ll <- x_ll
    result_table$y_ll <- y_ll
  }

  return(result_table)
}
