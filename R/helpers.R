#' Helper functions for the package
#' @keywords internal
sanitize_names <- function(x) {
  # 1. Remove just the brackets (but leave a space in their place)
  x <- gsub("[\\(\\)\\[\\]]", " ", x)
  # 2. Collapse any run of spaces into a single underscore
  x <- gsub(" +", "_", x)
  # 3. Remove any non‐alphanumeric/underscore (just in case)
  x <- gsub("[^A-Za-z0-9_]", "", x)
  # 4. Collapse multiple underscores, trim leading/trailing
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  # 5. Lowercase
  tolower(x)
}


#' Generate INSPIRE / GISCO Grid-Cell IDs from Coordinates
#'
#' A low-level helper function that computes standardized grid-cell identifiers
#' from numeric coordinate vectors.
#'
#' @param xll A numeric vector of x-coordinates for the lower-left (south-west)
#'   corner of each cell.
#' @param yll A numeric vector of y-coordinates for the lower-left (south-west)
#'   corner of each cell.
#' @param cellsize The grid resolution in the units of the coordinates.
#' @param epsg The integer EPSG code of the coordinate reference system.
#' @param style The format of the output ID string: "gisco_full" or "gisco_short".
#' @param origin A numeric vector of `c(x, y)` coordinates for the grid's origin.
#'
#' @return A character vector of grid-cell IDs.
#' @keywords internal
generate_grid_id_from_coords <- function(
  xll,
  yll,
  cellsize,
  epsg,
  style,
  origin = c(0, 0)
) {
  # This correctly handles floating-point inaccuracies near cell boundaries by
  # snapping coordinates to the nearest cell corner.
  xll <- round((xll - origin[1]) / cellsize) * cellsize + origin[1]
  yll <- round((yll - origin[2]) / cellsize) * cellsize + origin[2]

  xll <- as.integer(xll)
  yll <- as.integer(yll)

  # Build the ID string based on the chosen style
  if (style == "gisco_full") {
    prefix <- sprintf("CRS%dRES%dm", epsg, as.integer(cellsize))
    ids <- paste0(prefix, "N", yll, "E", xll)
  } else {
    # style == "gisco_short"
    cs <- as.integer(cellsize)
    nzero <- 0L
    while (cs %% 10L == 0L && cs > 0L) {
      cs <- cs %/% 10L
      nzero <- nzero + 1L
    }
    div <- 10^nzero
    size_label <- if (cellsize >= 1000) {
      paste0(cellsize / 1000, "km")
    } else {
      paste0(cellsize, "m")
    }

    yq <- yll / div
    xq <- xll / div

    if (any(yq != as.integer(yq)) || any(xq != as.integer(xq))) {
      warning("Some coordinates are not perfectly aligned to the INSPIRE grid.")
    }
    ids <- paste0(size_label, "N", as.integer(yq), "E", as.integer(xq))
  }

  return(ids)
}
