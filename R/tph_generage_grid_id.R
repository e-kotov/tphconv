#' Generate INSPIRE / GISCO Grid-Cell IDs
#'
#' Computes standardized grid-cell identifiers for SpatVector, SpatRaster,
#' or sf/sfc objects based on the lower-left (south-west) corner coordinates.
#'
#' @param x A `SpatVector`, `SpatRaster`, or `sf`/`sfc` object.
#' @param cellsize The grid resolution in the units of the object's CRS (e.g., meters).
#'   If `NULL`, it will be inferred from the raster's resolution or the bounding
#'   box of the first vector feature (if square).
#' @param crs An optional CRS string (e.g., "EPSG:3035") to use if the
#'   object's CRS cannot be automatically determined.
#' @param style The format of the output ID string: "gisco_full" or "inspire_short".
#' @param origin A numeric vector of `c(x, y)` coordinates for the grid's origin.
#'   Defaults to `c(0, 0)`.
#' @param raster_mask_na For `SpatRaster` input, if `TRUE`, IDs are only computed
#'   for cells that are not `NA`.
#' @param raster_layer For `SpatRaster` input, the layer index to use for the
#'   `NA` mask.
#' @return A character vector of grid-cell IDs. For `SpatRaster` inputs, the
#'   vector is named with the corresponding cell indices.
#'@keywords internal
tph_generage_grid_id <- function(
  x,
  cellsize = NULL,
  crs = NULL,
  style = c("gisco_full", "inspire_short"),
  origin = c(0, 0),
  raster_mask_na = FALSE,
  raster_layer = 1
) {
  # ... (Input validation and EPSG detection are unchanged) ...
  is_sv <- inherits(x, "SpatVector")
  is_sr <- inherits(x, "SpatRaster")
  is_sf <- inherits(x, "sf") || inherits(x, "sfc")
  if (!(is_sv || is_sr || is_sf)) {
    stop("Input 'x' must be a SpatVector, SpatRaster, or sf/sfc object.")
  }
  style <- match.arg(style)
  epsg <- tryCatch(
    {
      if (is_sv || is_sr) sf::st_crs(terra::crs(x))$epsg else sf::st_crs(x)$epsg
    },
    error = function(e) NA_integer_
  )
  if (is.na(epsg) && !is.null(crs)) {
    epsg <- sf::st_crs(crs)$epsg
  }
  if (is.na(epsg)) {
    stop(
      "Could not determine EPSG. Please specify a valid CRS, e.g., `crs = 'EPSG:3035'`."
    )
  }

  # ... (Coordinate extraction is unchanged) ...
  if (is_sr) {
    resxy <- terra::res(x)
    if (is.null(cellsize)) {
      if (abs(resxy[1] - resxy[2]) > 1e-9) {
        stop("Raster cells must be square to auto-detect cellsize.")
      }
      cellsize <- as.integer(round(resxy[1]))
    }
    idx <- if (raster_mask_na) {
      which(!is.na(terra::values(x[[raster_layer]], mat = FALSE)))
    } else {
      seq_len(terra::ncell(x))
    }
    if (length(idx) == 0) {
      return(character(0))
    }
    coords <- terra::xyFromCell(x, idx)
    xll <- coords[, "x"] - resxy[1] / 2
    yll <- coords[, "y"] - resxy[2] / 2
  } else if (is_sv) {
    if (is.null(cellsize)) {
      ex1 <- terra::ext(x[1])
      w1 <- ex1$xmax - ex1$xmin
      h1 <- ex1$ymax - ex1$ymin
      if (abs(w1 - h1) < 1e-9) {
        cellsize <- as.integer(round(w1))
      } else {
        stop("Provide `cellsize`; first feature's bounding box is not square.")
      }
    }
    crds_df <- terra::crds(x, df = TRUE)
    if (nrow(crds_df) == 0) {
      return(character(0))
    }
    if ("geom" %in% names(crds_df)) {
      xll <- tapply(crds_df$x, crds_df$geom, min, na.rm = TRUE)
      yll <- tapply(crds_df$y, crds_df$geom, min, na.rm = TRUE)
    } else {
      xll <- min(crds_df$x, na.rm = TRUE)
      yll <- min(crds_df$y, na.rm = TRUE)
    }
  } else {
    # sf/sfc
    if (is.null(cellsize)) {
      bb1 <- sf::st_bbox(sf::st_geometry(x)[[1]])
      w1 <- bb1["xmax"] - bb1["xmin"]
      h1 <- bb1["ymax"] - bb1["ymin"]
      if (abs(w1 - h1) < 1e-9) {
        cellsize <- as.integer(round(unname(w1)))
      } else {
        stop("Provide `cellsize`; first feature's bounding box is not square.")
      }
    }
    geoms <- if (inherits(x, "sf")) sf::st_geometry(x) else x
    bbs <- lapply(geoms, sf::st_bbox)
    xll <- vapply(bbs, `[`, numeric(1), "xmin", USE.NAMES = FALSE)
    yll <- vapply(bbs, `[`, numeric(1), "ymin", USE.NAMES = FALSE)
  }

  # --- FIX: Use round() instead of floor() for more robust snapping ---
  # This correctly handles floating-point inaccuracies near cell boundaries.
  xll <- round((xll - origin[1]) / cellsize) * cellsize + origin[1]
  yll <- round((yll - origin[2]) / cellsize) * cellsize + origin[2]

  xll <- as.integer(xll)
  yll <- as.integer(yll)

  # ... (ID string building is unchanged) ...
  if (style == "gisco_full") {
    prefix <- sprintf("CRS%dRES%dm", epsg, as.integer(cellsize))
    ids <- paste0(prefix, "N", yll, "E", xll)
  } else {
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

  if (is_sr) {
    names(ids) <- idx
  }
  return(ids)
}
