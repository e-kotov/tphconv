# tphconv 0.1.1 (2025-08-21)

* Added automatic conversion of `-1` values to `NA` in `tph_to_vector` and `tph_to_table`, ensuring that missing data is handled correctly.

# tphconv 0.1.0 (2025-08-21)

* Bugs fixed in `tph_to_raster`, now the grid is correctly aligned with the reference GISCO grid.

* `tph_to_raster` now also generates GISCO grid IDs in the second layer of the raster.

* New function `tph_to_vector` to convert TPH data to a vector formats.

* New function `tph_to_table` to convert TPH data to a simple table `data.frame`.

# tphconv 0.0.0.9000 (2025-08-06)

* Initial draft.
