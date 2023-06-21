
# funkyheatmap 0.4.0

## BREAKING CHANGES

* Deprecated the `col_annot_offset`, `col_annot_angle` and `expand` arguments in 
  `funky_heatmap()`. Use `position_args = position_arguments(...)` instead.

## MAJOR CHANGES

* Plot images as a geom by specifying either a path, or a directory and an extension.

## NEW FUNCTIONALITY

* Allow modifying the positioning of elements using the `position_args` argument
  in `funky_heatmap()`.

## BUG FIXES

* Fix missing 1.0 label in legend due to limitations in floating-point arithmetic.

* Fix missing categories when no palette is specified for a pie geom (#17).

# funkyheatmap 0.3.0

## MAJOR CHANGES

* Funky rectangles are now plotted using `geom_rounded_rect()`, thus simplifying the code quite a bit.

## MINOR CHANGES

* Added a parameter for allowing to change the angle of the column annotation labels,
  `col_annot_angle`.

* Removed dynutils as a dependency.

## BREAKING CHANGES

* Removed parameter `row_annot_offset` since it wasn't being used anymore.

# funkyheatmap 0.2.0

## MAJOR CHANGES

* The column info data frame can directly contain the `width`, `overlay`, 
  `legend`, `hjust`, `vjust`, `size`, and `label` columns without having to passed
  inside the `options` column first.

* The `options` column can now also be a json string which will then first be parsed
  and processed.

* Store the `data-raw/dynbenchmark_data.R` script inside the GitHub repo to be able
  to reproduce the `dynbenchmark_data` object.
  Also store the info and groupings for columns and rows in the `dynbenchmark_data`.

* Extended the dynbenchmark vignette.

# funkyheatmap 0.1.0

The initial release of funkyheatmap as a standalone package.

## MAJOR CHANGES

* Ported code `funky_heatmap()` from [`dynverse/dynbenchmark`](https://github.com/dynverse/dynbenchmark).

* Split up code into different helper functions.

* Added `verify_*()` functions for checking the file format of input objects and 
  trying to provide helpful messages when the format is not correct.

* Added documentation, unit tests and examples.

* Website is published at [funkyheatmap.dynverse.org](https://funkyheatmap.dynverse.org).