
# funkyheatmap 0.4.0

## BREAKING CHANGES

* Deprecated the `col_annot_offset`, `col_annot_angle` and `expand` arguments in 
  `funky_heatmap()`. Use `position_args = position_arguments(...)` instead (#6).

* Removed the `removed_entries` argument.

## NEW FUNCTIONALITY

* Allow modifying the positioning of elements using the `position_args` argument
  in `funky_heatmap()` (#12).

* Plot images as a geom by specifying either a path, or a directory and an extension (#6).

* Overhaul of the way legends are plotted (#23). Legends are now separate ggplots, the positioning of which is solved via patchwork.

* Allow customizing legends using the `legend` argument in `funky_heatmap()` (#23).

## MINOR CHANGES

* Throw warning if magick is not installed or if image is not found (#18).

* Minor fixes to the documentation (#22).

* Assume logical columns are meant to be displayed as text (#26).

## BUG FIXES

* Fix missing 1.0 label in legend due to limitations in floating-point arithmetic.

* Remove warnings due to changes in tidyselect (#15).

* Remove warnings due to 'size' being renamed to 'linewidth' in ggplot2 v3.4.0 (#16).

* Fix missing categories when no palette is specified for a pie geom (#17).

* Fix issue with rendering non-character columns as text (#5, #26).

* Fix error messge (#1, #26).

* Fix domain url (#26).

## R CMD check results

── R CMD check results ──────────────────────────────────────── funkyheatmap 0.4.0 ────
Duration: 37.2s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔