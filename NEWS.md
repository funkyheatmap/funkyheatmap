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