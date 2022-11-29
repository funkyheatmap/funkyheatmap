# funkyheatmap 0.1.1

## MINOR CHANGES

* The column info data frame can directly contain the `width`, `overlay`, 
  `legend`, `hjust`, `vjust`, `size`, and `label` columns without having to passed
  inside the `options` column first.

* The `options` column can now also be a json string which will then first be parsed
  and processed.

# funkyheatmap 0.1.0

The initial release of funkyheatmap as a standalone package.

## MAJOR CHANGES

* Ported code `funky_heatmap()` from [`dynverse/dynbenchmark`](https://github.com/dynverse/dynbenchmark).

* Split up code into different helper functions.

* Added `verify_*()` functions for checking the file format of input objects and 
  trying to provide helpful messages when the format is not correct.

* Added documentation, unit tests and examples.

* Website is published at [funkyheatmap.dynverse.org](https://funkyheatmap.dynverse.org).