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

## R CMD check results

── R CMD check results ──────────────── funkyheatmap 0.2.0 ────
Duration: 1m 13.3s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded