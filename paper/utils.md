``` r
library(funkyheatmap)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
legends <- list(
  list(title = "Bar", palette = "qc", enabled = FALSE, geom = "bar"),
  list(title = "Bar", palette = "benchmark", enabled = FALSE, geom = "bar"),
  list(title = "Bar", palette = "scaling", enabled = FALSE, geom = "bar"),
  list(title = "Bar", palette = "stability", enabled = FALSE, geom = "bar"),
  list(title = "Error reason", palette = "error_reasons", enabled = TRUE, geom = "pie"),
  list(title = "Score", palette = "overall", enabled = TRUE, geom = "funkyrect")
)

funky_heatmap(
  data = dynbenchmark_data$data,
  column_info = dynbenchmark_data$column_info,
  column_groups = dynbenchmark_data$column_groups,
  row_info = dynbenchmark_data$row_info,
  row_groups = dynbenchmark_data$row_groups,
  palettes = dynbenchmark_data$palettes,
  legends = legends,
  col_annot_offset = 3.2
)
```

    ## ℹ Some palettes were not used in the column info, adding legends for them.
    ## ℹ Legend 5 did not contain labels, inferring from the geom.
    ## ℹ Legend 5 did not contain color, inferring from the palette.
    ## ℹ Legend 6 did not contain labels, inferring from the geom.
    ## ℹ Legend 6 did not contain size, inferring from the labels.
    ## ℹ Legend 6 did not contain color, inferring from the palette.
    ## ℹ Legend 7 did not contain a geom, inferring from the column info.
    ## ℹ Legend 7 did not contain labels, inferring from the geom.
    ## ! Legend 7 has geom text but no specified labels, so disabling this legend for now.

<figure>
<img src="utils_files/figure-gfm/fig-dynbenchmark-1.svg"
alt="An example of a {funkyheatmap} visualisation using data from a benchmarking study of trajectory inference methods [@comparisonsinglecell_saelens2019]." />
<figcaption aria-hidden="true">An example of a
<code>{funkyheatmap}</code> visualisation using data from a benchmarking
study of trajectory inference methods <span class="citation"
data-cites="comparisonsinglecell_saelens2019">[@comparisonsinglecell_saelens2019]</span>.</figcaption>
</figure>

| Data type         | Example                          | Recommended geom    |
|:------------------|:---------------------------------|:--------------------|
| Numerical data    | Scores from 0 to 1               | funkyrect           |
| Aggregated data   | The mean of scores               | bar                 |
| Measurement data  | 3MB or 4h                        | rect + text overlay |
| Categorical data  | R or Python                      | text or image       |
| Proportional data | 80% success, 10% OOM, 10% failed | pie                 |
