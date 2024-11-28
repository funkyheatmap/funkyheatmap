
<!-- README.md is generated from README.Rmd. Please edit that file -->

# {funkyheatmap} <img src="https://raw.githubusercontent.com/funkyheatmap/logo/refs/heads/main/src/funkyheatmap_edited.png" align="right" alt="" width=120 />

<!-- badges: start -->

[![cran](https://www.r-pkg.org/badges/version-last-release/funkyheatmap)](https://cran.r-project.org/package=funkyheatmap)
[![R-CMD-check](https://github.com/funkyheatmap/funkyheatmap/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/funkyheatmap/funkyheatmap/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**{funkyheatmap}** allows generating heatmap-like visualisations for
data frames. Funky heatmaps can be fine-tuned by providing annotations
of the columns and rows, which allows assigning multiple palettes or
geometries or grouping rows and columns together in categories.

## Installation

You can install funkyheatmap like so:

``` r
install.packages("funkyheatmap")
```

You can also download **{funkyheatmap}** as a [standalone
executable](https://funkyheatmap.github.io/funkyheatmap/articles/executable.html)
or a [Nextflow
pipeline](https://funkyheatmap.github.io/funkyheatmap/articles/nextflow.html).

## Example

Let’s use the `mtcars` dataset as an example of what a funky heatmap
looks like.

``` r
library(funkyheatmap)
library(dplyr, warn.conflicts = FALSE)
library(tibble, warn.conflicts = FALSE)

data("mtcars")

data <- mtcars %>%
  rownames_to_column("id") %>%
  arrange(desc(mpg))
```

You need to provide some information on how each column should be
rendered, fox example:

``` r
column_info <- tribble(
  ~id,     ~group,         ~name,                      ~geom,        ~palette,    ~options,
  "id",    "",             "",                         "text",       NA,          list(hjust = 0, width = 6),
  "mpg",   "overall",      "Miles / gallon",           "bar",        "palette1",  list(width = 4, legend = FALSE),
  "cyl",   "overall",      "Number of cylinders",      "bar",        "palette2",  list(width = 4, legend = FALSE),
  "disp",  "group1",       "Displacement (cu.in.)",    "funkyrect",  "palette1",  lst(),
  "hp",    "group1",       "Gross horsepower",         "funkyrect",  "palette1",  lst(),
  "drat",  "group1",       "Rear axle ratio",          "funkyrect",  "palette1",  lst(),
  "wt",    "group1",       "Weight (1000 lbs)",        "funkyrect",  "palette1",  lst(),
  "qsec",  "group2",       "1/4 mile time",            "circle",     "palette2",  lst(),
  "vs",    "group2",       "Engine",                   "circle",     "palette2",  lst(),
  "am",    "group2",       "Transmission",             "circle",     "palette2",  lst(),
  "gear",  "group2",       "# Forward gears",          "circle",     "palette2",  lst(),
  "carb",  "group2",       "# Carburetors",            "circle",     "palette2",  lst()
)
```

Now you can generate the funky heatmap:

``` r
funky_heatmap(data, column_info = column_info, expand = list(xmax = 4))
```

<img src="man/figures/README-heatmap2-1.png" width="100%" />

## More information

- The [reference
  documentation](https://funkyheatmap.github.io/funkyheatmap/reference/index.html)
  on `funky_heatmap()` details the exact formats of each annotation
  object that you can pass to it.

- Check out the vignette
  [`vignette("mtcars", "funkyheatmap")`](https://funkyheatmap.github.io/funkyheatmap/articles/mtcars.html)
  for more information on how to customize this visualisation.

- In
  [`vignette("dynbenchmark", "funkyheatmap")`](https://funkyheatmap.github.io/funkyheatmap/articles/dynbenchmark.html)
  we use funkyheatmap to regenerate the figures from Saelens et
  al. (2019)
  [doi:10.1038/s41587-019-0071-9](https://doi.org/10.1038/s41587-019-0071-9).

- We used [Viash](https://viash.io) to wrap the
  `funkyheatmap::funky_heatmap()` function as a [standalone
  executable](https://funkyheatmap.github.io/funkyheatmap/articles/executable.html)
  and [Nextflow
  module](https://funkyheatmap.github.io/funkyheatmap/articles/nextflow.html).
