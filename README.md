
<!-- README.md is generated from README.Rmd. Please edit that file -->

# funkyheatmap: Generating Funky Heatmaps for Data Frames

<!-- badges: start -->

[![R-CMD-check](https://github.com/dynverse/funkyheatmap/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dynverse/funkyheatmap/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`funkyheatmap` allows generating heatmap-like visualisations for
benchmark data frames. Funky heatmaps can be fine-tuned by providing
annotations of the columns and rows, which allows assigning multiple
palettes or geometries or grouping rows and columns together in
categories.

## Installation

You can install the development version of funkyheatmap like so:

``` r
devtools::install_github("dynverse/funkyheatmap")
```

You can also download `funkyheatmap` as a [standalone
executable](http://funkyheatmap.dynverse.org/articles/executable.html)
or a [Nextflow
pipeline](http://funkyheatmap.dynverse.org/articles/nextflow.html).

## Example

Let’s use the `mtcars` dataset as an example of what a funky heatmap
looks like.

``` r
library(funkyheatmap)
library(dplyr, warn.conflicts = FALSE)
library(tibble, warn.conflicts = FALSE)

data("mtcars")
```

You can visualise the dataset as follows.

``` r
g <- funky_heatmap(mtcars)
#> ℹ Could not find column 'id' in data. Using rownames as 'id'.
#> ℹ No column info was provided, assuming all columns in `data` are to be plotted.
#> ℹ Column info did not contain column `name`, using `id` to generate it.
#> ℹ Column info did not contain information on which columns to plot, inferring from `data` types.
#> ℹ Column info did not contain group information, assuming columns are ungrouped.
#> ℹ Column info did not contain a column called 'palette', generating palettes based on the 'geom' column.
#> ℹ Column info did not contain a column called 'options', generating ptions based on the 'geom' column.
#> ℹ No row info was provided, assuming all rows in `data` are to be plotted.
#> ℹ Row info did not contain group information, assuming rows are ungrouped.
#> ℹ No palettes were provided, trying to automatically assign palettes.
#> ℹ Palette named 'numerical_palette' was not defined. Assuming palette is numerical. Automatically selected palette 'Blues'.
```

``` r
g
```

<img src="man/figures/README-view-heatmap-1.png" width="100%" />

## Customising the plot

However, this plot can look so much better if you provide additional
metadata for the rows and columns. See more information on how you can
customise.

``` r
data <- mtcars %>%
  rownames_to_column("id") %>%
  arrange(desc(mpg))

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

g <- funky_heatmap(data, column_info = column_info, expand = list(xmax = 4))
#> ℹ No row info was provided, assuming all rows in `data` are to be plotted.
#> ℹ Row info did not contain group information, assuming rows are ungrouped.
#> ℹ No column groups was provided, deriving from column info.
#> ℹ Column groups did not contain a column called 'palette'. Assuming no colour scales need to be used.
#> ℹ Column groups did not contain a column called 'level1'. Using `column_info$group` as a makeshift column group name.
#> ℹ No palettes were provided, trying to automatically assign palettes.
#> ℹ Palette named 'palette1' was not defined. Assuming palette is numerical. Automatically selected palette 'Blues'.
#> ℹ Palette named 'palette2' was not defined. Assuming palette is numerical. Automatically selected palette 'Reds'.
```

``` r
g
```

<img src="man/figures/README-view-heatmap-with-colinfo-1.png" width="100%" />

## More information

-   The [reference
    documentation](http://funkyheatmap.dynverse.org/reference/index.html)
    on `funky_heatmap()` details the exact formats of each annotation
    object that you can pass to it.

-   Check out the vignette
    [`vignette("mtcars", "funkyheatmap")`](http://funkyheatmap.dynverse.org/articles/mtcars.html)
    for more information on how to customize this visualisation.

-   In
    [`vignette("dynbenchmark", "funkyheatmap")`](http://funkyheatmap.dynverse.org/articles/dynbenchmark.html)
    we use funkyheatmap to regenerate the figures from Saelens et
    al. (2019)
    [doi:10.1038/s41587-019-0071-9](https://doi.org/10.1038/s41587-019-0071-9).

-   We used [Viash](https://viash.io) to wrap the
    `funkyheatmap::funky_heatmap()` function as a [standalone
    executable](http://funkyheatmap.dynverse.org/articles/executable.html)
    and [Nextflow
    module](http://funkyheatmap.dynverse.org/articles/nextflow.html).
