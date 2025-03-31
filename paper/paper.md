---
title: "funkyheatmap: Visualising data frames with mixed data types"
tags:
  - benchmarking
  - visualisations

authors:
- name: Robrecht Cannoodt*
  orcid: 0000-0003-3641-729X
  email: robrecht@data-intuitive.com
  affiliation: "1, 2, 3"
- name: Louise Deconinck*
  orcid: 0000-0001-8100-6823
  email: louise.deconinck@ugent.be
  affiliation: "1, 2"
- name: Artuur Couckuyt*
  orcid: 0000-0001-7858-6521
  email: artuur.couckuyt@ugent.be
  affiliation: "1, 2"
- name: Nikolay S. Markov*
  orcid: 0000-0002-3659-4387
  email: nikolai.markov@icloud.com
  affiliation: 6
- name: Luke Zappia
  orcid: 0000-0001-7744-8565
  affiliation: "3, 7, 9"
- name: Malte D. Luecken
  orcid: 0000-0001-7464-7921
  affiliation: "7, 9"
- name: Marta Interlandi
  orcid: 0000-0002-6863-2552
  email: marta.interlandi01@gmail.com
  affiliation: 5
- name: Yvan Saeys†
  orcid: 0000-0002-0415-1506
  email: yvan.saeys@ugent.be
  affiliation: "1, 2"
- name: Wouter Saelens†
  orcid: 0000-0002-7114-6248
  email: w.saelens@epfl.ch
  affiliation: "4, 10"

affiliations:
  - id: dambi
    index: 1
    name: Data Mining and Modelling for Biomedicine group, VIB Center for Inflammation Research, Ghent, Belgium
  - id: twist
    index: 2
    name: Department of Applied Mathematics, Computer Science, and Statistics, Ghent University, Belgium
  - id: di
    index: 3
    name: Data Intuitive, Lebbeke, Belgium
  - id: lmcb
    index: 4
    name: Laboratory of Myeloid Cell Biology in tissue homeostasis and regeneration, VIB Center for Inflammation Research, Ghent, Belgium
  - id: imi
    index: 5
    name: Institute of Medical Informatics, University of Muenster, Muenster, Germany
  - id: nu
    index: 6
    name: Division of Pulmonary and Critical Care Medicine, Northwestern University, Chicago, USA
  - id: icb
    index: 7
    name: Institute of Computational Biology, Computational Health Center, Helmholtz Munich, Munich, Germany
  - id: tum-ma
    index: 8
    name: Department of Mathematics, School of Computation, Information and Technology, Technical University of Munich, Munich, Germany
  - id: lhi
    index: 9
    name: Institute of Lung Health and Immunity (LHI), Helmholtz Munich, Comprehensive Pneumology Center (CPC-M), Germany; Member of the German Center for Lung Research (DZL)
  - id: dbmb
    index: 10
    name: Department of Biomedical Molecular Biology, Ghent University, Ghent, Belgium

engine: knitr
bibliography: paper.bib
---

*: Shared first authors, †: Shared last authors.

# Summary

The `{funkyheatmap}` package offers a flexible and user-friendly solution for visualising data frames containing a mixture of categorical, proportional, and text-based data. It simplifies the creation of informative and visually appealing heatmaps while providing extensive customization options to tailor the output. This tool is especially valuable in research settings for summarising and communicating complex results, such as those encountered in benchmarking studies.

The package is available on [CRAN](https://cran.r-project.org/package=funkyheatmap), [PyPI](https://pypi.org/project/funkyheatmappy) and [npm](https://www.npmjs.com/package/funkyheatmapjs). For a showcase of examples and an overview of the different implementations, visit the project website [funkyheatmap.github.io](https://funkyheatmap.github.io).

# Statement of need
Data visualisation is fundamental to exploratory data analysis and communicating findings. 
Table-creating packages, such as gt [@gt], allow the user to create beautiful hierarchical tables, but these packages are limited to showing textual data.
Heatmap-creating packages such as pheatmap [@pheatmap], superheat [@superheat] and ComplexHeatmap [@complexheatmap1; @complexheatmap2] require the central visualisation to remain a traditional heatmap and allow additional annotations to the side of this heatmap. Incorporating multiple datatypes is possible, but far from seamless.
While more powerful tools like ggplot2 [@ggplot2_wickham2009], Matplotlib [@matplotlib2dgraphics_hunter2007] and D3.js [@d3datadriven_bostock2011] exist, they often require complex scripting to generate comprehensive visualisations for data frames containing a mix of data types.

`{funkyheatmap}` addresses this challenge by:

* **Seamless Handling of Mixed Data**: Automates the selection of appropriate visualisations (rectangles, bars, pie charts, text) based on data types. and allows easy mixing of datatypes in the same visualisation.
* **Customization**: Provides granular control over colours, groupings, geometries, and annotations for tailored results.
* **Accessibility**: Offers a simplified interface for basic use and detailed documentation for advanced customization.

`{funkyheatmap}` has proven its utility in benchmarking studies within single-cell omics [@comparisonsinglecell_saelens2019; @benchmarkingatlaslevel_luecken2020; @benchmarkingintegrationmethods_yan2022; @spotlessreproduciblepipeline_sangaram2023; @definingbenchmarkingopen_luecken2023; @comprehensivebenchmarkingpractical_li2023], but its applications extend to diverse fields where visualisation of mixed data types is needed.

# Functionality
\autoref{fig-dynbenchmark-1} showcases the functionality of `{funkyheatmap}`, namely:

* **Diverse Geometries**: Supports a range of geometries (rectangles, bars, pie charts, text, images) to effectively represent different data types.
* **Hierarchical Categorical Grouping**: Facilitates the organisation of rows and columns into semantic groups with distinct colour palettes.

Besides this, the package includes comprehensive documentation, vignettes, and a test suite.

![An example of a `{funkyheatmap}` visualisation using data from a benchmarking study of trajectory inference methods [@comparisonsinglecell_saelens2019].\label{fig-dynbenchmark-1}](figure1.svg)

See the following \autoref{tbl-geoms} for more information regarding the recommended geom for different types of data.

| Data type         | Example                          | Recommended geom    |
|:------------------|:---------------------------------|:--------------------|
| Numerical data    | Scores from 0 to 1               | funkyrect           |
| Aggregated data   | The mean of scores               | bar                 |
| Measurement data  | 3MB or 4h                        | rect + text overlay |
| Categorical data  | R or Python                      | text or image       |
| Proportional data | 80% success, 10% OOM, 10% failed | pie                 |

The table presents the suggested visualisation methods (geoms) based on the data type of the columns. These recommendations provide a starting point for users to select the most appropriate visual representation for their specific data. \label{tbl-geoms}

## Example usage
In order to produce a `{funkyheatmap}` visualisation, you need to provide the data in the form of a dataframe, which also must contain a column named `id`.
If you provide no other information, a basic visualisation will be provided, but customization is possible by provinding additional information, such as a `column_info` dataframe which details how the columns in the dataframe get translated into different geoms, or a `row_groups` dataframe which allows you to group rows in the visualisation.

As an example, the following code produces \autoref{fig-mtcars}. An in-depth explanation of how we come to 
all these data structures and tribbles, please refer to the vignette named `mtcars`.

```r
library(funkyheatmap)
library(dplyr, warn.conflicts = FALSE)
library(tibble, warn.conflicts = FALSE)
library(RColorBrewer)

data("mtcars")
data <- mtcars %>%
  rownames_to_column("id") %>%
  arrange(desc(mpg)) %>%
  head(30)
# sort the columns in logical groupings (see column_info group column)
data <- data[, c("id", "qsec", "mpg", "wt", "cyl", "carb", "disp", "hp", "vs", "drat", "am", "gear")]

# change data to use images
# change the am: if 0 go to "automatic", if 1 go to "manual"
data[data$am == 0, "am"] <- "automatic"
data[data$am == 1, "am"] <- "manual"
# change the vs: if 0 go to "vengine", if 1 go to "straight"
data[data$vs == 0, "vs"] <- "vengine"
data[data$vs == 1, "vs"] <- "straight"

column_info <- tribble(
  ~id,     ~group,         ~name,                   ~geom,        ~palette,               ~options,
  "id",    "",             "Model",                 "text",        NA,                    lst(),
  "qsec",  "Performance",  "1/4 mile time",         "bar",        "perf_palette",         lst(),
  "mpg",   "Overall",      "Number of cylinders",   "bar",        "overall_palette",      lst(),
  "wt",    "Overall",      "Weight (1000 lbs)",     "bar",        "overall_palette",      lst(), 
  "cyl",   "Engine",       "Number of cylinders",   "rect",       "engine_palette",       lst(),  
  "cyl",   "Engine",       "",                      "text",       "black",              lst(overlay = TRUE),
  "carb",  "Engine",       "Carburetors",           "rect",       "engine_palette",       lst(),
  "carb",  "Engine",       "",                      "text",       "black",              lst(overlay = TRUE),    
  "disp",  "Engine",       "Displacement",          "funkyrect",  "engine_palette",       lst(),
  "hp",    "Engine",       "Horsepower",            "funkyrect",  "engine_palette",       lst(),
  "vs",    "Engine",       "Engine type",           "image",      "engine_palette",       lst(directory = "vignettes/images", extension = "png"),
  "drat",  "Transmission", "Rear axle ratio",       "funkyrect",  "transmission_palette", lst(),
  "am",    "Transmission", "Transmission",          "image",      "transmission_palette", lst(directory = "vignettes/images", extension = "png"),
  "gear",  "Transmission", "# Forward gears",       "rect",       "transmission_palette", lst(),
  "gear",  "Transmission", "",                      "text",       "black",              lst(overlay = TRUE)  
)

column_groups <- tribble(
  ~category,       ~group,          ~palette,
  "Performance",   "Performance",   "perf_palette",
  "Overall",       "Overall",       "overall_palette",
  "Engine",        "Engine",        "engine_palette",
  "Transmission",  "Transmission",  "transmission_palette"
)

palettes <- list(
  perf_palette = "Blues",
  overall_palette = "Greens",
  engine_palette = "YlOrBr",
  transmission_palette = "Reds",
  black = c("black", "black"),
  funky_palette_grey = RColorBrewer::brewer.pal(9, "Greys")[-1] %>% rev()
)

row_info <- data %>% transmute(id, group = ifelse(grepl("Merc", id), "Mercedes", "Other"))
# sort Mercedes cars to the top of the data and the row_info dataframe
data <- data[order(row_info$group), ]
row_info <- row_info[order(row_info$group), ]

row_groups <- tibble(level1 = c("Mercedes", "Other cars"), group = c("Mercedes", "Other"))

legends <- list(
    list(
        palette = "perf_palette",
        geom = "bar",
        title = "1/4 mile time",
        labels = c(paste0(min(data$qsec), "s"), rep("", 8), paste0(max(data$qsec), "s"))
    ),
    list(
        palette = "overall_palette",
        geom = "bar",
        title = "Miles per gallon",
        labels = c(paste0(min(data$mpg), "mpg"), rep("", 8), paste0(max(data$mpg), "mpg"))
    ),
    list(
        palette = "overall_palette",
        geom = "bar",
        title = "Weight",
        labels = c(paste0(min(data$wt), "lbs"), rep("", 8), paste0(max(data$wt), "lbs"))
    ),
    list(
        palette = "funky_palette_grey",
        geom = "funkyrect",
        title = "Overall",
        enabled = TRUE,
        labels = c("0", "", "0.2", "", "0.4", "", "0.6", "", "0.8", "", "1")
    ),
    list(
        palette = "engine_palette",
        enabled = FALSE
    ),
    list(
        palette = "transmission_palette",
        enabled = FALSE
    )
)

funky_heatmap(
  data,
  column_info = column_info,
  column_groups = column_groups,
  row_info = row_info,
  row_groups = row_groups,
  palettes = palettes,
  legends = legends
)

```

![An example of a `{funkyheatmap}` visualisation using the mtcars dataset.\label{fig-mtcars}](figure2.svg){width="12cm"}

# Conclusion
`{funkyheatmap}` streamlines the creation of publication-quality visualisation for mixed data types, empowering researchers and data scientists to communicate their results effectively. The development of `funkyheatmappy` (Python) and `funkyheatmapjs` (JavaScript) will further expand the accessibility and functionality of this visualisation solution.

# Acknowledgements
<!-- Acknowledgement of any financial support. -->
L.Z. acknowledges support from the Bavarian Ministry of Science and the Arts in the framework of the Bavarian Research Association “ForInter” (Interaction of human brain cells).
L.D. acknowledges support from the Research Foundation - Flanders (FWO) (1SF3822N). 
Y.S. acknowledges support from the Flemish Government under the “Onderzoeksprogramma Artificiële Intelligentie (AI) Vlaanderen” program.
A.C. and Y.S. are funded by an FWO TBM grant (Research Foundation – Flanders, T000119N), and Ghent University Special Research Fund (BOF18-GOA-024).
W.S. was supported by a Marie Skłodowska-Curie fellowship (101028476).

# References
