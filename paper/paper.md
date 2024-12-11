---
title: "funkyheatmap: Visualising data frames with mixed data types"
tags:
  - benchmarking
  - visualisations

authors:
- name: Robrecht Cannoodt*
  orcid: 0000-0003-3641-729X
  email: robrecht@data-intuitive.com
  affiliations:
	- ref: di
	- ref: dambi
	- ref: twist
- name: Louise Deconinck*
  orcid: 0000-0001-8100-6823
  email: louise.deconinck@ugent.be
  affiliations:
    - ref: dambi
    - ref: twist
- name: Artuur Couckuyt*
  orcid: 0000-0001-7858-6521
  email: artuur.couckuyt@ugent.be
  affiliations:
    - ref: dambi
    - ref: twist
- name: Nikolay S. Markov*
  orcid: 0000-0002-3659-4387
  email: nikolai.markov@icloud.com
  affiliations:
    - ref: nu
- name: Luke Zappia
  orcid: 0000-0001-7744-8565
  affiliations:
    - ref: di
    - ref: icb
    - ref: tum-ma
- name: Malte D. Luecken
  orcid: 0000-0001-7464-7921
  affiliations:
    - ref: icb
    - ref: lhi
- name: Marta Interlandi
  orcid: 0000-0002-6863-2552
  email: marta.interlandi01@gmail.com
  affiliations:
    - ref: imi
- name: Yvan Saeys†
  orcid: 0000-0002-0415-1506
  email: yvan.saeys@ugent.be
  affiliations:
    - ref: dambi
    - ref: twist
- name: Wouter Saelens†
  orcid: 0000-0002-7114-6248
  email: w.saelens@epfl.ch
  affiliations:
    - ref: dambi
    - ref: twist

affiliations:
  - id: dambi
    department: Data Mining and Modelling for Biomedicine group
    name: VIB Center for Inflammation Research
    city: Ghent
    country: Belgium
  - id: twist
    department: Department of Applied Mathematics, Computer Science, and Statistics
    name: Ghent University
    city: Ghent
    country: Belgium
  - id: di
    name: Data Intuitive
    city: Lebbeke
    country: Belgium
  - id: epfl
    name: École Polytechnique Fédérale de Lausanne (EPFL)
    department: Institute of Bioengineering, School of Life Sciences
    city: Lausanne
    country: Switzerland
  - id: imi
    department: Institute of Medical Informatics
    name: University of Muenster
    city: Muenster
    country: Germany
  - id: nu
    name: Northwestern University
    department: Division of Pulmonary and Critical Care Medicine
    city: Chicago
    country: USA
  - id: icb
    name: Helmholtz Munich
    department: Institute of Computational Biology, Computational Health Center
    city: Munich
    country: Germany
  - id: tum-ma
    name: Technical University of Munich
    department: Department of Mathematics, School of Computation, Information and Technology
    city: Munich
    country: Germany
  - id: lhi
    name: Institute of Lung Health and Immunity (LHI), Helmholtz Munich, Comprehensive Pneumology Center (CPC-M), Germany; Member of the German Center for Lung Research (DZL)
    city: Munic
    country: Germany
engine: knitr
bibliography: paper.bib
---

*: Shared first authors, †: Shared last authors.

# funkyheatmap: Visualising data frames with mixed data types

## Summary

The `{funkyheatmap}` package offers a flexible and user-friendly solution for visualising data frames containing a mixture of categorical, proportional, and text-based data. It simplifies the creation of informative and visually appealing heatmaps while providing extensive customization options to tailor the output. This tool is especially valuable in research settings for summarising and communicating complex results, such as those encountered in benchmarking studies.

The package is available on [CRAN](https://cran.r-project.org/package=funkyheatmap) and [PyPI](https://pypi.org/project/funkyheatmappy/) and has a JavaScript port in development. For detailed examples and vignettes, visit the project website [funkyheatmap.github.io](https://funkyheatmap.github.io).

## Statement of need
Data visualisation is fundamental to exploratory data analysis and communicating findings. While powerful tools like ggplot2 [@ggplot2_wickham2009], Matplotlib [@matplotlib2dgraphics_hunter2007] and D3.js [@d3datadriven_bostock2011] exist, they often require complex scripting to generate comprehensive visualisations for data frames containing a mix of data types. `{funkyheatmap}` addresses this challenge by:

* **Seamless Handling of Mixed Data**: Automates the selection of appropriate visualisations (rectangles, bars, pie charts, text) based on data types.
* **Customization**: Provides granular control over colours, groupings, geometries, and annotations for tailored results.
* **Accessibility**: Offers a simplified interface for basic use and detailed documentation for advanced customization.

`{funkyheatmap}` has proven its utility in benchmarking studies within single-cell omics [@comparisonsinglecell_saelens2019; @benchmarkingatlaslevel_luecken2020; @benchmarkingintegrationmethods_yan2022; @spotlessreproduciblepipeline_sangaram2023; @definingbenchmarkingopen_luecken2023; @comprehensivebenchmarkingpractical_li2023], but its applications extend to diverse fields where visualisation of mixed data types is needed.

## Functionality
\autoref{fig-dynbenchmark-1} showcases the functionality of `{funkyheatmap}`, namely:

* **Diverse Geometries**: Supports a range of geometries (rectangles, bars, pie charts, text, images) to effectively represent different data types.
* **Hierarchical Categorical Grouping**: Facilitates the organisation of rows and columns into semantic groups with distinct colour palettes.
* **Documentation and Testing**: Includes comprehensive documentation, vignettes, and a test suite for quality and ease of use.

![An example of a `{funkyheatmap}` visualisation using data from a benchmarking study of trajectory inference methods [@comparisonsinglecell_saelens2019].\label{fig-dynbenchmark-1}](utils_files/figure-gfm/fig-dynbenchmark-1.png)

See \autoref{tbl-geoms} for more information regarding the recommended geom for different types of data.

| Data type         | Example                          | Recommended geom    |
|:------------------|:---------------------------------|:--------------------|
| Numerical data    | Scores from 0 to 1               | funkyrect           |
| Aggregated data   | The mean of scores               | bar                 |
| Measurement data  | 3MB or 4h                        | rect + text overlay |
| Categorical data  | R or Python                      | text or image       |
| Proportional data | 80% success, 10% OOM, 10% failed | pie                 |

Recommended geometries in `{funkyheatmap}` for different data types. The table presents the suggested visualisation methods (geoms) based on the data type of the columns. These recommendations provide a starting point for users to select the most appropriate visual representation for their specific data. \label{tbl-geoms}

# Conclusion
`{funkyheatmap}` streamlines the creation of publication-quality visualisation for mixed data types, empowering researchers and data scientists to communicate their results effectively. The ongoing development of `funkyheatmappy` (Python) and `funkyheatmapjs` (JavaScript) will further expand the accessibility and functionality of this visualisation solution.

# Acknowledgements
<!-- Acknowledgement of any financial support. -->
L.Z. acknowledges support from the Bavarian Ministry of Science and the Arts in the framework of the Bavarian Research Association “ForInter” (Interaction of human brain cells).
L.D. acknowledges support from the Research Foundation - Flanders (FWO) (1SF3822N). 
Y.S. acknowledges support from the Flemish Government under the “Onderzoeksprogramma Artificiële Intelligentie (AI) Vlaanderen” program.
A.C. and Y.S. are funded by an FWO TBM grant (Research Foundation – Flanders, T000119N), and Ghent University Special Research Fund (BOF18-GOA-024).
W.S. was supported by a Marie Skłodowska-Curie fellowship (101028476).


