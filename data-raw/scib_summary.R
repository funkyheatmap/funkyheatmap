metrics_url <- "https://github.com/theislab/scib-reproducibility/raw/main/visualization/data/metrics_RNA_allTasks.csv"

metrics_raw <- readr::read_csv(
  metrics_url,
  col_names = c(
    "path",
    "NMI_cluster_label",
    "ARI_cluster_label",
    "ASW_label",
    "ASW_label_batch",
    "PCR_batch",
    "cell_cycle",
    "isolated_label_F1",
    "isolated_label_silhouette",
    "graph_connectivity",
    "kBET",
    "iLISI",
    "cLISI",
    "HVG_overlap",
    "trajectory"
  ),
  col_types = readr::cols(
    path     = readr::col_character(),
    .default = readr::col_double()
  ),
  skip = 1
)

labels <- list(
  methods = c(
    "BBKNN"          = "bbknn",
    "Conos"          = "conos",
    "trVAE"          = "trvae",
    "scVI"           = "scvi",
    "ComBat"         = "combat",
    "Harmony"        = "harmony",
    "LIGER"          = "liger",
    "Scanorama"      = "scanorama",
    "Seurat v3 CCA"  = "seurat",
    "Seurat v3 RPCA" = "seuratrpca",
    "MNN"            = "mnn",
    "FastMNN"        = "fastmnn",
    "scGen*"         = "scgen",
    "scANVI*"        = "scanvi",
    "DESC"           = "desc",
    "SAUCIE"         = "saucie",
    "Unintegrated"   = "unintegrated"
  ),
  metrics = list(
    batch = c(
      "Batch correction"   = "batch_correction",
      "PCR batch"          = "PCR_batch",
      "Batch ASW"          = "ASW_label_batch",
      "Graph iLISI"        = "iLISI",
      "Graph connectivity" = "graph_connectivity",
      "kBET"               = "kBET"
    ),
    bio = c(
      "Bio conservation"          = "bio_conservation",
      "NMI cluster/label"         = "NMI_cluster_label",
      "ARI cluster/label"         = "ARI_cluster_label",
      "Label ASW"                 = "ASW_label",
      "Isolated label F1"         = "isolated_label_F1",
      "Isolated label silhouette" = "isolated_label_silhouette",
      "Graph cLISI"               = "cLISI",
      "HVG conservation"          = "HVG_overlap",
      "Cell cycle conservation"   = "cell_cycle",
      "Trajectory conservation"   = "trajectory"
    )
  )
)

metrics <- metrics_raw |>
  # Remove empty rows from failed runs
  dplyr::filter(!is.na(NMI_cluster_label)) |>
  # Remove leading / if present
  dplyr::mutate(path = stringr::str_remove(path, "^/")) |>
  # Split the path into the different parts of the scenario
  tidyr::separate(
    path,
    into = c("dataset", NA, "scaling", "features", "method"),
    sep = "/"
  ) |>
  # Remove results for trvae_full (reconstruction not integration)
  dplyr::filter(method != "trvae_full") |>
  # Split method into method and output
  tidyr::separate(method, into = c("method", "output"), sep = "_") |>
  # Set factors with pretty labels
  dplyr::mutate(
    dataset = factor(dataset),
    scaling = factor(
      scaling,
      levels = c("scaled", "unscaled"),
      labels = c("Scaled", "Unscaled")
    ),
    features = factor(
      features,
      levels = c("full_feature", "hvg"),
      labels = c("Full", "HVG")
    ),
    method = factor(
      method,
      levels = labels$methods,
      labels = names(labels$methods)
    ),
    output = factor(
      output,
      levels = c("full", "embed", "knn"),
      labels = c("Features", "Embedding", "Graph")
    )
  ) |>
  # Create additional useful columns
  dplyr::mutate(
    input       = paste(dataset, scaling, features, sep = "-"),
    full_method = paste(method, output, sep = "-"),
    scenario    = paste(input, full_method, sep = "|")
  ) |>
  # Rescale scores by dataset
  dplyr::group_by(dataset) |>
  dplyr::mutate(
    dplyr::across(
      tidyselect::where(is.numeric),
      function (x) {
        if (all(is.na(x))) {
          return(x)
        }
        scales::rescale(x, to = c(0, 1))
      }
    )
  ) |>
  dplyr::ungroup() |>
  # Calculate overall scores
  dplyr::rowwise() |>
  dplyr::mutate(
    batch_correction = mean(
      c(PCR_batch, ASW_label_batch, iLISI, graph_connectivity, kBET),
      na.rm = TRUE
    ),
    bio_conservation = mean(
      c(
        NMI_cluster_label, ARI_cluster_label, ASW_label,
        isolated_label_F1, isolated_label_silhouette, cLISI,
        HVG_overlap, cell_cycle, trajectory
      ),
      na.rm = TRUE
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    overall = 0.4 * batch_correction + 0.6 * bio_conservation
  ) |>
  # Reorder columns
  dplyr::relocate(
    scenario, input, dataset, scaling, features, full_method, method,
    output, overall, batch_correction, PCR_batch, ASW_label_batch, iLISI,
    graph_connectivity, kBET, bio_conservation, NMI_cluster_label,
    ARI_cluster_label, ASW_label, isolated_label_F1,
    isolated_label_silhouette, cLISI, HVG_overlap, cell_cycle,
    trajectory
  )

scores <- metrics |>
  # Combine method and imput features
  dplyr::mutate(
    method_input = paste(method, output, features, scaling, sep = "-")
  ) |>
  # Select columns
  dplyr::select(method_input, dataset, overall) |>
  # Add missing method/dataset combinations
  tidyr::complete(method_input, dataset) |>
  # Fill missing scores with unintegrated
  dplyr::group_by(dataset) |>
  dplyr::mutate(
    overall_imputed = dplyr::if_else(
      is.na(overall),
      overall[method_input == "Unintegrated-Features-Full-Unscaled"],
      overall
    ),
  ) |>
  # Rank overall scores
  dplyr::mutate(rank = rank(-overall_imputed)) |>
  dplyr::select(-overall_imputed) |>
  # Calculate average ranks (ignoring simulations)
  dplyr::group_by(method_input) |>
  dplyr::mutate(
    avg_rank = mean(rank[!startsWith(as.character(dataset), "simulation")])
  ) |>
  # Filter to best input feature set
  tidyr::separate_wider_delim(
    method_input,
    names = c("method", "output", "features", "scaling"),
    delim = "-"
  ) |>
  dplyr::mutate(full_method = paste(method, output, sep = "-")) |>
  dplyr::group_by(full_method) |>
  dplyr::slice_min(avg_rank) |>
  dplyr::ungroup() |>
  dplyr::select(-full_method) |>
  # Make columns for each dataset
  tidyr::pivot_wider(
    id_cols = c(method, output, features, scaling, avg_rank),
    names_from = dataset,
    values_from = c(overall, rank)
  ) |>
  dplyr::arrange(avg_rank)

usability_url <- "https://github.com/theislab/scib-reproducibility/raw/main/visualization/data/usability4bestMethods.csv"
usability <- readr::read_csv(
  usability_url,
  col_select = 2:5,
  col_names = c(
    "row",
    "method",
    "usability",
    "package_score",
    "paper_score"
  ),
  col_types = readr::cols(
    method = readr::col_character(),
    .default = readr::col_number()
  ),
  skip = 1
) |>
  # Select which ComBat/MNN entry to use
  dplyr::filter(!(method %in% c("ComBat", "MNN"))) |>
  # Adjust some method names
  dplyr::mutate(
    method = dplyr::case_when(
      method == "ComBat (Scanpy)" ~ "ComBat",
      method == "MNNpy" ~ "MNN",
      method == "Seurat v3" ~ "Seurat v3 CCA",
      method == "scANVI" ~ "scANVI*",
      method == "scGen" ~ "scGen*",
      method == "TrVAE" ~ "trVAE",
      method == "CONOS" ~ "Conos",
      TRUE ~ method
    )
  )

is_seurat <- usability$method == "Seurat v3 CCA"
usability <- usability |>
  # Add a second Seurat entry
  dplyr::bind_rows(
    data.frame(
      method = "Seurat v3 RPCA",
      usability = usability$usability[is_seurat],
      package_score = usability$package_score[is_seurat],
      paper_score = usability$paper_score[is_seurat]
    )
  ) |>
  # Add ranks
  dplyr::mutate(
    package_rank = rank(-package_score),
    paper_rank = rank(-paper_score)
  ) |>
  dplyr::select(method, package_score, package_rank, paper_score, paper_rank)

scalability_time_url <- "https://github.com/theislab/scib-reproducibility/raw/main/visualization/data/scalability_score_time.csv"

scalability_time <- readr::read_csv(
  scalability_time_url,
  col_names = c("row", "path", "time_score_raw", "time_score"),
  col_select = c("path", "time_score"),
  col_types = readr::cols(
    path = readr::col_character(),
    .default = readr::col_number()
  ),
  skip = 1
) |>
  tidyr::separate_wider_delim(
    path,
    names = c("scaling", "features", "method"),
    delim = "/"
  ) |>
  # Set factors with pretty labels
  dplyr::mutate(
    scaling = factor(
      scaling,
      levels = c("scaled", "unscaled"),
      labels = c("Scaled", "Unscaled")
    ),
    features = factor(
      features,
      levels = c("full_feature", "hvg"),
      labels = c("Full", "HVG")
    ),
    method = factor(
      method,
      levels = labels$methods,
      labels = names(labels$methods)
    )
  ) |>
  dplyr::mutate(time_rank = rank(-time_score))

scalability_mem_url <- "https://github.com/theislab/scib-reproducibility/raw/main/visualization/data/scalability_score_memory.csv"

scalability_mem <- readr::read_csv(
  scalability_mem_url,
  col_names = c("row", "path", "memory_score_raw", "memory_score"),
  col_select = c("path", "memory_score"),
  col_types = readr::cols(
    path = readr::col_character(),
    .default = readr::col_number()
  ),
  skip = 1
) |>
  tidyr::separate_wider_delim(
    path,
    names = c("scaling", "features", "method"),
    delim = "/"
  ) |>
  # Set factors with pretty labels
  dplyr::mutate(
    scaling = factor(
      scaling,
      levels = c("scaled", "unscaled"),
      labels = c("Scaled", "Unscaled")
    ),
    features = factor(
      features,
      levels = c("full_feature", "hvg"),
      labels = c("Full", "HVG")
    ),
    method = factor(
      method,
      levels = labels$methods,
      labels = names(labels$methods)
    )
  ) |>
  dplyr::mutate(memory_rank = rank(-memory_score))

scib_summary <- scores |>
  dplyr::left_join(usability, by = "method") |>
  dplyr::left_join(scalability_time, by = c("scaling", "features", "method")) |>
  dplyr::left_join(scalability_mem, by = c("scaling", "features", "method"))

usethis::use_data(scib_summary, overwrite = TRUE)
