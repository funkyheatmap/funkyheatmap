#!/bin/bash

viash ns build --setup cb --parallel

# make executable
EXE_DIR=target/executable/funky_heatmap
$EXE_DIR/funky_heatmap ---setup push ---engine docker
quarto render vignettes/executable.qmd --output - --to gfm > $EXE_DIR/README.md
cp bin/data.tsv $EXE_DIR/example_data.tsv
pushd $EXE_DIR; zip ../executable.zip *; popd

# make nextflow module
NF_DIR=target/nextflow/funky_heatmap
cp bin/data.tsv $NF_DIR/example_data.tsv
pushd $NF_DIR; zip ../nextflow.zip *; popd