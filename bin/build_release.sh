#!/bin/bash

# make executable zip
viash build viash_src/config.vsh.yaml -p docker -o bin/docker --setup cachedbuildifneedbe
bin/docker/funky_heatmap ---setup pushifneedbe
quarto render vignettes/executable.Rmd --output - --to gfm > bin/docker/README.md
cp bin/data.tsv bin/docker/example_data.tsv
pushd bin/docker; zip ../executable.zip *; popd
# zip bin/executable.zip bin/docker/*

# make nextflow module
viash build viash_src/config.vsh.yaml -p nextflow -o bin/nextflow
quarto render vignettes/nextflow.Rmd --output - --to gfm > bin/nextflow/README.md
cp bin/data.tsv bin/nextflow/example_data.tsv
# zip bin/nextflow.zip bin/nextflow/*
pushd bin/nextflow; zip ../nextflow.zip *; popd