#!/usr/bin/env bash
set -e

cd /app

Rscript -e "shiny::runApp('project', host = Sys.getenv('SHINY_HOST', '0.0.0.0'), port = as.integer(Sys.getenv('SHINY_PORT', '3838')))"
