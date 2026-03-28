# Reproducibility Challenge Plots

This repository contains data and R scripts used to generate figures for the CAMARADES reproducibility challenge manuscript. 

## Overview

The `data/` directory contains both `Provided` (i.e., raw) and `Entered` (i.e., derived) data from each of the 5 participating teams.

There are separate analysis scripts for producing figures comparing original systematic reviews and their replications, including:

- Study retrieval overlap  (`study_retrieval_plots.R`)
- Risk-of-bias score concordance (`rob_plots.R`)
- Meta-analysis effect size comparisons (`meta_analysis_plots.R`)

All figures are saved in the `plots/` directory.

This repository also containts a script (`replication.R`) to apply the replication criteria suggested by [Hamilton et al (2025)](https://doi.org/10.1017/rsm.2025.10064) to the meta-analysis results 
