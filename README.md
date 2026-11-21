# neutral_news_replication
Replication package for the paper Neutral News as a Bridge to Broader Opinion Exposure
# Replication Package
## Neutral News as a Bridge to Broader Opinion Exposure

**Author:** Tereza Burýšková  
**Affiliation:** CERGE-EI

This repository contains the R code and supporting materials used to generate the numerical results and figures presented in the paper *Neutral News as a Bridge to Broader Opinion Exposure*.

The paper develops a theoretical model of information acquisition. All numerical data used in the analysis are generated directly from the model. No external or proprietary datasets are used.

## Repository contents

The repository contains the following files:

| File | Description |
|---|---|
| `expected utilities.R` | Computes and plots expected utilities across different two-source portfolios as a function of prior beliefs. |
| `two_concon.R` | Computes optimal source portfolios for different prior beliefs and utility function parameters. |
| `more_signals.R` | Extends the optimal source choice analysis to portfolios containing more signals and explores different source-precision parameters. |
| `optimal source choice.R` | Compares the optimality of different two-source portfolios across prior beliefs and source-precision parameters. |
| `intuition_concavity.R` | Illustrates belief updating and the associated utility changes following signals from one and two sources. |
| `manuscript.pdf` | PDF version of the manuscript. |

## Requirements

The scripts are written in R and use the following packages:

- `tidyverse`
- `ggplot2`
- `ggtern`
- `plotly`
- `readr`
- `gridExtra`
- `ggforce`

The `grid` package is also used and is included in base R.

## Instructions for replication

Each R script can be executed independently.

1. Download or clone this repository.
2. Open the relevant `.R` file in R or RStudio.
3. Install the required packages, if they are not already installed.
4. Run the script to generate the corresponding numerical results and figures.

The numerical calculations and data used for plotting are generated within the scripts. No additional input data files are required.

## Reproducibility

The scripts specify the model parameters and numerical grids used in the respective calculations. Where applicable, random seeds are set within the scripts.

The results can be reproduced by running the corresponding scripts using the specified R packages.

## Author responsibility

The author is responsible for the completeness and accuracy of the replication materials provided in this repository.
