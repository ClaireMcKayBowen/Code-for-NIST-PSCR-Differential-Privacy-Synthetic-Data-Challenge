
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Code for NIST PSCR DIfferential Privacy Synthetic Data Challenge

This repo contains code used to generate all the results in the
*Comparative Study of Differentially Private Synthetic Data Algorithms
from the NIST PSCR Differential Privacy Synthetic Data Challenge* by
Bowen and Snoke. Paper can be found in arxiv
[here](https://arxiv.org/abs/1911.12704).

### Abstract

Differentially private synthetic data generation offers a recent
solution to release analytically useful data while preserving the
privacy of individuals in the data. In order to utilize these algorithms
for public policy decisions, policymakers need an accurate understanding
of these algorithms’ comparative performance. Correspondingly, data
practitioners also require standard metrics for evaluating the analytic
qualities of the synthetic data. In this paper, we present an in-depth
evaluation of several differentially private synthetic data algorithms
using actual differentially private synthetic data sets created by
contestants in the recent National Institute of Standards and Technology
Public Safety Communications Research (NIST PSCR) Division’s
\`\`Differential Privacy Synthetic Data Challenge.’’ We offer analyses
of these algorithms based on both the accuracy of the data they create
and their usability by potential data providers. We frame the methods
used in the NIST PSCR data challenge within the broader differentially
private synthetic data literature. We implement additional utility
metrics, including two of our own, on the differentially private
synthetic data and compare mechanism utility on three categories. Our
comparative assessment of the differentially private data synthesis
methods and the quality metrics shows the relative usefulness, general
strengths and weaknesses, preferred choices of algorithms and metrics.
Finally we describe the implications of our evaluation for policymakers
seeking to implement differentially private synthetic data algorithms on
future data products.

### Recommended Libraries

We recommend the following libraries to execute some of the code or
additional
    functions.

  - [synthpop](https://cran.r-project.org/web/packages/synthpop/synthpop.pdf)
    is a “tool for producing synthetic versions of microdata containing
    confidential information so that they are safe to be released to
    users for exploratory analysis”. This library contains the code for
    calculating *pMSE*-ratio.
  - [tidyverse](https://www.tidyverse.org/) is a suite of R packages by
    RStudio that help with data structure, data analysis, and data
    visualization.
  - [rpart](https://cran.r-project.org/web/packages/rpart/rpart.pdf) is
    a package to apply recursive partitioning for classificaiton,
    regression and survival trees.

### rcode Directory

This directory contains the `.R` scripts for the used in the paper
except for some basic functions that we will list in the next section.

### Commonly Used R Functions

The following is a list of R functions we used from other packages or in
base R to generate our results.

  - `utility.gen()` from `synthpop` calculates the *pMSE*-ratio and
    other utility metrics.
