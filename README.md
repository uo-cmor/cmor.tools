
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cmor.tools

<!-- badges: start -->
<!-- badges: end -->

The goal of cmor.tools is to provide standardised tools to initialise a
research project folder, undertake analysis, and generate output reports
(journal manuscripts, etc.) for research projects undertaken at CMOR.

## Installation

You can install cmor.tools from [GitHub](https://github.com) with:

``` r
#install.packages("remotes")
remotes::install_github("nek-rwl/cmor.tools")
```

## Components

This section will describe the various main components included in the
package.

-   **create\_research\_project** This function (and related
    sub-routines) set up an R package, directory structure, and template
    files to hold the data, code, and output from a research project.
    This work is closely based on Ben Marwick’s `rrtools` package.

-   **tar\_render\_manuscript** This function is used in `targets` plans
    to generate rendered `.docx` output from a `.Rmd` file.
