# cmor.tools 0.6.0.9000

* New `plot_cmor_colours()` plots the available colour schemes for
  `scale_colour_cmor()` etc.

* Correct extraction of CMOR colour palettes.

# cmor.tools 0.6.0

* Deprecate `create_descriptive_table()` in favour of `formattr` package.

* `tar_render_manuscript()` now correctly uses Pandoc's --citeproc option.

* Updated `manuscript.Rmd` template.

* Updated initial project setup with `create_research_project()`

# cmor.tools 0.5.0

* Deprecated output formatting functions `number()` etc. in favour of the
  `formattr` package.

* New unit tests for several functions.

# cmor.tools 0.4.0

* New `scale_colour_cmor()` and friends provide standard colour schemes.

* Preliminary new `theme_cmor()` will provide a standard plot theme. Currently
  only calls `ggplot2::theme_classic()`.

# cmor.tools 0.3.0

* Deprecated `sf6d_profile()` and `sf6d_utility()` in favour of the
    `SF6Dvalues` package.

* Added a `NEWS.md` file to track changes to the package.
