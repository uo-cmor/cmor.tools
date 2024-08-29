# cmor.tools 1.6

* Update `CMORprojects` to version 0.7

* Update `ttables` to version 0.3

# cmor.tools 1.5

* Add `ttables` version 0.1

* Update `CMORprojects` to version 0.5

# cmor.tools 1.4

* Update `CMORprojects` to version 0.4

# cmor.tools 1.3.1

* Update `SF6Dvalues` to version 0.6.1

# cmor.tools 1.3.0

* Update `SF6Dvalues` to version 0.6.0

# cmor.tools 1.2.5

* Update `CMORprojects` to version 0.3.5

# cmor.tools 1.2.4

* Update `CMORprojects` to version 0.3.4

# cmor.tools 1.2.3

* Update `CMORprojects` to version 0.3.3

* Update `formattr` to version 0.3.1

# cmor.tools 1.2.2

* Update `CMORprojects` to version 0.3.2

# cmor.tools 1.2.1

* Update `CMORprojects` to version 0.3.1

# cmor.tools 1.2.0

* Update `CMORprojects` to version 0.3.0

# cmor.tools 1.1.0

* Update `formattr` to version 0.3.0

# cmor.tools 1.0.0

* Remove defunct functions and provide links to their new locations.

* Update DESCRIPTION & README to describe the new package structure.

# cmor.tools 0.9.0

* Restructure `cmor.tools` to install and load the CMOR tools ecosystem.

* Move project workflow tools to `CMORprojects` and regression analysis tools
  to `regtools`.

# cmor.tools 0.8.0

* Hard deprecate defunct functions and remove unneeded dependencies.

# cmor.tools 0.7.0

* Deprecate plot themes and colour palettes in favour of `CMORplots` package.

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
