
<!-- README.md is generated from README.Rmd. Please edit that file -->
learningCurve
=============

**Author:** [Brad Boehmke](http://bradleyboehmke.github.io/) & [Jason Freels](https://github.com/Auburngrads)<br/> **License:** [GPL-3.0](https://opensource.org/licenses/GPL-3.0)

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/learningCurve)](https://cran.r-project.org/package=learningCurve) [![Travis-CI Build Status](https://travis-ci.org/bradleyboehmke/learningCurve.svg?branch=master)](https://travis-ci.org/bradleyboehmke/learningCurve)

`learningCurve` is an R package implements common learning curve production functions. It incorporates Crawford's and Wright's learning curve functions to compute unit and cumulative block estimates for time (or cost) of units along with an aggregate learning curve. It also provides delta and error functions and some basic learning curve plotting functions.along with functions to compute aggregated learning curves, error rates, and to visualize learning curves.

Installation
------------

You can install `learningCurve` two ways.

-   Using the latest released version from CRAN:

<!-- -->

    install.packages("learningCurve")

-   Using the latest development version from GitHub:

<!-- -->

    if (packageVersion("devtools") < 1.6) {
      install.packages("devtools")
    }

    devtools::install_github("Auburngrads/learningCurve")

Learning
--------

To get started with `learningCurve`, read the intro vignette: `vignette("learningCurve", package = "learningCurve")`. This will provide a thorough introduction to the functions provided in the package.
