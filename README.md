# exceed.pipelines <a href="https://data.exceed.le.ac.uk/docs/exceedapi"><img src="man/figures/logo.png" align="right" height="139" /></a>

<!-- badges: start -->
[![R-CMD-check](https://github.com/legenepi/exceed-pipelines/workflows/R-CMD-check/badge.svg)](https://github.com/legenepi/exceed-pipelines/actions)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![test-coverage](https://github.com/legenepi/exceed-pipelines/workflows/test-coverage/badge.svg)](https://github.com/legenepi/exceed-pipelines/actions)
[![Codecov test coverage](https://codecov.io/gh/legenepi/exceed-pipelines/branch/main/graph/badge.svg)](https://app.codecov.io/gh/legenepi/exceed-pipelines?branch=main)
<!-- badges: end -->

The pipeline interface is designed to allow writing and sharing of reusable
code and minimizing the duplication of effort.

## Introduction

For an introduction to pipelines, please review the following:

-   [vignette on
    pipelines](https://dev.exceed.le.ac.uk/exceedapi/articles/data_pipelines_intro.html)
    in `exceedapi` package documentation.
-   [chapter 4](https://dev.exceed.le.ac.uk/exceedapi/tutorial/pipelines.html)
    on pipelines in `exceedapi-tutorial`.

## Getting started

By default, `exceedapi` will search for reusable steps in the current 
environment or in the main branch in 
[legenepi/exceed-pipelines](https://github.com/legenepi/exceed-pipelines) 
repository on github. No configuration is necessary but if you're loading steps 
from the GitHub repo then it's recommended that you create a 
[GitHub Personal Access Token (PAT)](https://github.com/settings/tokens/new) 
with `repo` scope, and set it using `gitcreds::gitcreds_set()`. For more
information on using GitHub Personal Access Token (PAT) in R, follow the guide 
at [Happy Git](https://happygitwithr.com/https-pat.html#tldr)

Pipelines can also loaded from a local directory or from any other github repo. 
For example, if you want to load steps from the `main` branch in a repo named 
`skyfall` under the account `bond`, you would create a .yaml config file with 
the following:

```{yaml}
pipeline:
  search:
  - bond/skyfall@main
  - legenepi/exceed-pipelines@main:R
```

This would add `bond/skyfall` repo as the first place where `exceedapi` would
look for steps, followed by the default `legenepi/exceed-pipelines`

You can specify the exact version to load from GitHub using the standard
GitHub syntax for referencing branches and tags. For example, to load a 
specific version from the default repo, you can create a config file with the
following:

```{yaml}
pipeline:
  search:
  - legenepi/exceed-pipelines@v0.4.0:R
```

You also need to decide whether you want this to be a per-project configuration
file or a global one. 

-   For per-project files, create an `exceedapi.yaml` in the top-level
    directory of your project.

-   For global configuration files create `.exceedapi.yaml` in your home
    directory.

In most cases, it's recommended to create a per-project configuration files, 
kept in separate directories.

NOTE: the name of configuration file is different if it's in the home directory
and must start with a dot `.`

## Installing the package

Installing the package is not required if you just want to load pipeline steps 
from GitHub directly. You could, however, install `exceed.pipelines` as a 
package but keep in mind you'd have to update it whenever new functionality is
added.

You can install the latest version of `exceed.pipelines` package from the 
development server:

``` r
install.packages("https://dev.exceed.le.ac.uk/exceed-pipelines/dist/exceed.pipelines.tar.gz")
```

All previous versions of the package are archived 
[here](https://dev.exceed.le.ac.uk/exceed-pipelines/dist?V=1).

