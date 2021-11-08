---
editor_options: 
  markdown: 
    wrap: 72
---

# exceed.pipelines

The pipeline interface is designed to allow writing and sharing of
reusable code and minimizing the duplication of effort.

## What are pipelines

For an introduction to pipelines, please review the following:

-   [vignette on
    pipelines](https://dev.exceed.le.ac.uk/exceedapi/articles/data_pipelines_intro.html)
    in `exceedapi` package documentation.
-   [chapter
    4](https://dev.exceed.le.ac.uk/exceedapi/tutorial/pipelines.html) on
    pipelines in `exceedapi-tutorial`.

## Getting started

By default, `exceedapi` will search for reusable steps under a
subdirectory called `R` in the current working directory. This is
similar to how R packages load all `R/*.R` files in the package
directory, but pipelines go one step further. Pipelines can also load
files under nested subdirectories of `R` (e.g. `R/*/.../*.R`). You can
also override this behavior and provide a search path for pipelines. For
example, let's assume that you want `exceedapi` to load steps from
`exceed-pipelines` repository.

1.  First clone the repository.

```{=html}
<!-- -->
```
    $ git clone git@github.com:legenepi/exceed-pipelines.git ~/exceed/pipelines

    # -- or --

    $ git clone https://github.com/legenepi/exceed-pipelines.git ~/exceed/pipelines

1.  Next, create a configuration that tells the `exceedapi` where to
    look for pipeline steps. The configuration file must contain at
    least one directory under `pipeline -> search` option:

```{yaml}
pipeline:
  search:
    - ~/exceed/pipelines
```

1.  Finally, decide whether you want a per-project configuration file or
    a global one.

-   For per-project files, create an `exceedapi.yaml` in the top-level
    directory of your project.

-   For global configuration files create `.exceedapi.yaml` in your home
    directory.

NOTE: the name of configuration file is different if it's in the home
directory and must start with a dot \``` .` ``

## Advanced setup

To ensure reproducibility of your code over time you need to fix the
specific version of `exceed-pipelines` you're working with. One of the
simplest and reliable ways to do that is using [submodules with
git.](https://git-scm.com/book/en/v2/Git-Tools-Submodules)

TODO: add instructions for setting up submodules
