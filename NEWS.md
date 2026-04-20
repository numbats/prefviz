# prefviz 0.1.2

* `get_tern_data()` split into `get_tern_data2d()` (2D plots) and `get_tern_datahd()` (HD plots).
* `get_tern_datahd()` now returns the full data frame with a `labels` column, replacing the need for `get_tern_labels()`.
* `get_tern_labels()` is deprecated; use `get_tern_datahd(ternable)[["labels"]]` instead.
* Added `lifecycle` dependency.

# prefviz 0.1.1

* Fixed `helmert_transform()` to use `geozoo::f_helmert()` correctly.
* Updated package description to reflect expanded scope.
* Addressed CRAN reviewer comments: improved documentation and examples across multiple functions, added `data.R` documentation for bundled datasets, removed internal `sysdata.rda`.

# prefviz 0.1.0

* First release.
