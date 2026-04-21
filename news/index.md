# Changelog

## prefviz 0.1.2

- Added
  [`pairwise_calculator()`](https://numbats.github.io/prefviz/reference/pairwise_calculator.md)
  to compute all head-to-head comparisons from ranked preference data,
  returning a `"pairwise"` S3 object with TCP ratios, raw win counts,
  and Condorcet winner/loser detection.
- Added
  [`pairwise_heatmap()`](https://numbats.github.io/prefviz/reference/pairwise_heatmap.md)
  to visualise pairwise results as a red–green diverging heatmap, with
  tile annotations for TCP percentages or raw vote counts.
- Added
  [`dop_bar()`](https://numbats.github.io/prefviz/reference/dop_bar.md)
  to plot the preference distribution for a single contest round as a
  bar chart, supporting both wide and long input formats.
- `get_tern_data()` split into
  [`get_tern_data2d()`](https://numbats.github.io/prefviz/reference/ternary_getters.md)
  (2D plots) and
  [`get_tern_datahd()`](https://numbats.github.io/prefviz/reference/ternary_getters.md)
  (HD plots).
- [`get_tern_datahd()`](https://numbats.github.io/prefviz/reference/ternary_getters.md)
  now returns the full data frame with a `labels` column, replacing the
  need for
  [`get_tern_labels()`](https://numbats.github.io/prefviz/reference/ternary_getters.md).
- [`get_tern_labels()`](https://numbats.github.io/prefviz/reference/ternary_getters.md)
  is deprecated; use `get_tern_datahd(ternable)[["labels"]]` instead.

## prefviz 0.1.1

CRAN release: 2026-04-13

- Fixed
  [`helmert_transform()`](https://numbats.github.io/prefviz/reference/helmert_transform.md)
  to use
  [`geozoo::f_helmert()`](https://rdrr.io/pkg/geozoo/man/f_helmert.html)
  correctly.
- Updated package description to reflect expanded scope.
- Addressed CRAN reviewer comments: improved documentation and examples
  across multiple functions, added `data.R` documentation for bundled
  datasets, removed internal `sysdata.rda`.

## prefviz 0.1.0

- First release.
