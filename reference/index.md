# Package index

## Draw a 2D ternary plots

Main plotting functions for ternary diagrams

- [`geom_ternary_cart()`](https://numbats.github.io/prefviz/reference/geom_ternary_cart.md)
  : Draw the 2D ternary simplex
- [`geom_ternary_region()`](https://numbats.github.io/prefviz/reference/geom_ternary_region.md)
  [`stat_ternary_region()`](https://numbats.github.io/prefviz/reference/geom_ternary_region.md)
  [`StatTernaryRegion`](https://numbats.github.io/prefviz/reference/geom_ternary_region.md)
  : Create polygonal regions in a ternary plot based on a reference
  point
- [`add_vertex_labels()`](https://numbats.github.io/prefviz/reference/add_vertex_labels.md)
  : Add vertex labels to ternary plot
- [`stat_ordered_path()`](https://numbats.github.io/prefviz/reference/stat_ordered_path.md)
  [`StatOrderedPath`](https://numbats.github.io/prefviz/reference/stat_ordered_path.md)
  : Reorder observations for a path geom

## Ternable object

Create a `ternable` object and prepare it for plotting

- [`ternable()`](https://numbats.github.io/prefviz/reference/ternable.md)
  : Create a ternable object
- [`get_tern_data()`](https://numbats.github.io/prefviz/reference/ternary_getters.md)
  [`get_tern_edges()`](https://numbats.github.io/prefviz/reference/ternary_getters.md)
  [`get_tern_labels()`](https://numbats.github.io/prefviz/reference/ternary_getters.md)
  : Getter functions to extract components from ternable object for
  ternary plots

## Data transformation

Prepare your PrefLib and distribution of preferences data for plotting

- [`dop_transform()`](https://numbats.github.io/prefviz/reference/dop_transform.md)
  : Transform AEC distribution of preferences from long to wide format
- [`dop_irv()`](https://numbats.github.io/prefviz/reference/dop_irv.md)
  : Get full distribution of preferences in each instant runoff voting
  round as percentage
- [`helmert_transform()`](https://numbats.github.io/prefviz/reference/helmert_transform.md)
  : Transform compositional data using Helmert matrix

## Data

- [`aecdop_2022`](https://numbats.github.io/prefviz/reference/aecdop.md)
  [`aecdop_2025`](https://numbats.github.io/prefviz/reference/aecdop.md)
  : Distribution of preferences by candidate by division in the
  Australian Federal Election (2022 and 2025)
- [`elb_centroid`](https://numbats.github.io/prefviz/reference/elb_centroid.md)
  : Centroids of electoral divisions in the 2025 Australian Federal
  Election
- [`elb_map`](https://numbats.github.io/prefviz/reference/elb_map.md) :
  Electoral boundaries map for the 2025 Australian Federal Election
