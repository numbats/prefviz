# Reorder observations for a path geom

`stat_ordered_path()` reorders observations along each path using a
user-supplied ordering aesthetic (`order_by`) before drawing the path.
The statistic can be used to ensure that paths are drawn in a consistent
order even when the input data are not pre-sorted. This is equivalent to
reordering the data before passing it to
[`geom_path()`](https://ggplot2.tidyverse.org/reference/geom_path.html).

## Usage

``` r
stat_ordered_path(
  mapping = NULL,
  data = NULL,
  geom = "path",
  position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  decreasing = TRUE,
  na_method = c("drop_na", "drop_group"),
  ...
)

StatOrderedPath
```

## Format

An object of class `StatOrdered` (inherits from `Stat`, `ggproto`, `gg`)
of length 4.

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).
  Must at least supply `x`, `y`, and `order_by`. The `group` aesthetic
  can be used to define separate paths.

- data:

  Data frame to be used for this layer. If `NULL`, the default, the data
  is inherited from the plot.

- geom:

  The geometric object to use to draw the paths. Defaults to `"path"`.

- position:

  A position adjustment to use on the data for this layer. This can be
  used in various ways, including to prevent overplotting and improving
  the display. The `position` argument accepts the following:

  - The result of calling a position function, such as
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html).
    This method allows for passing extra arguments to the position.

  - A string naming the position adjustment. To give the position as a
    string, strip the function name of the `position_` prefix. For
    example, to use
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html),
    give the position as `"jitter"`.

  - For more information and other ways to specify the position, see the
    [layer
    position](https://ggplot2.tidyverse.org/reference/layer_positions.html)
    documentation.

- show.legend:

  Logical or `NA`. Should this layer be included in the legends? `NA`,
  the default, includes the layer if any aesthetics are mapped.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them.

- decreasing:

  Logical. If `TRUE`, paths are ordered in decreasing order of
  `order_by`. If `FALSE` (default), ordering is increasing.

- na_method:

  Character string specifying how to handle missing values in
  `order_by`. One of:

  - `"drop_na"` (default): drop only rows where `order_by` is `NA`;

  - `"drop_group"`: drop entire groups that contain any `NA` in
    `order_by`.

- ...:

  Additional parameters passed on to the underlying geom.

## Value

A ggplot2 layer that can be added to a plot object.

## Details

The statistic expects an `order_by` aesthetic that supplies the variable
used to order observations within each group.

Missing values `order_by` are handled according to `na_method`. If
`na_method` is "drop_na", missing values are dropped from the data, and
the path is still drawn, but might skipping steps due to missing values.
If `na_method` is "drop_group", the entire group whose missing values
belong is dropped from the data, and the path is not drawn.

Ties in `order_by` are allowed but trigger a warning and preserve the
original row order for tied values.

Duplicates in `order_by` are dropped and a warning is issued.

Grouping is controlled via the usual `group` aesthetic. If a `group`
column is present in the data, reordering is performed independently
within each group; otherwise, the entire data is treated as a single
path.

## Aesthetics

`stat_ordered_path()` understands the following aesthetics (required are
in bold).

- **x**

- **y**

- **order_by**

- group

- alpha, colour, linewidth, linetype, etc. (inherited from
  [`ggplot2::geom_path()`](https://ggplot2.tidyverse.org/reference/geom_path.html))

## Examples

``` r
library(ggplot2)
library(dplyr)
# Data prep
input_df <- prefviz:::aecdop22_widen |> 
   filter(DivisionNm %in% c("Higgins", "Monash"))
tern22 <- ternable(input_df, ALP:Other)

# Base plot
p <- get_tern_data(tern22, plot_type = "2D") |> 
  ggplot(aes(x = x1, y = x2)) +
  geom_ternary_cart() +
  geom_ternary_region(
    aes(fill = after_stat(vertex_labels)),
    vertex_labels = tern22$vertex_labels,
    alpha = 0.3, color = "grey50",
    show.legend = FALSE
  ) +
  geom_point(aes(color = ElectedParty)) +
  add_vertex_labels(tern22$simplex_vertices) +
  scale_color_manual(
    values = c("ALP" = "red", "LNP" = "blue", "Other" = "grey70"),
    aesthetics = c("fill", "colour")
  )

# Add ordered paths
p + 
  stat_ordered_path(
    aes(group = DivisionNm, order_by = CountNumber, color = ElectedParty), 
    size = 0.5)
#> Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` instead.

```
