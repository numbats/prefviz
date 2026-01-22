#' Transform compositional data using Helmert matrix
#'
#' @description 
#' Transform n-dimension compositional data (all values sum to 1) into an (n-1)-dimensional Euclidean space
#' using the Helmert matrix. This dimension reduction is the geometric basis for 
#' plotting points within the simplex.
#' 
#' @param data A data frame or matrix containing the compositional data. 
#' @param items <[`tidy-select`][dplyr::dplyr_tidy_select]> Columns representing the 
#'   components of the composition. Default is [everything()], which selects all columns. 
#'   Must select at least 3 columns.
#' @param append (Optional) A logical value indicating whether the transformed data should be appended to the original data frame. 
#'  Default is `FALSE`.
#' 
#' @return A data frame containing the Helmert-transformed coordinates, named 
#'   `x1`, `x2`, ..., `x(n-1)`, where `n` is the number of items. If `append = TRUE`, 
#'   these columns are added to the input `data`.
#' 
#' @examples
#' # Example 1: Transform a matrix (all columns)
#' comp_mat <- matrix(c(0.5, 0.3, 0.2,
#'                      0.4, 0.4, 0.2,
#'                      0.6, 0.2, 0.2),
#'                    ncol = 3, byrow = TRUE)
#' helmert_transform(comp_mat)
#' 
#' # Example 2: Transform specific columns in a data frame
#' df <- data.frame(
#'   electorate = c("A", "B", "C"),
#'   ALP = c(0.5, 0.4, 0.6),
#'   LNP = c(0.3, 0.4, 0.2),
#'   Other = c(0.2, 0.2, 0.2)
#' )
#' helmert_transform(df, items = c(ALP, LNP, Other))
#'
#' @export
helmert_transform <- function(data, items = dplyr::everything(), append = FALSE) {
  stopifnot(is.data.frame(data) || is.matrix(data))

  input_df <- data.frame(data)

  item_col_ind <- tidyselect::eval_select(
      rlang::enquo(items), 
      input_df)
  item_col_chr <- colnames(input_df)[item_col_ind]

  # Validate compositional data
  validate_df <- validate_ternable(input_df, item_col_chr)

  # Helmert transformation
  input_mat <- validate_df[, item_col_chr, drop = FALSE] |> as.matrix()
  cart_output <- geozoo::f_composition(input_mat)
  colnames(cart_output) <- paste0("x", seq_len(ncol(cart_output)))
  
  # Combine with original data
  if (append) {
    res <- cbind(data.frame(data), data.frame(cart_output))
  } else {
    res <- data.frame(cart_output)
  }

  return(res)
  }
