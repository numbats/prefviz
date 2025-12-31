#' Transform compositional data using Helmert matrix
#'
#' @description 
#' Transform n-dimension compositional data (all values sum to 1) to its (n-1)-dimensional space
#' using the Helmert matrix. 
#' 
#' @param data A data frame or matrix containing the compositional data. 
#' @param alternatives (Optional) A character or numeric vector specifying the columns of compositional data to use. 
#'  If `NULL` (default), all columns are used.
#' @param alternatives <[`tidy-select`][dplyr::dplyr_tidy_select]> Columns of compositional data to
#'   use. Default is [everything()], which selects all columns. Must select at least 3 columns.
#' @param append (Optional) A logical value indicating whether the transformed data should be appended to the original data frame. 
#'  Default is `FALSE`.
#' 
#' @return A data frame with the original columns and additional columns 
#' containing the Helmert-transformed coordinates, named `x1`, `x2`, ..., `x(n-1)` where `n` is 
#' the number of dimensions in the compositional data.
#' 
#' @examples
#' \dontrun{
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
#' helmert_transform(df, alternatives = c(ALP, LNP, Other))
#'}
helmert_transform <- function(data, alternatives = everything(), append = FALSE) {
  stopifnot(is.data.frame(data) || is.matrix(data))

  input_df <- data.frame(data)

  alternative_col_ind <- tidyselect::eval_select(
      rlang::enquo(alternatives), 
      input_df)
  alternative_col_chr <- colnames(input_df)[alternative_col_ind]

  # Validate compositional data
  validate_df <- validate_ternable(input_df, alternative_col_chr)

  # Helmert transformation
  input_mat <- validate_df[, alternative_col_chr, drop = FALSE] |> as.matrix()
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
