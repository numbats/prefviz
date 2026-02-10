input_df <-  pref25_2d[, c("ALP", "LNP", "Other")]
validate_df <- validate_ternable(input_df, c("ALP", "LNP", "Other"))
input_df <- validate_df |> as.matrix()

cart_output <- geozoo::f_composition(input_mat)
colnames(cart_output) <- paste0("x", seq_len(ncol(cart_output)))

# Combine with original data
if (append) {
  res <- bind_cols(as_tibble(input_df), as_tibble(cart_output))
} else {
  res <- tibble(cart_output)
}

res
