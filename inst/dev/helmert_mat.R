library(palmerpenguins)
library(geozoo)
library(randomForest)
library(tidyverse)
library(tourr)
library(detourr)

source("inst/dev/poc1.R")

helmert_transform <- function(data, alternatives = NULL){
  # Check input type
  if(is.matrix(data) + is.data.frame(data) < 1){
    stop("Input must be a matrix or a data frame")
  }

  # Convert input to matrix
  if(is.null(alternatives)){
    input_mat <- as.matrix(data)
  } else{
    input_mat <- as.matrix(data[, alternatives])
  }

  # Check if the rows sum to 1, if not, normalize and give warnings
  if (sum(near(rowSums(input_mat), 1)) != nrow(input_mat)) {
    warning("Input does not sum to 1, normalizing")
    input_mat <- input_mat / rowSums(input_mat)
  }

  # Helmert transformation
  output_mat <- f_composition(input_mat)
  colnames(output_mat) <- paste0("x", 1:ncol(output_mat))

  # Plug back to the dataset
  res <- data |>
    cbind(cart_output)

  return(res)
}

### Plot 2D ternary

helm <- f_composition(as.matrix(pref_2025[, c("ALP", "LNP", "Other")]))
colnames(helm) <- paste0("x", 1:ncol(helm))
res <- pref_2025 |>
  cbind(helm)

# Define the simplex. Input columns of alternatives ElectedParty
alt <- res$ElectedParty |> unique()
simp <- simplex(p=length(alt)-1)
sp <- data.frame(simp$points)
colnames(sp) <- paste0("x", 1:length(sp))
sp$ElectedParty <- alt

p_tern_helm <- ggplot() +
  geom_polygon(data = sp, aes(x = x1, y = x2), fill = NA, color = "black") +
  geom_text(data=sp, aes(x=x1, y=x2, label=ElectedParty),
            nudge_x=c(-0.06, 0.07, 0),
            nudge_y=c(-0.05, -0.05, 0.05)) +
  geom_point(data=res |> filter(CountNumber == 0), aes(x=x1, y=x2, color=ElectedParty)) +
  scale_y_reverse() +
  coord_fixed(ratio=1) +
  theme_void()

# Use the functions

pref_2025 |> 
  filter(CountNumber == 0) |>
  ggtern_cart2d(alternatives = c("ALP", "LNP", "Other")) +
  geom_point(aes(color = ElectedParty), size = 1, alpha = 0.8)

### Plot 4D ternary

# 5 dimensions: ALP, LNP, GRN, IND, Other
url_2025 <- "https://results.aec.gov.au/31496/Website/Downloads/HouseDopByDivisionDownload-31496.csv"
aec_2025_hd <- read_csv(url_2025, skip = 1) |>
  filter(CalculationType == "Preference Percent") |>
  mutate(Party = case_when(
    !(PartyAb %in% c("LP", "ALP", "NP", "LNP", "LNQ", "GRN", "IND")) ~ "Other",
    PartyAb %in% c("LP", "NP", "LNP", "LNQ") ~ "LNP",
    TRUE ~ PartyAb
  ))

pref_2025_hd <- aec_2025_hd |>
  left_join(
      aec_2025_hd |>
        filter(Elected == "Y") |>
        select(DivisionNm, ElectedParty = Party, CountNumber),
      by = c("DivisionNm", "CountNumber")) |>
  group_by(DivisionNm, CountNumber, Party, ElectedParty) |>
  summarise(
    Votes = sum(CalculationValue, na.rm = TRUE),
    .groups = "drop"
  ) |>
  ungroup() |>
  pivot_wider(
    names_from = Party,
    values_from = Votes,
    values_fill = 0
  ) |>
  mutate(
    across(ALP:IND, ~.x/100),
    Other = ifelse(1 - ALP - LNP - GRN - IND < 0, 0, 1 - ALP - LNP - GRN - IND)
  )

cart_df <- helmert_transform(pref_2025_hd, alternatives = c(4:8), append = TRUE) |>
  filter(CountNumber == 0)

simp <- geozoo::simplex(p = 4)
sp <- data.frame(simp$points)
colnames(sp) <- paste0("x", 1:length(sp))

labels <- c("ALP", "GRN", "LNP", "Other", "IND", rep("", nrow(cart_df)))
cart_df_simp <- bind_rows(sp, cart_df) |>
  mutate(labels = labels)
input_df <- cart_df_simp |> select(x1:x4)

animate_xy(
  input_df,
  # col = cart_df_simp$ElectedParty,
  edges = as.matrix(simp$edges),
  obs_labels  = cart_df_simp$labels,
  axes = "bottomleft"
)

render_gif(
  cart_df_simp |> select(x1:x4),
  grand_tour(),
  display_xy(
    col = cart_df_simp$ElectedParty,
    edges = as.matrix(simp$edges),
    obs_labels = cart_df_simp$labels,
    axes = "bottomleft"
  )
)