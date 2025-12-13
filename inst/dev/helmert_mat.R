library(palmerpenguins)
library(geozoo)
library(randomForest)
library(tidyverse)
library(tourr)

source("inst/dev/poc1.R")

penguins <- penguins |> drop_na()
penguins_sub <- penguins[,1:5]

penguins_rf <- randomForest(species~.,
                             data=penguins_sub,
                             importance=TRUE)

# Project 4D into 3D
proj <- t(geozoo::f_helmert(3)[-1,])
p_rf_v_p <- as.matrix(penguins_rf$votes) %*% proj
colnames(p_rf_v_p) <- c("x1", "x2")
p_rf_v_p <- p_rf_v_p |>
  as.data.frame() |>
  mutate(species = penguins_sub$species)

simp <- simplex(p=2)
sp <- data.frame(cbind(simp$points), simp$points[c(2,3,1),])
colnames(sp) <- c("x1", "x2", "x3", "x4")
sp$species = sort(unique(penguins_sub$species))

ggplot() +
  geom_polygon(data = sp, aes(x = x1, y = x2), fill = NA, color = "black") +
  geom_text(data=sp, aes(x=x1, y=x2, label=species),
            nudge_x=c(-0.06, 0.07, 0),
            nudge_y=c(-0.05, -0.05, 0.05)) +
  geom_point(data=p_rf_v_p, aes(x=x1, y=x2, color=species)) +
  scale_y_reverse() +
  coord_fixed(ratio=1) +
  theme_void()

penguins_rf$votes |> colnames()

### Helmert transformation

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

### Plot

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

ggplotly(p_tern_helm, tooltip = "ElectedParty") 


ggtern_cart2d(pref_2025 |> filter(CountNumber == 0), alternatives = c("ALP", "LNP", "Other")) +
  geom_point(aes(color = ElectedParty), size = 1, alpha = 0.8) 

