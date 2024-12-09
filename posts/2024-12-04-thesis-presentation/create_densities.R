library(ggplot2)

# Function to calculate quasi-Poisson density
dquasipoisson <- function(x, lambda, phi) {
  phi*dpois(x, lambda)
}

# Set up the data
x <- 0:15
lambda <- 2
phi <- 2

poisson_dist <- dpois(x, lambda = lambda)
quasipoisson_dist <- dquasipoisson(x, lambda = lambda, phi = phi)
negbin_dist <- dnbinom(x, mu = lambda, size = 1/phi)  # size = 1/phi for negative binomial

# Create a data frame
df <- data.frame(
  x = rep(x, 3),
  density = c(poisson_dist, quasipoisson_dist, negbin_dist),
  distribution = rep(c("Pois(λ=2)", "Quasi-Pois(λ=2, φ=2)", "NegBin(μ=2, φ=2)"), each = length(x))
)

# Create the plot
ggplot(df, aes(x = x, y = density, color = distribution)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = x) +
  labs(x = "Value",
       y = "Density",
       color = "Distribution") +
  theme_classic() +
  theme(legend.position = "bottom")


