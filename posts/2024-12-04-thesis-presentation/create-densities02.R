## PLOT 1: POISSON & QP AS DISCRETE CURVES
# Set up the plotting area for two plots side by side
par(mfrow = c(1, 2))

# Generate x values
x <- 0:10

# Poisson distribution (λ = 2)
poisson_probs <- dpois(x, lambda = 2)

# Plot Poisson distribution
plot(x, poisson_probs, type = "h", lwd = 2, 
     main = "Poisson(λ = 2)", 
     xlab = "x", ylab = "Probability", 
     ylim = c(0, max(poisson_probs)))
points(x, poisson_probs, pch = 16, cex = 1.5)

# Quasi-Poisson distribution (λ = 2, dispersion = 2)
quasi_poisson_var <- 2 * x  # Variance for quasi-Poisson
quasi_poisson_probs <- dnorm(x, mean = 2, sd = sqrt(quasi_poisson_var))

# Plot quasi-Poisson distribution
plot(x, quasi_poisson_probs, type = "h", lwd = 2,
     main = "Quasi-Poisson(λ = 2, φ = 2)", 
     xlab = "x", ylab = "Probability",
     ylim = c(0, max(quasi_poisson_probs)))
points(x, quasi_poisson_probs, pch = 16, cex = 1.5)

# Reset plotting parameters
par(mfrow = c(1, 1))


## PLOT 2: 3 PLOTS WITH LINES (NOT CONTINUOUS LOOKING)
# Set up the plotting area for three plots in a row
par(mfrow = c(1, 3))

# Generate x values (use more points for a smoother curve)
x <- seq(0, 10, by = 0.01)

# Poisson distribution (λ = 2)
poisson_probs <- dpois(x, lambda = 2)

# Plot Poisson distribution
plot(x, poisson_probs, type = "l", lwd = 2, 
     main = "Poisson(λ = 2)", 
     xlab = "x", ylab = "Probability Density", 
     ylim = c(0, max(poisson_probs)))

# Quasi-Poisson distribution (λ = 2, dispersion = 2)
quasi_poisson_var <- 2 * x  # Variance for quasi-Poisson
quasi_poisson_probs <- dnorm(x, mean = 2, sd = sqrt(quasi_poisson_var))

# Plot quasi-Poisson distribution
plot(x, quasi_poisson_probs, type = "l", lwd = 2,
     main = "Quasi-Poisson(λ = 2, φ = 2)", 
     xlab = "x", ylab = "Probability Density",
     ylim = c(0, max(quasi_poisson_probs)))

# Negative Binomial distribution (mu = 2, phi = 2)
# Convert mu and phi to size and prob parameters for dnbinom
size <- 2 / (2 - 1)  # size = mu / (phi - 1)
prob <- 1 / 2  # prob = 1 / phi

nb_probs <- dnbinom(x, size = size, prob = prob)

# Plot Negative Binomial distribution
plot(x, nb_probs, type = "l", lwd = 2,
     main = "Negative Binomial(μ = 2, φ = 2)", 
     xlab = "x", ylab = "Probability Density",
     ylim = c(0, max(nb_probs)))

# Reset plotting parameters
par(mfrow = c(1, 1))


## PLOT 3: NORMAL APPROXIMATIONS TO MAKE EACH LOOK CONTINUOUS
# Parameters
lambda <- 2
phi <- 2

# Generate x values
x <- seq(0, 10, by = 0.01)

# Poisson distribution (λ = 2) - Normal approximation
poisson_mean <- lambda
poisson_sd <- sqrt(lambda)
poisson_probs <- dnorm(x, mean = poisson_mean, sd = poisson_sd)

# Quasi-Poisson distribution (λ = 2, dispersion = 2)
quasi_poisson_mean <- lambda
quasi_poisson_sd <- sqrt(phi * lambda)
quasi_poisson_probs <- dnorm(x, mean = quasi_poisson_mean, sd = quasi_poisson_sd)

# Negative Binomial distribution (mu = 2, phi = 2) - Gamma approximation
nb_shape <- lambda / (phi - 1)
nb_scale <- phi - 1
nb_probs <- dgamma(x, shape = nb_shape, scale = nb_scale)

# Create the plot
plot(x, poisson_probs, type = "n", 
     main = "Comparison of Distributions (λ = 2, φ = 2)",
     xlab = "x", ylab = "Probability Density",
     ylim = c(0, max(poisson_probs, quasi_poisson_probs, nb_probs)))

# Add shaded areas with new colors
polygon(c(x, rev(x)), c(poisson_probs, rep(0, length(x))), col = adjustcolor("olivedrab", alpha.f = 0.3), border = NA)
polygon(c(x, rev(x)), c(quasi_poisson_probs, rep(0, length(x))), col = adjustcolor("blue", alpha.f = 0.3), border = NA)
polygon(c(x, rev(x)), c(nb_probs, rep(0, length(x))), col = adjustcolor("orange", alpha.f = 0.3), border = NA)

# Add lines on top of shaded areas with new colors
lines(x, poisson_probs, lwd = 2, col = "olivedrab")
lines(x, quasi_poisson_probs, lwd = 2, col = "blue")
lines(x, nb_probs, lwd = 2, col = "orange")

# Add a legend with new colors
legend("topright", 
       legend = c("Poisson", "Quasi-Poisson", "Negative Binomial"),
       col = c("olivedrab", "blue", "orange"), 
       lwd = 2, 
       fill = adjustcolor(c("olivedrab", "blue", "orange"), alpha.f = 0.3),
       border = NA,
       bty = "n")