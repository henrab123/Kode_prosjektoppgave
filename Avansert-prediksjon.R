# Last inn n??dvendige biblioteker
library(ggplot2)

# Definer funksjonen for log-likelihood
log_likelihood <- function(gamma, theta, s) {
  n <- length(gamma)  # Antall observasjoner
  eta <- gamma + s    # Beregn eta
  
  log_like <- numeric(n)
  for (i in 1:n) {
    a <- theta[i] - eta[i]
    b <- theta[i + 1] - eta[i]
    integral <- integrate(function(t) exp(-t^2 / 2), lower = a, upper = b)$value
    log_like[i] <- -0.5 * log(2 * pi) + log(integral)
  }
  
  return(sum(log_like))
}

# Derivert funksjon for f??rste ordens derivert
first_derivative <- function(gamma, theta, s) {
  n <- length(gamma)
  eta <- gamma + s
  
  first_deriv <- numeric(n)
  for (i in 1:n) {
    a <- theta[i] - eta[i]
    b <- theta[i + 1] - eta[i]
    # Beregn de to normal CDF differensene og deres derivater
    phi_a <- dnorm(a)
    phi_b <- dnorm(b)
    denom <- pnorm(b) - pnorm(a)
    
    # F??rste derivert
    first_deriv[i] <- (phi_b - phi_a) / denom
  }
  
  return(sum(first_deriv))
}

# Derivert funksjon for andre ordens derivert
second_derivative <- function(gamma, theta, s) {
  n <- length(gamma)
  eta <- gamma + s
  
  second_deriv <- numeric(n)
  for (i in 1:n) {
    a <- theta[i] - eta[i]
    b <- theta[i + 1] - eta[i]
    phi_a <- dnorm(a)
    phi_b <- dnorm(b)
    phi_a_prime <- -a * phi_a
    phi_b_prime <- -b * phi_b
    denom <- pnorm(b) - pnorm(a)
    
    # Andre derivert (kvotientenregel)
    second_deriv[i] <- (phi_b_prime - phi_a_prime) / denom -
      (phi_b - phi_a) * (dnorm(b) - dnorm(a)) / denom^2
  }
  
  return(sum(second_deriv))
}

# Definer parametere (theta og s)
theta <- c(-2, -1, 0, 1, 2)
s <- rep(0.5, length(theta))

# Array med gamma verdier (kan v??re et spekter av verdier)
gamma_values <- seq(-2, 2, length.out = 100)

# Beregn andre derivert for forskjellige gamma verdier
second_derivs <- sapply(gamma_values, function(gamma) second_derivative(gamma, theta, s))

# Lag en data frame for plotting
plot_data <- data.frame(gamma = gamma_values, second_derivative = second_derivs)

# Plot andre derivert som funksjon av gamma
ggplot(plot_data, aes(x = gamma, y = second_derivative)) +
  geom_line() +
  labs(title = "Andre Derivert av Log-Likelihood som funksjon av gamma",
       x = "Gamma",
       y = "Andre Derivert") +
  theme_minimal()
